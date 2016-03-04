module QCommon.CVarShared
  ( checkUserInfoModified
  , command
  , createFailedError
  , findVar
  , forceSet
  , get
  , infoValidate
  , set
  , update
  , variableString
  ) where

import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.Shared as FS
import           QuakeState
import           Types
import qualified Util.Lib as Lib

import           Control.Lens (use, (^.), (.=), (%=), (&), (%~), (&), (.~))
import           Control.Monad (when)
import           Data.Bits ((.|.), (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe (isNothing, isJust)

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
get name value flags =
  do existingVar <- findVar name
     maybe tryCreatingNewCVar updateExistingVar existingVar
  where tryCreatingNewCVar
          | isUserOrServerInfo && not isValidName = invalidCVar "invalid info cvar name\n"
          | isUserOrServerInfo && not isValidValue = invalidCVar "invalid info cvar value\n"
          | otherwise = createNewCVar
        updateExistingVar var =
          do let updatedVar = var & cvFlags %~ (.|. flags)
             update updatedVar
             return (Just updatedVar)
        isUserOrServerInfo = (flags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)) /= 0
        isValidName = infoValidate name
        isValidValue = infoValidate value
        invalidCVar msg =
          do Com.printf msg
             return Nothing
        createNewCVar =
          do let newCVar = CVarT name value Nothing flags True (Lib.atof value)
             globals.gCVars %= HM.insert name newCVar
             return (Just newCVar)

-- Some characters are invalid for info strings.
infoValidate :: B.ByteString -> Bool
infoValidate s = not ('\\' `BC.elem` s || '"' `BC.elem` s || ';' `BC.elem` s)

update :: CVarT -> Quake ()
update cvar = globals.gCVars %= HM.insert (cvar^.cvName) cvar

findVar :: B.ByteString -> Quake (Maybe CVarT)
findVar name =
  do vars <- use (globals.gCVars)
     return (HM.lookup name vars)

variableString :: B.ByteString -> Quake B.ByteString
variableString varName =
  do foundVar <- findVar varName
     return (maybe "" (^.cvString) foundVar)

command :: Quake Bool
command = error "CVar.command" -- TODO

set :: B.ByteString -> B.ByteString -> Quake CVarT
set name value = set2 name value False

set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 name value force =
  do var <- findVar name
     maybe createNewCVar (proceedSet2 name value force) var
  where createNewCVar =
          do var <- get name value 0
             maybe createFailedError return var

proceedSet2 :: B.ByteString -> B.ByteString -> Bool -> CVarT -> Quake CVarT
proceedSet2 name value force var
  | hasUserServerFlag && invalidValue =
      do Com.printf "invalid info cvar value\n"
         return var
  | not force && writeProtected =
      do Com.printf (name `B.append` " is write protected.\n")
         return var
  | not force && hasLatchFlag = updateLatchedVar var name value
  | otherwise = updateVar var value force
  where hasUserServerFlag = userServerFlag /= 0
        writeProtected = noSetFlag /= 0
        hasLatchFlag = latchFlag /= 0
        latchFlag = flags .&. Constants.cvarLatch
        noSetFlag = flags .&. Constants.cvarNoSet
        userServerFlag = flags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)
        flags = var^.cvFlags
        invalidValue = not (infoValidate value)

updateLatchedVar :: CVarT -> B.ByteString -> B.ByteString -> Quake CVarT
updateLatchedVar var name value =
  do state <- use (globals.gServerState)
     proceedUpdate state
  where proceedUpdate state
          | Just value == latchedString = return var
          | isNothing latchedString && value == (var^.cvString) = return var
          | state /= 0 = delayedVarUpdate var name value
          | otherwise = normalVarUpdate var name value
        latchedString = var^.cvLatchedString

delayedVarUpdate :: CVarT -> B.ByteString -> B.ByteString -> Quake CVarT
delayedVarUpdate var name value =
  do Com.printf (name `B.append` " will be changed for next game.\n")
     update updatedCVar
     return updatedCVar
  where updatedCVar = var & cvLatchedString .~ Just value

normalVarUpdate :: CVarT -> B.ByteString -> B.ByteString -> Quake CVarT
normalVarUpdate var name value =
  do update updatedCVar
     handleGameCVar
     return updatedCVar
  where updatedCVar = var & cvLatchedString .~ Nothing
                          & cvString .~ value
                          & cvValue .~ Lib.atof value
        handleGameCVar =
          when (name == "game") $
            do FS.setGameDir value
               FS.execAutoexec

updateVar :: CVarT -> B.ByteString -> Bool -> Quake CVarT
updateVar var value force
  | value == updatedCVar^.cvString = return updatedCVar
  | otherwise =
      do checkUserInfoModified updatedCVar
         update finalCVar
         return finalCVar
  where updatedCVar
          | force && isJust (var^.cvLatchedString) = var & cvLatchedString .~ Nothing
          | otherwise = var
        finalCVar = updatedCVar & cvModified .~ True
                                & cvString .~ value
                                & cvValue .~ Lib.atof value

createFailedError :: Quake CVarT
createFailedError =
  do Com.fatalError "Failed to create cvar"
     return (CVarT "" "" Nothing 0 False 0)
     
checkUserInfoModified :: CVarT -> Quake () -- make compiler happy
checkUserInfoModified var =
  when ((var^.cvFlags) .&. Constants.cvarUserInfo /= 0) $
    globals.gUserInfoModified .= True

forceSet :: B.ByteString -> B.ByteString -> Quake CVarT
forceSet name value = set2 name value True