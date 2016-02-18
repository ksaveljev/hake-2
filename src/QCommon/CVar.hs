module QCommon.CVar 
  ( get
  , getExisting
  , initialize
  , initializeCVars
  , set
  , update
  , variableString
  ) where

import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import           QCommon.CVarShared
import qualified QCommon.FS as FS
import           QuakeState
import           Types
import qualified Util.Lib as Lib

import           Control.Lens (use, (.=), (^.), (&), (.~))
import           Control.Monad (when, void)
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe (isJust, isNothing, fromMaybe)
import           Data.Serialize (encode)

getExisting :: B.ByteString -> Quake CVarT
getExisting = fmap (fromMaybe (error "CVar.getExisting returned Nothing")) . findVar

initialize :: Quake ()
initialize =
  do Cmd.addCommand "set" (Just setF)
     Cmd.addCommand "cvarlist" (Just listF)

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
     if | Just value == latchedString -> return var
        | isNothing latchedString && value == (var^.cvString) -> return var
        | state /= 0 -> delayedVarUpdate var name value
        | otherwise -> normalVarUpdate var name value
  where latchedString = var^.cvLatchedString

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

setF :: XCommandT
setF = XCommandT "CVar.setF" $
  do c <- Cmd.argc
     checkParamsAndSet c
  where checkParamsAndSet c
          | c /= 3 && c /= 4 = showUsageMessage
          | c == 4 = handleFullSet
          | otherwise = handleSet
        showUsageMessage = Com.printf "usage: set <variable> <value> [u / s]\n"
        handleFullSet =
          do [v1, v2, v3] <- mapM Cmd.argv [1, 2, 3] -- IMPROVE
             checkParamsAndFullSet v1 v2 v3
        handleSet =
          do [v1, v2] <- mapM Cmd.argv [1, 2] -- IMPROVE
             void (set v1 v2)
        checkParamsAndFullSet v1 v2 "u" = void (fullSet v1 v2 Constants.cvarUserInfo)
        checkParamsAndFullSet v1 v2 "s" = void (fullSet v1 v2 Constants.cvarServerInfo)
        checkParamsAndFullSet _ _ _ = Com.printf "flags can only be 'u' or 's'\n"

listF :: XCommandT
listF = XCommandT "CVar.listF" $
  do vars <- use (globals.gCVars)
     mapM_ printCVar vars
     Com.printf (encode (HM.size vars) `B.append` " cvars\n")

printCVar :: CVarT -> Quake ()
printCVar v =
  Com.printf (B.concat [archive, userInfo, serverInfo, varFlag, varInfo])
  where archive = info "*"
        userInfo = info "U"
        serverInfo = info "S"
        info str | (v^.cvFlags) .&. Constants.cvarServerInfo /= 0 = str
                 | otherwise = " "
        varFlag | (v^.cvFlags) .&. Constants.cvarNoSet /= 0 = "-"
                | (v^.cvFlags) .&. Constants.cvarLatch /= 0 = "L"
                | otherwise = " "
        varInfo = B.concat [" ", v^.cvName, " \"", v^.cvString, "\"\n"]

fullSet :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
fullSet name value flags =
  do var <- findVar name
     maybe varNotFound updateFoundVar var
  where varNotFound =
          do newVar <- get name value flags
             maybe createFailedError return newVar
        updateFoundVar var =
          do checkUserInfoModified var
             let updatedCVar = updateFields var
             update updatedCVar
             return updatedCVar
        updateFields var = var & cvModified .~ True
                               & cvString .~ value
                               & cvValue .~ Lib.atof value
                               & cvFlags .~ flags

checkUserInfoModified :: CVarT -> Quake () -- make compiler happy
checkUserInfoModified var =
  when ((var^.cvFlags) .&. Constants.cvarUserInfo /= 0) $
    globals.gUserInfoModified .= True

createFailedError :: Quake CVarT
createFailedError =
  do Com.fatalError "Failed to create cvar"
     return (CVarT "" "" Nothing 0 False 0)
     
initializeCVars :: [(B.ByteString, B.ByteString, Int)] -> Quake ()
initializeCVars = mapM_ (\(name, val, flags) -> get name val flags)