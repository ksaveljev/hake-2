module QCommon.CVar 
  ( findVar
  , forceSet
  , fullSet
  , get
  , getExisting
  , getLatchedVars
  , initialize
  , initializeCVars
  , serverInfo
  , set
  , setValueF
  , setValueI
  , update
  , variableString
  , variableValue
  ) where

import qualified Constants
import qualified Game.Cmd as Cmd
import qualified Game.Info as Info
import           Game.CVarT
import qualified QCommon.Com as Com
import           QCommon.CVarShared
import {-# SOURCE #-} qualified QCommon.Shared as FS
import           QuakeState
import           Types
import qualified Util.Lib as Lib
import           Util.Binary (encode)

import           Control.Lens (use, (^.), (.=), (&), (.~))
import           Control.Monad (void, when, foldM)
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe (fromMaybe)

getExisting :: B.ByteString -> Quake CVarT
getExisting = fmap (fromMaybe (error "CVar.getExisting returned Nothing")) . findVar

initialize :: Quake ()
initialize =
  do Cmd.addCommand "set" (Just setF)
     Cmd.addCommand "cvarlist" (Just listF)

setF :: XCommandT
setF = XCommandT "CVar.setF" $
  checkParamsAndSet =<< Cmd.argc
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
  Com.printf (B.concat [archive, user, server, varFlag, varInfo])
  where archive = info "*"
        user = info "U"
        server = info "S"
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

initializeCVars :: [(B.ByteString, B.ByteString, Int)] -> Quake ()
initializeCVars = mapM_ (\(name, val, flags) -> get name val flags)

serverInfo :: Quake B.ByteString
serverInfo = bitInfo Constants.cvarServerInfo

bitInfo :: Int -> Quake B.ByteString
bitInfo bit =
  do vars <- fmap (filter (\v -> (v^.cvFlags) .&. bit /= 0) . HM.elems) (use (globals.gCVars))
     collectInfo B.empty vars
  where collectInfo = foldM (\info var -> Info.setValueForKey info (var^.cvName) (var^.cvString))

setValueF :: B.ByteString -> Float -> Quake ()
setValueF name value = void (set name val)
  where val | value == fromIntegral tv = encode tv
            | otherwise = encode value
        tv = truncate value :: Int

setValueI :: B.ByteString -> Int -> Quake ()
setValueI name value = void (set name (encode value))

variableValue :: B.ByteString -> Quake Float
variableValue name =
  do var <- findVar name
     return (maybe 0 (Lib.atof . (^.cvString)) var)

getLatchedVars :: Quake ()
getLatchedVars = updateLatchedVars =<< use (globals.gCVars)

updateLatchedVars :: HM.HashMap B.ByteString CVarT -> Quake ()
updateLatchedVars cVars =
  do globals.gCVars .= updatedCVars
     when (gameVarBefore /= gameVarAfter) $
       maybe (return ()) autoexec gameVarAfter
  where updatedCVars = HM.map updateLatchedVar cVars
        gameVarBefore = HM.lookup "game" cVars
        gameVarAfter = HM.lookup "game" updatedCVars
        autoexec game =
          do FS.setGameDir (game^.cvString)
             FS.execAutoexec

updateLatchedVar :: CVarT -> CVarT
updateLatchedVar var = maybe var updateVar (var^.cvLatchedString)
  where updateVar latchedString = var & cvString .~ latchedString
                                      & cvLatchedString .~ Nothing
                                      & cvValue .~ Lib.atof latchedString