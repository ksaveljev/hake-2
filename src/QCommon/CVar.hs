module QCommon.CVar 
  ( forceSet
  , get
  , getExisting
  , initialize
  , initializeCVars
  , serverInfo
  , set
  , update
  , variableString
  ) where

import qualified Constants
import qualified Game.Cmd as Cmd
import qualified Game.Info as Info
import qualified QCommon.Com as Com
import           QCommon.CVarShared
import           QuakeState
import           Types
import qualified Util.Lib as Lib
import           Util.Binary (encode)

import           Control.Lens (use, (^.), (&), (.~))
import           Control.Monad (void)
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
     collectInfo vars B.empty
  where collectInfo [] info = return info
        collectInfo (var:vars) info =
          do info' <- Info.setValueForKey info (var^.cvName) (var^.cvString)
             collectInfo vars info'
{-
- cvars <- liftM (filter (\e -> (e^.cvFlags) .&. bit /= 0) . Map.elems) (use $ globals.cvarVars)
    collectInfo cvars ""

  where collectInfo :: [CVarT] -> B.ByteString -> Quake B.ByteString
        collectInfo [] info = return info
        collectInfo (x:xs) info = do
          Info.setValueForKey info (x^.cvName) (x^.cvString) >>= collectInfo xs-}
