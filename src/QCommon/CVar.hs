module QCommon.CVar 
  (get
  ,getExisting
  ,initialize
  ,set
  ,update
  ,variableString)
  where

import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import           QCommon.CVarShared
import           QuakeState
import           Types
import qualified Util.Lib as Lib

import           Control.Lens ((%=), (&), (%~))
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe (fromMaybe)

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
get name value flags =
  do existingVar <- findVar name
     case existingVar of
       Nothing ->
         if | isUserOrServerInfo && not isValidName -> invalidCVar "invalid info cvar name\n"
            | isUserOrServerInfo && not isValidValue -> invalidCVar "invalid info cvar value\n"
            | otherwise -> createNewCVar
       Just var -> do
         let updatedVar = var & cvFlags %~ (.|. flags)
         update updatedVar
         return (Just updatedVar)
  where isUserOrServerInfo = (flags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)) /= 0
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

getExisting :: B.ByteString -> Quake CVarT
getExisting = fmap (fromMaybe (error "CVar.getExisting returned Nothing")) . findVar

initialize :: Quake ()
initialize =
  do Cmd.addCommand "set" (Just setF)
     Cmd.addCommand "cvarlist" (Just listF)

update :: CVarT -> Quake ()
update = error "CVar.update" -- TODO

set :: B.ByteString -> B.ByteString -> Quake CVarT
set name value = set2 name value False

set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 = error "CVar.set2" -- TODO

setF :: XCommandT
setF = error "CVar.setF" -- TODO

listF :: XCommandT
listF = error "CVar.listF" -- TODO