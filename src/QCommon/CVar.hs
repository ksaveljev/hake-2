{-# LANGUAGE MultiWayIf #-}
module QCommon.CVar 
  (get
  ,getExisting
  ,initialize
  ,update)
  where

import qualified Constants
import qualified QCommon.Com as Com
import           QuakeState
import           Types
import qualified Util.Lib as Lib

import           Control.Lens ((%=), (&), (%~), use)
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

findVar :: B.ByteString -> Quake (Maybe CVarT)
findVar name =
  do vars <- use (globals.gCVars)
     return (HM.lookup name vars)

initialize :: Quake ()
initialize = error "CVar.initialize" -- TODO

update :: CVarT -> Quake ()
update = error "CVar.update" -- TODO