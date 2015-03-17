{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.CVar where

-- CVar implements console variables. The original code is located in cvar.c

import Data.Maybe (isJust, fromJust)
import Data.Bits ((.&.), (.|.))
import Control.Lens ((^.), (%=), (.=), use)
import Control.Monad (void, when)
import Data.Foldable (find)
import Data.Traversable (traverse)
import Data.Sequence ((<|))
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Game.CVarT as CVarT
import qualified QCommon.Com as Com
import qualified Constants
import qualified Util.Lib as Lib
import {-# SOURCE #-} qualified Game.Cmd as Cmd

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
get varName varValue flags = do
    let isUserOrServerInfo = flags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)

    if isUserOrServerInfo /= 0 && not (infoValidate varName)
      then do
        Com.printf "invalid info cvar name\n"
        return Nothing
      else do
        var <- findVar varName

        if | isJust var -> do
               let cvar = fromJust var
                   updatedCVar = cvar { _cvFlags = (cvar^.cvFlags) .|. flags }
               update updatedCVar
               return $ Just updatedCVar
           | isUserOrServerInfo /= 0 && not (infoValidate varValue) -> do
               Com.printf "invalid info cvar value\n"
               return Nothing
           | otherwise -> do
               let newCVar = CVarT.newCVarT { _cvName     = varName
                                            , _cvString   = varValue
                                            , _cvModified = True
                                            , _cvValue    = Lib.atof varValue
                                            , _cvFlags    = flags
                                            }

               globals.cvarVars %= (newCVar <|)

               return $ Just newCVar

getAndSet :: B.ByteString -> B.ByteString -> Int -> CVarTLens -> Quake CVarT
getAndSet varName varValue flags quakeStateLens = do
    Just cVar <- get varName varValue flags
    quakeStateLens .= cVar
    return cVar

update :: CVarT -> Quake ()
update cvar = globals.cvarVars %= fmap (\v -> if v^.cvName == cvar^.cvName then cvar else v)

init :: Quake ()
init = do
    Cmd.addCommand "set" (Just setF)
    Cmd.addCommand "cvarlist" (Just listF)

variableString :: B.ByteString -> Quake B.ByteString
variableString varName = do
    foundVar <- findVar varName

    case foundVar of
      Just v -> return $ v^.cvString
      Nothing -> return ""

findVar :: B.ByteString -> Quake (Maybe CVarT)
findVar varName = do
    vars <- use $ globals.cvarVars
    return $ find (\v -> v^.cvName == varName) vars

-- Creates a variable if not found and sets their value, the parsed float value and their flags.
fullSet :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
fullSet varName value flags = do
    var <- findVar varName

    case var of
      Nothing -> do -- create it
        Just newCVar <- get varName value flags
        return newCVar

      Just cvar -> do
        when (((cvar^.cvFlags) .&. Constants.cvarUserInfo) /= 0) $
          globals.userInfoModified .= True

        let updatedCVar = cvar { _cvModified = True
                               , _cvString = value
                               , _cvValue = Lib.atof value
                               , _cvFlags = flags
                               }

        update updatedCVar

        return updatedCVar

-- Sets the value of the variable without forcing.
set :: B.ByteString -> B.ByteString -> Quake CVarT
set varName value = set2 varName value False

-- Sets the value of the variable with forcing.
forceSet :: B.ByteString -> B.ByteString -> Quake CVarT
forceSet varName value = set2 varName value True

-- Gereric set function, sets the value of the variable, with forcing its even possible to 
-- override the variables write protection. 
set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 varName value force = undefined -- TODO

-- Set command, sets variables.
setF :: XCommandT
setF = do
    c <- Cmd.argc

    if | (c /= 3) && (c /= 4) ->
           Com.printf "usage: set <variable> <value> [u / s]\n"
       | c == 4 -> do
           v1 <- Cmd.argv 1
           v2 <- Cmd.argv 2
           v3 <- Cmd.argv 3
           if | v3 == "u" -> void $ fullSet v1 v2 Constants.cvarUserInfo
              | v3 == "s" -> void $ fullSet v1 v2 Constants.cvarServerInfo
              | otherwise -> Com.printf "flags can only be 'u' or 's'\n"
       | otherwise -> do
           v1 <- Cmd.argv 1
           v2 <- Cmd.argv 2
           void $ set v1 v2

-- List command, lists all available commands
listF :: XCommandT
listF = do
    vars <- use $ globals.cvarVars

    _ <- traverse printCVar vars

    Com.printf $ BC.pack (show (Seq.length vars)) `B.append` " cvars\n" -- TODO: maybe use binary package for Int to ByteString conversion?

  where printCVar var = do
          Com.printf $ if (var^.cvFlags .&. Constants.cvarArchive) /= 0 then "*" else " "
          Com.printf $ if (var^.cvFlags .&. Constants.cvarUserInfo) /= 0 then "U" else " "
          Com.printf $ if (var^.cvFlags .&. Constants.cvarServerInfo) /= 0 then "S" else " "

          Com.printf $ if | (var^.cvFlags .&. Constants.cvarNoSet) /= 0 -> "-"
                          | (var^.cvFlags .&. Constants.cvarLatch) /= 0 -> "L"
                          | otherwise -> " "

          Com.printf $ " " `B.append` (var^.cvName)
                           `B.append` " \""
                           `B.append` (var^.cvString)
                           `B.append` "\"\n"

setValueI :: B.ByteString -> Int -> Quake ()
setValueI = undefined -- TODO

setValueF :: B.ByteString -> Float -> Quake ()
setValueF = undefined -- TODO

variableValue :: B.ByteString -> Quake Float
variableValue = undefined -- TODO

command :: Quake Bool
command = undefined -- TODO

bitInfo :: Int -> Quake B.ByteString
bitInfo = undefined -- TODO

serverInfo :: Quake B.ByteString
serverInfo = undefined -- TODO

getLatchedVars :: Quake ()
getLatchedVars = undefined -- TODO

userInfo :: Quake B.ByteString
userInfo = undefined -- TODO

writeVariables :: FilePath -> Quake ()
writeVariables = undefined -- TODO

completeVariable :: B.ByteString -> UV.Vector B.ByteString
completeVariable = undefined -- TODO

infoValidate :: B.ByteString -> Bool
infoValidate = undefined -- TODO
