{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.CVar where

-- CVar implements console variables. The original code is located in cvar.c

import Data.Bits ((.&.))
import Control.Lens ((^.))
import Control.Monad (liftM, void)
import Data.Traversable (traverse)
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as State
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV

import Quake (Quake)
import QuakeState
import QCommon.XCommandT
import qualified QCommon.Com as Com
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd

get :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
get = undefined -- TODO

init :: Quake ()
init = do
    Cmd.addCommand "set" setF
    Cmd.addCommand "cvarlist" listF

variableString :: B.ByteString -> Quake B.ByteString
variableString varName = do
    foundVar <- findVar varName

    case foundVar of
      Just v -> return $ v^.cvString
      Nothing -> return ""

findVar :: B.ByteString -> Quake (Maybe CVarT)
findVar = undefined -- TODO

fullSet :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
fullSet = undefined -- TODO

set :: B.ByteString -> B.ByteString -> Quake CVarT
set = undefined -- TODO

forceSet :: B.ByteString -> B.ByteString -> Quake CVarT
forceSet = undefined -- TODO

set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 = undefined -- TODO

-- Set command, sets variables
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
    vars <- liftM (^.globals.cvarVars) State.get

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
