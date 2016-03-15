module Server.SVGame
  ( centerPrintf
  , configString
  , cprintf
  , dprintf
  , initGameProgs
  , inPHS
  , pfError2
  , setModel
  , startSound
  , unicast
  , writeByte
  , writeDir
  , writePos
  , writeShort
  , writeString
  ) where

import qualified Game.GameBase as GameBase
import {-# SOURCE #-} Game.GameImportT
import qualified Game.GameSave as GameSave
import qualified QCommon.Com as Com
import           Types

import qualified Data.ByteString as B
import           Linear (V3)

dprintf :: B.ByteString -> Quake ()
dprintf = Com.printf

cprintf :: Maybe (Ref EdictT) -> Int -> B.ByteString -> Quake ()
cprintf = error "SVGame.cprintf" -- TODO

centerPrintf :: Ref EdictT -> B.ByteString -> Quake ()
centerPrintf = error "SVGame.centerPrintf" -- TODO

startSound :: Maybe (Ref EdictT) -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound = error "SVGame.startSound" -- TODO

configString :: Int -> B.ByteString -> Quake ()
configString = error "SVGame.configString" -- TODO

pfError2 :: Int -> B.ByteString -> Quake ()
pfError2 = error "SVGame.pfError2" -- TODO

setModel :: Ref EdictT -> Maybe B.ByteString -> Quake ()
setModel = error "SVGame.setModel" -- TODO

inPHS :: V3 Float -> V3 Float -> Quake Bool
inPHS = error "SVGame.inPHS" -- TODO

unicast :: Ref EdictT -> Bool -> Quake ()
unicast = error "SVGame.unicast" -- TODO

writeByte :: Int -> Quake ()
writeByte = error "SVGame.writeByte" -- TODO

writeShort :: Int -> Quake ()
writeShort = error "SVGame.writeShort" -- TODO

writeString :: B.ByteString -> Quake ()
writeString = error "SVGame.writeString" -- TODO

writePos :: V3 Float -> Quake ()
writePos = error "SVGame.writePos" -- TODO

writeDir :: V3 Float -> Quake ()
writeDir = error "SVGame.writeDir" -- TODO

initGameProgs :: Quake ()
initGameProgs =
  do shutdownGameProgs
     GameBase.getGameApi newGameImportT
     GameSave.initGame

shutdownGameProgs :: Quake ()
shutdownGameProgs = GameBase.shutdownGame