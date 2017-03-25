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

import           Control.Lens    (use, ix, (.=))
import           Control.Monad   (unless)
import qualified Data.ByteString as B
import           Linear          (V3)

import qualified Constants
import qualified Game.GameBase   as GameBase
import {-# SOURCE #-} Game.GameImportT
import qualified Game.GameSave   as GameSave
import qualified QCommon.Com     as Com
import qualified QCommon.MSG     as MSG
import qualified QCommon.SZ      as SZ
import           QuakeState
import           Server.ServerT
import qualified Server.SVSend   as SVSend
import           Types
import           Util.Binary     (encode)

dprintf :: B.ByteString -> Quake ()
dprintf = Com.printf

cprintf :: Maybe (Ref EdictT) -> Int -> B.ByteString -> Quake ()
cprintf = error "SVGame.cprintf" -- TODO

centerPrintf :: Ref EdictT -> B.ByteString -> Quake ()
centerPrintf = error "SVGame.centerPrintf" -- TODO

startSound :: Maybe (Ref EdictT) -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound = error "SVGame.startSound" -- TODO

configString :: Int -> B.ByteString -> Quake ()
configString index val
    | index < 0 || index >= Constants.maxConfigStrings =
        Com.comError Constants.errDrop (B.concat ["configstring: bad index ", encode index, "\n"])
    | otherwise = do
        svGlobals.svServer.sConfigStrings.ix index .= val
        state <- use (svGlobals.svServer.sState)
        unless (state == Constants.ssLoading) $ do
            SZ.clear (svGlobals.svServer.sMulticast)
            MSG.writeCharI (svGlobals.svServer.sMulticast) (fromIntegral Constants.svcConfigString)
            MSG.writeShort (svGlobals.svServer.sMulticast) (fromIntegral index)
            MSG.writeString (svGlobals.svServer.sMulticast) val
            origin <- use (globals.gVec3Origin)
            SVSend.multicast origin Constants.multicastAllR

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
initGameProgs = do
    shutdownGameProgs
    GameBase.getGameApi newGameImportT
    GameSave.initGame

shutdownGameProgs :: Quake ()
shutdownGameProgs = GameBase.shutdownGame