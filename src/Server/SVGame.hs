module Server.SVGame
    ( centerPrintf
    , configString
    , cprintf
    , dprintf
    , initGameProgs
    , inPHS
    , pfError2
    , setModel
    , shutdownGameProgs
    , startSound
    , unicast
    , writeByte
    , writeDir
    , writePos
    , writeShort
    , writeString
    ) where

import           Control.Lens          (use, ix, (^.), (.=), (&), (.~))
import           Control.Monad         (unless, when)
import           Data.Bits             (shiftR, shiftL, (.&.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Linear                (V3)

import qualified Constants
import           Game.CModelT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase         as GameBase
import qualified Game.GameSave         as GameSave
import qualified QCommon.CM            as CM
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import qualified QCommon.MSG           as MSG
import           QCommon.NetChanT
import           QCommon.SizeBufT
import qualified QCommon.SZ            as SZ
import           QuakeRef
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVSend         as SVSend
import qualified Server.SVWorld        as SVWorld
import           Types
import           Util.Binary           (encode)

import {-# SOURCE #-} qualified Server.SVInit         as SVInit

dprintf :: B.ByteString -> Quake ()
dprintf = Com.printf

cprintf :: Maybe (Ref EdictT) -> Int -> B.ByteString -> Quake ()
cprintf Nothing _ str = Com.printf str
cprintf (Just edictRef) level str = do
    edict <- readRef edictRef
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    when ((edict^.eIndex) < 1 || (edict^.eIndex) > maxClients) $
        Com.comError Constants.errDrop "cprintf to a non-client"
    client <- readRef (Ref ((edict^.eIndex) - 1))
    SVSend.clientPrintf client level str

centerPrintf :: Ref EdictT -> B.ByteString -> Quake ()
centerPrintf edictRef str = do
    edict <- readRef edictRef
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    unless ((edict^.eIndex) < 1 || (edict^.eIndex) > maxClients) $ do
        MSG.writeByteI (svGlobals.svServer.sMulticast) (fromIntegral Constants.svcCenterPrint)
        MSG.writeString (svGlobals.svServer.sMulticast) str
        unicast edictRef True

startSound :: Maybe (Ref EdictT) -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound Nothing _ _ _ _ _ = return ()
startSound (Just edictRef) channel soundNum volume attenuation timeOfs =
    SVSend.startSound Nothing edictRef channel soundNum volume attenuation timeOfs

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
pfError2 = Com.comError

setModel :: Ref EdictT -> Maybe B.ByteString -> Quake ()
setModel _ Nothing = Com.comError Constants.errDrop "PF_setmodel: NULL"
setModel edictRef name@(Just modelName) = do
    idx <- SVInit.modelIndex name
    modifyRef edictRef (\v -> v & eEntityState.esModelIndex .~ idx)
    -- if it is an inline model, get the size information for it
    when (BC.head modelName == '*') $ do
        cModelRef <- CM.inlineModel modelName
        model <- readRef cModelRef
        modifyRef edictRef (\v -> v & eMins .~ (model^.cmMins)
                                    & eMaxs .~ (model^.cmMaxs))
        SVWorld.linkEdict edictRef

inPHS :: V3 Float -> V3 Float -> Quake Bool
inPHS p1 p2 = do
    leafNum <- CM.pointLeafNum p1
    cluster <- CM.leafCluster leafNum
    area1 <- CM.leafArea leafNum
    mask <- CM.clusterPHS cluster
    leafNum' <- CM.pointLeafNum p2
    cluster' <- CM.leafCluster leafNum'
    area2 <- CM.leafArea leafNum'
    case () of
        _ | cluster' == -1 -> return False
          | (mask `B.index` (cluster' `shiftR` 3)) .&. (1 `shiftL` (cluster' .&. 7)) == 0 -> return False -- more than one bounce away
          | otherwise -> CM.areasConnected area1 area2

unicast :: Ref EdictT -> Bool -> Quake ()
unicast edictRef reliable = do
    edict <- readRef edictRef
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    unless ((edict^.eIndex) < 1 || (edict^.eIndex) > maxClients) $ do
        buf <- use (svGlobals.svServer.sMulticast.sbData)
        if reliable
            then SZ.write (svGlobals.svServerStatic.ssClients.ix ((edict^.eIndex) - 1).cNetChan.ncMessage) buf (B.length buf)
            else SZ.write (svGlobals.svServerStatic.ssClients.ix ((edict^.eIndex) - 1).cDatagram) buf (B.length buf)
        SZ.clear (svGlobals.svServer.sMulticast)

writeByte :: Int -> Quake ()
writeByte = MSG.writeByteI (svGlobals.svServer.sMulticast)

writeShort :: Int -> Quake ()
writeShort = MSG.writeShort (svGlobals.svServer.sMulticast)

writeString :: B.ByteString -> Quake ()
writeString = MSG.writeString (svGlobals.svServer.sMulticast)

writePos :: V3 Float -> Quake ()
writePos = MSG.writePos (svGlobals.svServer.sMulticast)

writeDir :: V3 Float -> Quake ()
writeDir = MSG.writeDir (svGlobals.svServer.sMulticast)

initGameProgs :: Quake ()
initGameProgs = do
    shutdownGameProgs
    GameBase.getGameApi newGameImportT
    GameSave.initGame

shutdownGameProgs :: Quake ()
shutdownGameProgs = GameBase.shutdownGame
