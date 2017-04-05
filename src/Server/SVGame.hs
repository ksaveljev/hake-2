{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SVGame where

import Control.Lens (use, (.=), ix, zoom, preuse, (^.), (&), (.~))
import Control.Monad (when, unless, liftM)
import Data.Bits ((.&.), shiftL, shiftR)
import Data.Maybe (isNothing, fromJust)
import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import {-# SOURCE #-} Game.GameImportT
import Game.LevelLocalsT
import Game.GameLocalsT
import Game.CVarT
import Game.SpawnTempT
import Game.EntityStateT
import QCommon.SizeBufT
import Game.CModelT
import Game.EdictT
import Server.ServerT
import Server.ServerStaticT
import QCommon.NetChanT
import Server.ClientT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified Game.GameSave as GameSave
import qualified QCommon.CM as CM
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.MSG as MSG
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified Server.SVInit as SVInit
import {-# SOURCE #-} qualified Server.SVSend as SVSend
import qualified Server.SVWorld as SVWorld

{-
- PF_Unicast
- 
- Sends the contents of the mutlicast buffer to a single client.
-}
unicast :: Ref EdictT -> Bool -> Quake ()
unicast edictRef reliable = do
    edict <- readRef edictRef
    let p = edict^.eIndex
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    unless (p < 1 || p > maxClientsValue) $ do
      buf <- use $ svGlobals.svServer.sMulticast.sbData

      if reliable
        then SZ.write (svGlobals.svServerStatic.ssClients.ix (p - 1).cNetChan.ncMessage) buf (B.length buf)
        else SZ.write (svGlobals.svServerStatic.ssClients.ix (p - 1).cDatagram) buf (B.length buf)

      SZ.clear (svGlobals.svServer.sMulticast)

{-
- PF_dprintf
- 
- Debug print to server console.
-}
dprintf :: B.ByteString -> Quake ()
dprintf = Com.printf

-- Centerprintf for critical messages.
cprintfHigh :: Ref EdictT -> B.ByteString -> Quake ()
cprintfHigh edictRef str = cprintf (Just edictRef) Constants.printHigh str

{-
- PF_cprintf
- 
- Print to a single client.
-}
cprintf :: Maybe (Ref EdictT) -> Int -> B.ByteString -> Quake ()
cprintf maybeEdict level str = do
    case maybeEdict of
      Nothing ->
        Com.printf str

      Just edictRef -> do
        edict <- readRef edictRef

        maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

        when ((edict^.eIndex) < 1 || (edict^.eIndex) > maxClientsValue) $
          Com.comError Constants.errDrop "cprintf to a non-client"

        Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix ((edict^.eIndex) - 1)

        SVSend.clientPrintf client level str

{-
- PF_centerprintf
- 
- centerprint to a single client.
-}
centerPrintf :: Ref EdictT -> B.ByteString -> Quake ()
centerPrintf edictRef str = do
    edict <- readRef edictRef
    let n = edict^.eIndex
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    unless (n < 1 || n > maxClientsValue) $ do
      MSG.writeByteI (svGlobals.svServer.sMulticast) (fromIntegral Constants.svcCenterPrint)
      MSG.writeString (svGlobals.svServer.sMulticast) str
      unicast edictRef True

{-
-  PF_error
- 
-  Abort the server with a game error. 
-}
pfError :: B.ByteString -> Quake ()
pfError str = Com.comError Constants.errDrop ("Game Error: " `B.append` str)

pfError2 :: Int -> B.ByteString -> Quake ()
pfError2 level str = Com.comError level str

{-
- PF_setmodel
- 
- Also sets mins and maxs for inline bmodels.
-}
setModel :: Ref EdictT -> Maybe B.ByteString -> Quake ()
setModel edictRef name = do
    when (isNothing name) $
      Com.comError Constants.errDrop "PF_setmodel: NULL"

    let modelName = fromJust name
    idx <- SVInit.modelIndex name

    modifyRef edictRef (\v -> v & eEntityState.esModelIndex .~ idx)

    -- if it is an inline model, get the size information for it
    when (BC.head modelName == '*') $ do
      (Ref modelIdx) <- CM.inlineModel modelName
      Just model <- preuse $ cmGlobals.cmMapCModels.ix modelIdx

      modifyRef edictRef (\v -> v & eMins .~ (model^.cmMins)
                                     & eMaxs .~ (model^.cmMaxs))

      SVWorld.linkEdict edictRef

configString :: Int -> B.ByteString -> Quake ()
configString index val = do
    when (index < 0 || index >= Constants.maxConfigStrings) $
      Com.comError Constants.errDrop ("configstring: bad index " `B.append` BC.pack (show index) `B.append` "\n") -- IMPROVE?

    svGlobals.svServer.sConfigStrings.ix index .= val

    state <- use $ svGlobals.svServer.sState

    unless (state == Constants.ssLoading) $ do
      SZ.clear (svGlobals.svServer.sMulticast)
      MSG.writeCharI (svGlobals.svServer.sMulticast) (fromIntegral Constants.svcConfigString)
      MSG.writeShort (svGlobals.svServer.sMulticast) (fromIntegral index)
      MSG.writeString (svGlobals.svServer.sMulticast) val

      origin <- use $ globals.gVec3Origin
      SVSend.multicast origin Constants.multicastAllR

writeChar :: Int -> Quake ()
writeChar c = MSG.writeCharI (svGlobals.svServer.sMulticast) c

writeByte :: Int -> Quake ()
writeByte c = MSG.writeByteI (svGlobals.svServer.sMulticast) c

writeShort :: Int -> Quake ()
writeShort c = MSG.writeShort (svGlobals.svServer.sMulticast) c

writeLong :: Int -> Quake ()
writeLong c = MSG.writeLong (svGlobals.svServer.sMulticast) c

writeFloat :: Float -> Quake ()
writeFloat f = MSG.writeFloat (svGlobals.svServer.sMulticast) f

writeString :: B.ByteString -> Quake ()
writeString s = MSG.writeString (svGlobals.svServer.sMulticast) s

writePos :: V3 Float -> Quake ()
writePos pos = MSG.writePos (svGlobals.svServer.sMulticast) pos

writeDir :: V3 Float -> Quake ()
writeDir dir = MSG.writeDir (svGlobals.svServer.sMulticast) dir

writeAngle :: Float -> Quake ()
writeAngle f = MSG.writeAngle (svGlobals.svServer.sMulticast) f

{-
- PF_inPVS
- 
- Also checks portalareas so that doors block sight.
-}
inPVS :: V3 Float -> V3 Float -> Quake Bool
inPVS p1 p2 = do
    leafNum <- CM.pointLeafNum p1
    cluster <- CM.leafCluster leafNum
    area1 <- CM.leafArea leafNum
    mask <- CM.clusterPVS cluster

    leafNum' <- CM.pointLeafNum p2
    cluster' <- CM.leafCluster leafNum'
    area2 <- CM.leafArea leafNum'

    if | cluster' == -1 -> return False
       | (mask `B.index` (cluster' `shiftR` 3)) .&. (1 `shiftL` (cluster' .&. 7)) == 0 -> return False
       | otherwise -> do
           connected <- CM.areasConnected area1 area2

           if not connected
             then return False -- a door blocks sight
             else return True

{-
- PF_inPHS.
- 
- Also checks portalareas so that doors block sound.
-}
inPHS :: V3 Float -> V3 Float -> Quake Bool
inPHS p1 p2 = do
    leafNum <- CM.pointLeafNum p1
    cluster <- CM.leafCluster leafNum
    area1 <- CM.leafArea leafNum
    mask <- CM.clusterPHS cluster

    leafNum' <- CM.pointLeafNum p2
    cluster' <- CM.leafCluster leafNum'
    area2 <- CM.leafArea leafNum'

    if | cluster' == -1 -> return False
       | (mask `B.index` (cluster' `shiftR` 3)) .&. (1 `shiftL` (cluster' .&. 7)) == 0 -> return False -- more than one bounce away
       | otherwise -> do
           connected <- CM.areasConnected area1 area2

           if not connected
             then return False -- a door blocks hearing
             else return True

startSound :: Maybe (Ref EdictT) -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound maybeEdictRef channel soundNum volume attenuation timeOfs = do
    case maybeEdictRef of
      Nothing -> return ()
      Just edictRef -> SVSend.startSound Nothing edictRef channel soundNum volume attenuation timeOfs

{-
-  SV_ShutdownGameProgs
- 
- Called when either the entire server is being killed, or it is changing
- to a different game directory. 
-}
shutdownGameProgs :: Quake ()
shutdownGameProgs = GameBase.shutdownGame

{-
- SV_InitGameProgs
- 
- Init the game subsystem for a new map. 
-}
initGameProgs :: Quake ()
initGameProgs = do
    -- unload anything we have now
    shutdownGameProgs
    -- all functions set in game_export_t (rst)
    GameBase.getGameApi newGameImportT
    GameSave.initGame
