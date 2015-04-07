{-# LANGUAGE OverloadedStrings #-}
module Server.SVGame where

import Control.Lens (use, (.=), ix, zoom, preuse, (^.))
import Control.Monad (when, unless)
import Data.Maybe (isNothing, fromJust)
import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified Game.GameSave as GameSave
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
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
unicast :: EdictT -> Bool -> Quake ()
unicast _ _ = io (putStrLn "SVGame.unicast") >> undefined -- TODO

{-
- PF_dprintf
- 
- Debug print to server console.
-}
dprintf :: B.ByteString -> Quake ()
dprintf = Com.printf

-- Centerprintf for critical messages.
cprintfHigh :: EdictT -> B.ByteString -> Quake ()
cprintfHigh _ _ = io (putStrLn "SVGame.cprintfHigh") >> undefined -- TODO

{-
- PF_cprintf
- 
- Print to a single client.
-}
cprintf :: EdictT -> Int -> B.ByteString -> Quake ()
cprintf _ _ _ = io (putStrLn "SVGame.cprintf") >>  undefined -- TODO

{-
- PF_centerprintf
- 
- centerprint to a single client.
-}
centerPrintf :: EdictReference -> B.ByteString -> Quake ()
centerPrintf _ _ = io (putStrLn "SVGame.centerPrintf") >> undefined -- TODO

{-
-  PF_error
- 
-  Abort the server with a game error. 
-}
pfError :: B.ByteString -> Quake ()
pfError _ = io (putStrLn "SVGame.pfError") >> undefined -- TODO

pfError2 :: Int -> B.ByteString -> Quake ()
pfError2 _ _ = io (putStrLn "SVGame.pfError2") >> undefined -- TODO

{-
- PF_setmodel
- 
- Also sets mins and maxs for inline bmodels.
-}
setModel :: EdictReference -> Maybe B.ByteString -> Quake ()
setModel er@(EdictReference edictIdx) name = do
    when (isNothing name) $
      Com.comError Constants.errDrop "PF_setmodel: NULL"

    let modelName = fromJust name
    idx <- SVInit.modelIndex modelName

    gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esModelIndex .= idx

    -- if it is an inline model, get the size information for it
    when (BC.head modelName == '*') $ do
      modelIdx <- CM.inlineModel modelName
      Just model <- preuse $ cmGlobals.cmMapCModels.ix modelIdx

      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictMinMax) $ do
        eMins .= (model^.cmMins)
        eMaxs .= (model^.cmMaxs)

      SVWorld.linkEdict er

configString :: Int -> B.ByteString -> Quake ()
configString index val = do
    when (index < 0 || index >= Constants.maxConfigStrings) $
      Com.comError Constants.errDrop ("configstring: bad index " `B.append` BC.pack (show index) `B.append` "\n") -- IMPROVE?

    svGlobals.svServer.sConfigStrings.ix index .= val

    state <- use $ svGlobals.svServer.sState

    unless (state == Constants.ssLoading) $ do
      SZ.clear (svGlobals.svServer.sMulticast)
      MSG.writeCharI (svGlobals.svServer.sMulticast) Constants.svcConfigString
      MSG.writeShort (svGlobals.svServer.sMulticast) index
      MSG.writeString (svGlobals.svServer.sMulticast) val

      origin <- use $ globals.vec3Origin
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
inPVS _ _ = io (putStrLn "SVGame.inPVS") >> undefined -- TODO

{-
- PF_inPHS.
- 
- Also checks portalareas so that doors block sound.
-}
inPHS :: V3 Float -> V3 Float -> Quake Bool
inPHS _ _ = io (putStrLn "SVGame.inPHS") >> undefined -- TODO

startSound :: EdictReference -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound _ _ _ _ _ _ = io (putStrLn "SVGame.startSound") >> undefined -- TODO

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
