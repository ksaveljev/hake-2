module Server.SVGame where

import Linear.V3 (V3)
import qualified Data.ByteString as B

import Quake
import Game.EdictT
import qualified Game.GameBase as GameBase

{-
- PF_Unicast
- 
- Sends the contents of the mutlicast buffer to a single client.
-}
unicast :: EdictT -> Bool -> Quake ()
unicast = undefined -- TODO

{-
- PF_dprintf
- 
- Debug print to server console.
-}
dprintf :: B.ByteString -> Quake ()
dprintf = undefined -- TODO

-- Centerprintf for critical messages.
cprintfHigh :: EdictT -> B.ByteString -> Quake ()
cprintfHigh = undefined -- TODO

{-
- PF_cprintf
- 
- Print to a single client.
-}
cprintf :: EdictT -> Int -> B.ByteString -> Quake ()
cprintf = undefined -- TODO

{-
- PF_centerprintf
- 
- centerprint to a single client.
-}
centerPrintf :: EdictT -> B.ByteString -> Quake ()
centerPrintf = undefined -- TODO

{-
-  PF_error
- 
-  Abort the server with a game error. 
-}
pfError :: B.ByteString -> Quake ()
pfError = undefined -- TODO

pfError2 :: Int -> B.ByteString -> Quake ()
pfError2 = undefined -- TODO

{-
- PF_setmodel
- 
- Also sets mins and maxs for inline bmodels.
-}
setModel :: EdictT -> B.ByteString -> Quake ()
setModel = undefined -- TODO

configString :: Int -> B.ByteString -> Quake ()
configString = undefined -- TODO

writeChar :: Int -> Quake ()
writeChar = undefined -- TODO

writeByte :: Int -> Quake ()
writeByte = undefined -- TODO

writeShort :: Int -> Quake ()
writeShort = undefined -- TODO

writeLong :: Int -> Quake ()
writeLong = undefined -- TODO

writeFloat :: Float -> Quake ()
writeFloat = undefined -- TODO

writeString :: B.ByteString -> Quake ()
writeString = undefined -- TODO

writePos :: V3 Float -> Quake ()
writePos = undefined -- TODO

writeDir :: V3 Float -> Quake ()
writeDir = undefined -- TODO

writeAngle :: Float -> Quake ()
writeAngle = undefined -- TODO

{-
- PF_inPVS
- 
- Also checks portalareas so that doors block sight.
-}
inPVS :: V3 Float -> V3 Float -> Quake Bool
inPVS = undefined -- TODO

{-
- PF_inPHS.
- 
- Also checks portalareas so that doors block sound.
-}
inPHS :: V3 Float -> V3 Float -> Quake Bool
inPHS = undefined -- TODO

startSound :: EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound = undefined -- TODO

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
initGameProgs = undefined -- TODO
