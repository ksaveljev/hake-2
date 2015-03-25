module Server.SVSend where

import Control.Lens (use)
import Control.Monad (when)
import Linear.V3 (V3(..))
import qualified Data.ByteString as B

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.MSG as MSG

{-
=============================================================================

EVENT MESSAGES

=============================================================================
-}

{-
=================
SV_ClientPrintf

Sends text across to be displayed if the level passes
=================
-}
clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()
clientPrintf _ _ _ = io (putStrLn "SVSend.clientPrintf") >> undefined -- TODO

{-
=================
SV_BroadcastPrintf

Sends text to all active clients
=================
-}
broadcastPrintf :: Int -> B.ByteString -> Quake ()
broadcastPrintf _ _ = io (putStrLn "SVSend.broadcastPrintf") >> undefined -- TODO

{-
=================
SV_BroadcastCommand

Sends text to all active clients
=================
-}
broadcastCommand :: B.ByteString -> Quake ()
broadcastCommand s = do
    state <- use $ svGlobals.svServer.sState
    when (state /= 0) $ do
      MSG.writeByteI (svGlobals.svServer.sMulticast) Constants.svcStuffText
      MSG.writeString (svGlobals.svServer.sMulticast) s
      multicast (V3 0 0 0) Constants.multicastAllR -- TODO: we send V3 0 0 0 but there is NULL in jake2 

{-
=================
SV_Multicast

Sends the contents of sv.multicast to a subset of the clients,
then clears sv.multicast.

MULTICAST_ALL	same as broadcast (origin can be null)
MULTICAST_PVS	send to clients potentially visible from org
MULTICAST_PHS	send to clients potentially hearable from org
=================
-}
multicast :: V3 Float -> Int -> Quake ()
multicast _ _ = io (putStrLn "SVsend.multicast") >> undefined -- TODO

{-
==================
SV_StartSound

Each entity can have eight independant sound sources, like voice,
weapon, feet, etc.

If cahnnel & 8, the sound will be sent to everyone, not just
things in the PHS.

FIXME: if entity isn't in PHS, they must be forced to be sent or
have the origin explicitly sent.

Channel 0 is an auto-allocate channel, the others override anything
already running on that entity/channel pair.

An attenuation of 0 will play full volume everywhere in the level.
Larger attenuations will drop off.  (max 4 attenuation)

Timeofs can range from 0.0 to 0.1 to cause sounds to be started
later in the frame than they normally would.

If origin is null, the origin is determined from the entity origin
or the midpoint of the entity box for bmodels.
==================
-}
startSound :: V3 Float -> EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound _ _ _ _ _ _ _ = io (putStrLn "SVsend.startSound") >> undefined -- TODO

{-
=======================
SV_SendClientMessages
=======================
-}
sendClientMessages :: Quake ()
sendClientMessages = io (putStrLn "SVSend.sendClientMessages") >> undefined -- TODO
