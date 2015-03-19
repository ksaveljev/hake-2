module Server.SVSend where

import qualified Data.ByteString as B

import Quake
import Server.ClientT

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
clientPrintf = undefined -- TODO

{-
=================
SV_BroadcastPrintf

Sends text to all active clients
=================
-}
broadcastPrintf :: Int -> B.ByteString -> Quake ()
broadcastPrintf = undefined -- TODO

{-
=======================
SV_SendClientMessages
=======================
-}
svSendClientMessages :: Quake ()
svSendClientMessages = undefined -- TODO
