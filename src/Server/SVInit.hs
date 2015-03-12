module Server.SVInit where

import qualified Data.ByteString as B

import Quake

{-
- SV_Map
- 
- the full syntax is:
- 
- map [*] <map>$ <startspot>+ <nextserver>
- 
- command from the console or progs. Map can also be a.cin, .pcx, or .dm2 file.
- 
- Nextserver is used to allow a cinematic to play, then proceed to
- another level:
- 
- map tram.cin+jail_e3
-}
svMap :: Bool -> B.ByteString -> Bool -> Quake ()
svMap = undefined -- TODO
