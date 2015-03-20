{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SVInit where

import Control.Lens ((.=), use)
import Control.Monad (when, void, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified Client.SCR as SCR
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CVar as CVar
import qualified Server.SVSend as SVSend

{-
- SV_SpawnServer.
- 
- Change the server to a new map, taking all connected clients along with
- it.
-}
spawnServer :: B.ByteString -> B.ByteString -> Int -> Bool -> Bool -> Quake ()
spawnServer = undefined -- TODO

{-
- SV_InitGame.
- 
- A brand new game has been started.
-}
initGame :: Quake ()
initGame = undefined -- TODO

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
svMap attractLoop levelString loadGame = do
    svGlobals.svServer.sLoadGame .= loadGame
    svGlobals.svServer.sAttractLoop .= attractLoop

    state <- use $ svGlobals.svServer.sState

    when (state == Constants.ssDead && not loadGame)
      initGame -- the game is just starting

    -- if there is a + in the map, set nextserver to the remainder
    level <- case '+' `BC.elemIndex` levelString of
              Nothing -> do
                void $ CVar.set "nextserver" ""
                return levelString
              Just idx -> do
                let levelSubstring = B.drop (idx + 1) levelString
                void $ CVar.set "nextserver" ("gamemap \"" `B.append` levelSubstring `B.append` "\"")
                return $ B.take idx levelString

    -- rst: base1 works for full, demo1 works for demo, so we need to store first map
    firstmap <- use $ svGlobals.svFirstMap
    when (B.null firstmap) $
      unless (or $ fmap (`BC.isSuffixOf` levelString) [".cin", ".pcx", ".dm2"]) $ do
        let fm = case '+' `BC.elemIndex` levelString of
                   Nothing -> levelString
                   Just idx -> B.drop (idx + 1) levelString
        svGlobals.svFirstMap .= fm

    -- ZOID: special hack for end game screen in coop mode
    coop <- CVar.variableValue "coop"
    fm <- use $ svGlobals.svFirstMap
    when (coop /= 0 && level == "victory.pcx") $
      void $ CVar.set "nextserver" ("gamemap \"*" `B.append` fm `B.append` "\"")

    -- if there is a $, use the remainder as a spawnpoint
    let (spawnpoint, updatedLevel) = case '$' `BC.elemIndex` level of
                                       Nothing -> ("", level)
                                       Just idx -> (B.drop (idx + 1) level, B.take idx level)

    -- skip the end-of-unit flag * if necessary
    let finalLevel = if updatedLevel `BC.index` 0 == '*'
                       then B.drop 1 updatedLevel
                       else updatedLevel

    let len = B.length finalLevel

    SCR.beginLoadingPlaque -- for local system
    SVSend.broadcastCommand "changing\n"

    if | len > 4 && ".cin" `BC.isSuffixOf` finalLevel ->
           spawnServer finalLevel spawnpoint Constants.ssCinematic attractLoop loadGame
       | len > 4 && ".dm2" `BC.isSuffixOf` finalLevel ->
           spawnServer finalLevel spawnpoint Constants.ssDemo attractLoop loadGame
       | len > 4 && ".pcx" `BC.isSuffixOf` finalLevel ->
           spawnServer finalLevel spawnpoint Constants.ssPic attractLoop loadGame
       | otherwise -> do
           SVSend.sendClientMessages
           spawnServer finalLevel spawnpoint Constants.ssGame attractLoop loadGame
           CBuf.copyToDefer

    SVSend.broadcastCommand "reconnect\n"
