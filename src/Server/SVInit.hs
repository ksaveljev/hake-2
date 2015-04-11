{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SVInit where

import Data.Bits ((.|.))
import Data.Foldable (forM_)
import Control.Lens ((.=), use, (^.), (%=), (+=), preuse, ix)
import Control.Monad (when, void, unless, liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Client.CL as CL
import qualified Client.SCR as SCR
import qualified Game.GameBase as GameBase
import qualified Game.GameSpawn as GameSpawn
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified QCommon.SZ as SZ
import qualified Server.SVGame as SVGame
import qualified Server.SVMain as SVMain
import {-# SOURCE #-} qualified Server.SVSend as SVSend
import qualified Server.SVWorld as SVWorld
import qualified Sys.NET as NET
import qualified Util.Lib as Lib

findIndex :: B.ByteString -> Int -> Int -> Bool -> Quake Int
findIndex name start maxIdx create =
    if B.length name == 0
      then return 0
      else do
        configStrings <- use $ svGlobals.svServer.sConfigStrings
        case findConfigString configStrings 1 of
          (True, idx) -> return idx
          (False, idx) ->
            if not create
              then return 0
              else do
                when (idx == maxIdx) $
                  Com.comError Constants.errDrop "*Index: overflow"

                svGlobals.svServer.sConfigStrings %= (V.// [(start + idx, name)])

                state <- use $ svGlobals.svServer.sState

                when (state /= Constants.ssLoading) $ do
                  SZ.clear (svGlobals.svServer.sMulticast)
                  MSG.writeCharI (svGlobals.svServer.sMulticast) Constants.svcConfigString
                  MSG.writeShort (svGlobals.svServer.sMulticast) (start + idx)
                  MSG.writeString (svGlobals.svServer.sMulticast) name
                  origin <- use $ globals.vec3Origin
                  SVSend.multicast origin Constants.multicastAllR

                return idx

  where findConfigString :: V.Vector B.ByteString -> Int -> (Bool, Int)
        findConfigString configStrings i
          | i >= maxIdx || configStrings V.! (start + i) == "" = (False, i)
          | configStrings V.! (start + i) == name = (True, i)
          | otherwise = findConfigString configStrings (i + 1)

modelIndex :: B.ByteString -> Quake Int
modelIndex name = findIndex name Constants.csModels Constants.maxModels True

soundIndex :: B.ByteString -> Quake Int
soundIndex name = findIndex name Constants.csSounds Constants.maxSounds True

imageIndex :: B.ByteString -> Quake Int
imageIndex name = findIndex name Constants.csImages Constants.maxImages True

{-
- SV_CreateBaseline
- 
- Entity baselines are used to compress the update messages to the clients --
- only the fields that differ from the baseline will be transmitted.
-}
createBaseline :: Quake ()
createBaseline = do
    numEdicts <- use $ gameBaseGlobals.gbNumEdicts

    edictBaseline 1 numEdicts

  where edictBaseline :: Int -> Int -> Quake ()
        edictBaseline idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix idx
              if not (edict^.eInUse) || (edict^.eEntityState.esModelIndex) == 0 && (edict^.eEntityState.esSound) == 0 && (edict^.eEntityState.esEffects) == 0
                then edictBaseline (idx + 1) maxIdx
                else do
                  gameBaseGlobals.gbGEdicts.ix idx.eEntityState.esNumber .= idx

                  -- take current state as baseline
                  gameBaseGlobals.gbGEdicts.ix idx.eEntityState.esOldOrigin .= (edict^.eEntityState.esOrigin)
                  Just entityState <- preuse $ gameBaseGlobals.gbGEdicts.ix idx.eEntityState
                  svGlobals.svServer.sBaselines.ix idx .= entityState

checkForSavegame :: Quake ()
checkForSavegame = io (putStrLn "SVInit.checkForSavegame") >> undefined -- TODO

{-
- SV_SpawnServer.
- 
- Change the server to a new map, taking all connected clients along with
- it.
-}
spawnServer :: B.ByteString -> B.ByteString -> Int -> Bool -> Bool -> Quake ()
spawnServer server spawnPoint srvState attractLoop loadGame = do
    when attractLoop $
      void $ CVar.set "paused" "0"

    Com.printf "------- Server Initialization -------\n"

    Com.dprintf $ "SpawnServer: " `B.append` server `B.append` "\n"

    demofile <- use $ svGlobals.svServer.sDemoFile

    forM_ demofile Lib.fClose

    -- any partially connected client will be restarted
    svGlobals.svServerStatic.ssSpawnCount += 1

    svGlobals.svServer.sState .= Constants.ssDead
    globals.serverState .= Constants.ssDead

    -- wipe the entire per-level structure
    svGlobals.svServer .= newServerT

    svGlobals.svServerStatic.ssRealTime .= 0
    svGlobals.svServer.sLoadGame .= loadGame
    svGlobals.svServer.sAttractLoop .= attractLoop

    -- save name for levels that don't set message
    svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csName, server)])

    deathmatch <- CVar.variableValue "deathmatch"
    if deathmatch /= 0
      then do
        airAccelerateValue <- liftM (^.cvValue) svAirAccelerateCVar
        svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csAirAccel, BC.pack $ show airAccelerateValue)]) -- IMPROVE: convert Float to ByteString using binary package?
        pMoveGlobals.pmAirAccelerate .= airAccelerateValue
      else do
        svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csAirAccel, "0")])
        pMoveGlobals.pmAirAccelerate .= 0

    bufData <- use $ svGlobals.svServer.sMulticastBuf
    SZ.init (svGlobals.svServer.sMulticast) bufData Constants.maxMsgLen

    svGlobals.svServer.sName .= server

    -- leave slots at start for clients only
    svGlobals.svServerStatic.ssClients %=
      fmap (\client -> let updatedState = if (client^.cState) > Constants.csConnected
                                           then Constants.csConnected
                                           else client^.cState
                      in client { _cState = updatedState, _cLastFrame = -1 })

    svGlobals.svServer.sTime .= 1000
    svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csName, server)])

    (modelIdx, iw) <- if srvState /= Constants.ssGame
                        then
                          CM.loadMap "" False [0] -- no real map
                        else do
                          let mapName = "maps/" `B.append` server `B.append` ".bsp"
                          svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csModels + 1, mapName)])
                          CM.loadMap mapName False [0]

    svGlobals.svServer.sModels %= (V.// [(1, CModelReference modelIdx)])

    let checksum = head iw
    svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csMapChecksum, BC.pack (show checksum))]) -- IMPROVE: convert Int to ByteString using binary package?

    -- clear physics interaction links
    SVWorld.clearWorld

    modelsCount <- CM.numInlineModels
    svGlobals.svServer.sConfigStrings %= (V.// fmap (\i -> (Constants.csModels + 1 + i, "*" `B.append` BC.pack (show i))) [1..modelsCount-1]) -- IMPROVE: convert Int to ByteString using binary package?
    -- copy references
    updatedModels <- mapM (\i -> CM.inlineModel ("*" `B.append` BC.pack (show i)) >>= \mIdx -> return (i + 1, CModelReference mIdx)) [1..modelsCount-1] -- IMPROVE: convert Int to ByteString using binary package?
    svGlobals.svServer.sModels %= (V.// updatedModels)

    -- spawn the rest of the entities on the map
    
    -- precache and static commands can be issued during
    -- map initialization

    svGlobals.svServer.sState .= Constants.ssLoading
    globals.serverState .= Constants.ssLoading

    -- load and spawn all other entities
    es <- CM.entityString
    GameSpawn.spawnEntities server es spawnPoint

    -- run two frames to allow everything to settle
    GameBase.runFrame
    GameBase.runFrame

    -- all precaches are complete
    svGlobals.svServer.sState .= srvState
    globals.serverState .= srvState

    -- create a baseline for more efficient communications
    createBaseline

    -- check for savegame
    checkForSavegame

    -- set serverinfo variable
    void $ CVar.fullSet "mapname" server (Constants.cvarServerInfo .|. Constants.cvarNoSet)

{-
- SV_InitGame.
- 
- A brand new game has been started.
-}
initGame :: Quake ()
initGame = do
    initialized <- use $ svGlobals.svServerStatic.ssInitialized

    if initialized
      -- cause any connected clients to reconnect
      then SVMain.shutdown "Server restarted\n" True
      else do
        -- make sure the client is down
        CL.drop
        SCR.beginLoadingPlaque

    -- get any latched variable changes (maxclients, etc)
    CVar.getLatchedVars

    svGlobals.svServerStatic.ssInitialized .= True

    coop <- CVar.variableValue "coop"
    deathmatch <- CVar.variableValue "deathmatch"

    when (coop /= 0 && deathmatch /= 0) $ do
      Com.printf "Deathmatch and Coop both set, disabling Coop\n"
      void $ CVar.fullSet "coop" "0" (Constants.cvarServerInfo .|. Constants.cvarLatch)

    -- dedicated servers can't be single player and are usually DM
    -- so unless they explicitly set coop, force it to deathmatch
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar
    when (dedicatedValue /= 0 && coop == 0) $
      void $ CVar.fullSet "deathmatch" "1" (Constants.cvarServerInfo .|. Constants.cvarLatch)

    initClients

    r <- Lib.rand
    svGlobals.svServerStatic.ssSpawnCount .= fromIntegral r

    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
    svGlobals.svServerStatic.ssClients .= V.generate maxClientsValue (\idx -> newClientT { _cServerIndex = idx })

    let numClientEntities = maxClientsValue * Constants.updateBackup * 64
    svGlobals.svServerStatic.ssNumClientEntities .= numClientEntities
    svGlobals.svServerStatic.ssClientEntities .= V.replicate numClientEntities (newEntityStateT Nothing)

    -- init network stuff
    NET.config (maxClientsValue > 1)

    -- heartbeats will always be sent to the id master
    svGlobals.svServerStatic.ssLastHeartbeat .= -99999 -- send immediately
    let idmaster = "192.246.40.37:" `B.append` BC.pack (show Constants.portMaster) -- IMPROVE: convert Int to ByteString using binary package?
    idmasterNetAdr <- NET.stringToAdr idmaster
    case idmasterNetAdr of
      Nothing -> return () -- well, shouldn't happen really
      Just adr -> svGlobals.svMasterAdr %= (V.// [(0, adr)])

    -- init game
    SVGame.initGameProgs

    clients <- use $ svGlobals.svServerStatic.ssClients
    let updatedClients = V.imap (\idx client -> client { _cEdict = Just (EdictReference (idx + 1)), _cLastCmd = newUserCmdT }) clients
    svGlobals.svServerStatic.ssClients .= updatedClients

  where initClients :: Quake ()
        initClients = do
          coop <- CVar.variableValue "coop"
          deathmatch <- CVar.variableValue "deathmatch"
          maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
          let serverLatchFlags = Constants.cvarServerInfo .|. Constants.cvarLatch

          if | deathmatch /= 0 ->
                 if | maxClientsValue <= 1 -> void $ CVar.fullSet "maxclients" "8" serverLatchFlags
                    | maxClientsValue > Constants.maxClients -> void $ CVar.fullSet "maxclients" (BC.pack $ show Constants.maxClients) serverLatchFlags
             | coop /= 0 ->
                 when (maxClientsValue <= 1 || maxClientsValue > 4) $
                   void $ CVar.fullSet "maxclients" "4" serverLatchFlags
             | otherwise ->
                 -- non-deathmatch, non-coop is one player
                 void $ CVar.fullSet "maxclients" "1" serverLatchFlags

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
