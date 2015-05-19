{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.PlayerClient where

import Control.Lens (use, (^.), ix, preuse, (.=), zoom, (%=))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (toLower)
import Data.Maybe (isNothing)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.Info as Info
import qualified Game.GameItems as GameItems
import qualified Game.GameMisc as GameMisc
import qualified Game.GameSVCmds as GameSVCmds
import qualified Game.GameUtil as GameUtil
import qualified Game.PlayerHud as PlayerHud
import qualified Game.PlayerView as PlayerView
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

-- Called when a player drops from the server. Will not be called between levels. 
clientDisconnect :: Maybe EdictReference -> Quake ()
clientDisconnect _ = io (putStrLn "PlayerClient.clientDisconnect") >> undefined -- TODO

{-
- Called when a player begins connecting to the server. The game can refuse
- entrance to a client by returning false. If the client is allowed, the
- connection process will continue and eventually get to ClientBegin()
- Changing levels will NOT cause this to be called again, but loadgames
- will. 
-}
clientConnect :: EdictReference -> B.ByteString -> Quake (Bool, B.ByteString)
clientConnect edictRef@(EdictReference edictIdx) userInfo = do
    -- check to see if they are on the banned IP list
    value <- Info.valueForKey userInfo "ip"
    banned <- GameSVCmds.filterPacket value
    if banned
      then do
        userInfo' <- Info.setValueForKey userInfo "rejmsg" "Banned."
        return (False, userInfo')
      else do
        -- check for a spectator
        value' <- Info.valueForKey userInfo "spectator"
        deathmatchValue <- liftM (^.cvValue) deathmatchCVar

        (done, userInfo') <- if deathmatchValue /= 0 && B.length value' > 0 && value' /= "0"
                               then do
                                 io (putStrLn "PlayerClient.clientConnect") >> undefined -- TODO
                               else do
                                 -- check for a password
                                 pass <- Info.valueForKey userInfo "password"
                                 actualPassword <- liftM (^.cvString) spectatorPasswordCVar
                                 if not (passwordOK actualPassword pass)
                                   then do
                                     info <- Info.setValueForKey userInfo "rejmsg" "Password required or incorrect."
                                     return (True, info)
                                   else
                                     return (False, userInfo)

        if done
          then 
            return (False, userInfo')
          else do
            -- they can connect
            let gClientIdx = edictIdx - 1
                gClientRef = GClientReference gClientIdx
            gameBaseGlobals.gbGEdicts.ix edictIdx.eClient .= Just gClientRef

            -- if there is already a body waiting for us (a loadgame), just
            -- take it, otherwise spawn one from scratch
            Just inUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eInUse
            unless inUse $ do
              -- clear the respawning variables
              initClientResp gClientRef
              autoSaved <- use $ gameBaseGlobals.gbGame.glAutosaved
              Just weapon <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpWeapon

              when (not autoSaved || isNothing weapon) $
                initClientPersistant gClientRef

            userInfo'' <- clientUserInfoChanged edictRef userInfo

            maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients

            when (maxClients > 1) $ do
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              Just netName <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpNetName
              dprintf $ netName `B.append` " connected\n"

            gameBaseGlobals.gbGEdicts.ix edictIdx.eSvFlags .= 0 -- make sure we start with known default
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpConnected .= True

            return (True, userInfo'')

{-
- Some information that should be persistant, like health, is still stored
- in the edict structure, so it needs to be mirrored out to the client
- structure before all the edicts are wiped. 
-}
saveClientData :: Quake ()
saveClientData = do
    maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
    edicts <- use $ gameBaseGlobals.gbGEdicts

    mapM_ (\idx -> updateClient (edicts V.! (idx + 1)) idx) [0..maxClients-1]

  where updateClient :: EdictT -> Int -> Quake ()
        updateClient edict idx =
          when (edict^.eInUse) $ do
            Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix idx
            let pers = client^.gcPers

            let updatedPers = pers { _cpHealth     = edict^.eEdictStatus.eHealth
                                   , _cpMaxHealth  = edict^.eEdictStatus.eMaxHealth
                                   , _cpSavedFlags = (edict^.eFlags) .&. (Constants.flGodMode .|. Constants.flNoTarget .|. Constants.flPowerArmor)
                                   }

            coopValue <- liftM (^.cvValue) coopCVar
            let finalPers = if coopValue /= 0
                              then updatedPers { _cpScore = client^.gcResp.crScore }
                              else updatedPers
            
            gameBaseGlobals.gbGame.glClients.ix idx.gcPers .= finalPers

initBodyQue :: Quake ()
initBodyQue = do
    gameBaseGlobals.gbLevel.llBodyQue .= 0
    mapM_ spawnBodyQue [0..Constants.bodyQueueSize-1]

  where spawnBodyQue :: Int -> Quake ()
        spawnBodyQue _ = do
          EdictReference idx <- GameUtil.spawn
          gameBaseGlobals.gbGEdicts.ix idx.eClassName .= "bodyque"

spInfoPlayerStart :: EdictReference -> Quake ()
spInfoPlayerStart (EdictReference edictIdx) = do
    coopValue <- liftM (^.cvValue) coopCVar

    unless (coopValue == 0) $ do
      mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

      when (mapName == "security") $ do
        time <- use $ gameBaseGlobals.gbLevel.llTime
        -- invoke one of our gross, ugly, disgusting hacks
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
          eaThink .= Just spCreateCoopSpots
          eaNextThink .= time + Constants.frameTime

{-
- QUAKED info_player_deathmatch (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for deathmatch games.
-}
spInfoPlayerDeathmatch :: EdictReference -> Quake ()
spInfoPlayerDeathmatch er = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue == 0
      then GameUtil.freeEdict er
      else void $ think GameMisc.spMiscTeleporterDest er

{-
- QUAKED info_player_coop (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for coop games.
-}
spInfoPlayerCoop :: EdictReference -> Quake ()
spInfoPlayerCoop er@(EdictReference edictIdx) = do
    coopValue <- liftM (^.cvValue) coopCVar

    if coopValue == 0
      then GameUtil.freeEdict er
      else do
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

        let names = [ "jail2", "jail4", "mine1", "mine2", "mine3", "mine4" , "lab"
                    , "boss1", "fact3", "biggun", "space", "command", "power2", "strike" ]

        when (any (== mapName) names) $ do
          -- invoke one of our gross, ugly, disgusting hacks
          time <- use $ gameBaseGlobals.gbLevel.llTime

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaThink .= Just spFixCoopSpots
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaNextThink .= time + Constants.frameTime

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = return ()

spFixCoopSpots :: EntThink
spFixCoopSpots =
  GenericEntThink "SP_FixCoopSpots" $ \_ -> do
    io (putStrLn "PlayerClient.spFixCoopSpots") >> undefined -- TODO

spCreateCoopSpots :: EntThink
spCreateCoopSpots =
  GenericEntThink "SP_CreateCoopSpots" $ \_ -> do
    io (putStrLn "PlayerClient.spCreateCoopSpots") >> undefined -- TODO

clientBeginServerFrame :: EdictReference -> Quake ()
clientBeginServerFrame _ = io (putStrLn "PlayerClient.clientBeginServerFrame") >> undefined -- TODO

initClientResp :: GClientReference -> Quake ()
initClientResp (GClientReference gClientIdx) = do
    frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
    Just pers <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp .= newClientRespawnT { _crEnterFrame = frameNum, _crCoopRespawn = pers }

{-
- This is only called when the game first initializes in single player, but
- is called after each death and level change in deathmatch. 
-}
initClientPersistant :: GClientReference -> Quake ()
initClientPersistant (GClientReference gClientIdx) = do
    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers .= newClientPersistantT

    Just itemRef@(GItemReference itemIdx) <- GameItems.findItem "Blaster"

    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers) $ do
      cpSelectedItem .= itemIdx
      cpInventory.ix itemIdx .= 1

      cpWeapon .= Just itemRef

      cpHealth .= 100
      cpMaxHealth .= 100

      cpMaxBullets .= 200
      cpMaxShells .= 100
      cpMaxRockets .= 50
      cpMaxGrenades .= 50
      cpMaxCells .= 200
      cpMaxSlugs .= 50

      cpConnected .= True

{-
- Called whenever the player updates a userinfo variable.
- 
- The game can override any of the settings in place (forcing skins or
- names, etc) before copying it off. 
-
-}
clientUserInfoChanged :: EdictReference -> B.ByteString -> Quake B.ByteString
clientUserInfoChanged (EdictReference edictIdx) userInfo = do
    -- check for malformed or illegal info strings
    if not (Info.validate userInfo)
      then return "\\name\\badinfo\\skin\\male/grunt"
      else do
        Just (Just (GClientReference gClientIdx)) <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eClient

        -- set name
        name <- Info.valueForKey userInfo "name"
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpNetName .= name

        -- set spectator
        s <- Info.valueForKey userInfo "spectator"
        -- spectators are only supported in deathmatch
        deathmatchValue <- liftM (^.cvValue) deathmatchCVar
        if deathmatchValue /= 0 && s /= "0"
          then gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpSpectator .= True
          else gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpSpectator .= False

        -- set skin
        skin <- Info.valueForKey userInfo "skin"

        let playerNum = edictIdx - 1

        -- combine name and skin into a configstring
        configString <- use $ gameBaseGlobals.gbGameImport.giConfigString
        configString (Constants.csPlayerSkins + playerNum) (name `B.append` "\\" `B.append` skin)

        -- fov
        dmFlagsValue :: Int <- liftM (truncate .(^.cvValue)) dmFlagsCVar
        if deathmatchValue /= 0 && (dmFlagsValue .&. Constants.dfFixedFov) /= 0
          then
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psFOV .= 90
          else do
            fov <- Info.valueForKey userInfo "fov"
            let tmpFov = Lib.atoi fov
                finalFov = if | tmpFov < 1 -> 90
                              | tmpFov > 160 -> 160
                              | otherwise -> tmpFov
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psFOV .= fromIntegral finalFov

        -- handedness
        hand <- Info.valueForKey userInfo "hand"
        when (B.length hand > 0) $
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpHand .= Lib.atoi hand

        -- save off the userinfo in case we want to check something later
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpUserInfo .= userInfo

        return userInfo

passwordOK :: B.ByteString -> B.ByteString -> Bool
passwordOK p1 p2 =
    if B.length p1 > 0 && p1 /= "none" && p1 /= p2
      then False
      else True

{-
- Called when a client has finished connecting, and is ready to be placed
- into the game. This will happen every level load. 
-}
clientBegin :: EdictReference -> Quake ()
clientBegin edictRef@(EdictReference edictIdx) = do
    let gClientRef = GClientReference (edictIdx - 1)
    gameBaseGlobals.gbGEdicts.ix edictIdx.eClient .= Just gClientRef

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        clientBeginDeathmatch edictRef
      else do
        -- if there is already a body waiting for us (a loadgame), just
        -- take it, otherwise spawn one from scratch
        Just inUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eInUse

        if inUse
          then do
            -- the client has cleared the client side viewangles upon
            -- connecting to the server, which is different than the
            -- state when the game is saved, so we need to compensate
            -- with deltaangles
            Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix (edictIdx - 1)
            gameBaseGlobals.gbGame.glClients.ix (edictIdx - 1).gcPlayerState.psPMoveState.pmsDeltaAngles .= fmap Math3D.angleToShort (gClient^.gcPlayerState.psViewAngles)
          else do
            -- a spawn point will completely reinitialize the entity
            -- except for the persistant data that was initialized at
            -- ClientConnect() time
            GameUtil.initEdict edictRef
            gameBaseGlobals.gbGEdicts.ix edictIdx.eClassName .= "player"
            initClientResp gClientRef
            putClientInServer edictRef

        intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime
        if intermissionTime /= 0
          then
            PlayerHud.moveClientToIntermission edictRef
          else do
            maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
            when (maxClients > 1) $ do
              gameImport <- use $ gameBaseGlobals.gbGameImport
              let writeByte = gameImport^.giWriteByte
                  writeShort = gameImport^.giWriteShort
                  multicast = gameImport^.giMulticast
                  bprintf = gameImport^.giBprintf

              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
              Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix (edictIdx - 1)

              writeByte Constants.svcMuzzleFlash
              writeShort edictIdx
              writeByte Constants.mzLogin
              multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs
              bprintf Constants.printHigh ((gClient^.gcPers.cpNetName) `B.append` " entered the game\n")

        -- make sure all view stuff is valid
        PlayerView.clientEndServerFrame edictRef

clientBeginDeathmatch :: EdictReference -> Quake ()
clientBeginDeathmatch _ = do
    io (putStrLn "PlayerClient.clientBeginDeathmatch") >> undefined -- TODO

-- Called when a player connects to a server or respawns in a deathmatch.
putClientInServer :: EdictReference -> Quake ()
putClientInServer edictRef@(EdictReference edictIdx) = do
    -- find a spawn point
    -- do it before setting health back up, so farthest
    -- ranging doesn't count this client
    (spawnOrigin, spawnAngles) <- selectSpawnPoint edictRef
    let gClientIdx = edictIdx - 1

    -- deathmatch wipes most client data every spawn
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    coopValue <- liftM (^.cvValue) coopCVar
    resp <- if | deathmatchValue /= 0 -> do
                  Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                  let userInfo = gClient^.gcPers.cpUserInfo
                  initClientPersistant (GClientReference gClientIdx)
                  void $ clientUserInfoChanged edictRef userInfo
                  return (gClient^.gcResp)

               | coopValue /= 0 -> do
                   Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                   let userInfo = gClient^.gcPers.cpUserInfo
                       coopResp = (gClient^.gcResp.crCoopRespawn) { _cpGameHelpChanged = gClient^.gcPers.cpGameHelpChanged
                                                                  , _cpHelpChanged = gClient^.gcPers.cpHelpChanged
                                                                  }
                       resp = (gClient^.gcResp) { _crCoopRespawn = coopResp }

                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers .= resp^.crCoopRespawn
                   void $ clientUserInfoChanged edictRef userInfo
                   when ((resp^.crScore) > (resp^.crCoopRespawn.cpScore)) $
                     gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpScore .= resp^.crScore

                   return resp

               | otherwise -> return newClientRespawnT

    -- clear everything but the persistant data
    Just saved <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers
    gameBaseGlobals.gbGame.glClients.ix gClientIdx .= (newGClientT gClientIdx) { _gcPers = saved }

    when ((saved^.cpHealth) <= 0) $
      initClientPersistant (GClientReference gClientIdx)

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp .= resp

    -- copy some data from the client to the entity
    fetchClientEntData edictRef

    -- clear entity values
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEdictOther.eoGroundEntity .= Nothing
      eClient                    .= Just (GClientReference gClientIdx)
      eEdictStatus.eTakeDamage   .= Constants.damageAim
      eMoveType                  .= Constants.moveTypeWalk
      eEdictStatus.eViewHeight   .= 22
      eInUse                     .= True
      eClassName                 .= "player"
      eEdictPhysics.eMass        .= 200
      eSolid                     .= Constants.solidBbox
      eEdictStatus.eDeadFlag     .= Constants.deadNo
      eEdictPhysics.eAirFinished .= levelTime + 12
      eClipMask                  .= Constants.maskPlayerSolid
      eEdictInfo.eiModel         .= Just "players/male/tris.md2"
      eEdictAction.eaPain        .= Just playerPain
      eEdictAction.eaDie         .= Just playerDie
      eWaterLevel                .= 0
      eWaterType                 .= 0
      eFlags                     %= (.&. (complement Constants.flNoKnockback))
      eSvFlags                   %= (.&. (complement Constants.svfDeadMonster))
      eEdictMinMax.eMins         .= V3 (-16) (-16) (-24)
      eEdictMinMax.eMaxs         .= V3 16 16 32
      eEdictPhysics.eVelocity    .= V3 0 0 0
      
    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
      gcPlayerState .= newPlayerStateT
      gcPlayerState.psPMoveState.pmsOrigin .= fmap (truncate . (* 8)) spawnOrigin

    io (putStrLn "PlayerClient.putClientInServer") >> undefined -- TODO

selectSpawnPoint :: EdictReference -> Quake (V3 Float, V3 Float)
selectSpawnPoint _ = do
    io (putStrLn "PlayerClient.selectSpawnPoint") >> undefined -- TODO

fetchClientEntData :: EdictReference -> Quake ()
fetchClientEntData _ = do
    io (putStrLn "PlayerClient.fetchClientEntData") >> undefined -- TODO

playerPain :: EntPain
playerPain = 
  GenericEntPain "player_pain" $ \_ _ _ _ -> return ()

playerDie :: EntDie
playerDie =
  GenericEntDie "player_die" $ \_ _ _ _ _ -> do
    io (putStrLn "PlayerClient.playerDie") >> undefined -- TODO
