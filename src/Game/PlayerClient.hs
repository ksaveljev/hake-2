{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.PlayerClient where

import Control.Lens (use, (^.), ix, preuse, (.=), zoom, (%=), (&), (.~), (%~), (+~), (-~))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (toLower)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.Info as Info
import qualified Game.GameChase  as GameChase
import qualified Game.GameItems as GameItems
import qualified Game.GameMisc as GameMisc
import qualified Game.GameSVCmds as GameSVCmds
import qualified Game.GameUtil as GameUtil
import qualified Game.Monsters.MPlayer as MPlayer
import qualified Game.PlayerHud as PlayerHud
import qualified Game.PlayerTrail as PlayerTrail
import qualified Game.PlayerView as PlayerView
import qualified Game.PlayerWeapon as PlayerWeapon
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
clientConnect edictRef userInfo = do
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
            edict <- readEdictT edictRef

            -- they can connect
            let gClientIdx = (edict^.eIndex) - 1
                gClientRef = GClientReference gClientIdx

            modifyEdictT edictRef (\v -> v & eClient .~ Just gClientRef)

            -- if there is already a body waiting for us (a loadgame), just
            -- take it, otherwise spawn one from scratch
            let inUse = edict^.eInUse

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

            modifyEdictT edictRef (\v -> v & eSvFlags .~ 0) -- make sure we start with known default
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

    mapM_ (\idx -> io (MV.read edicts (idx + 1)) >>= \e -> updateClient e idx) [0..maxClients-1]

  where updateClient :: EdictT -> Int -> Quake ()
        updateClient edict idx =
          when (edict^.eInUse) $ do
            Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix idx
            let pers = client^.gcPers

            let updatedPers = pers { _cpHealth     = edict^.eHealth
                                   , _cpMaxHealth  = edict^.eMaxHealth
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
          bodyRef <- GameUtil.spawn
          modifyEdictT bodyRef (\v -> v & eClassName .~ "bodyque")

spInfoPlayerStart :: EdictReference -> Quake ()
spInfoPlayerStart edictRef = do
    coopValue <- liftM (^.cvValue) coopCVar

    unless (coopValue == 0) $ do
      mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

      when (mapName == "security") $ do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        -- invoke one of our gross, ugly, disgusting hacks
        modifyEdictT edictRef (\v -> v & eThink .~ Just spCreateCoopSpots
                                       & eNextThink .~ levelTime + Constants.frameTime)

{-
- QUAKED info_player_deathmatch (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for deathmatch games.
-}
spInfoPlayerDeathmatch :: EdictReference -> Quake ()
spInfoPlayerDeathmatch edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue == 0
      then GameUtil.freeEdict edictRef
      else void $ think GameMisc.spMiscTeleporterDest edictRef

{-
- QUAKED info_player_coop (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for coop games.
-}
spInfoPlayerCoop :: EdictReference -> Quake ()
spInfoPlayerCoop edictRef = do
    coopValue <- liftM (^.cvValue) coopCVar

    if coopValue == 0
      then
        GameUtil.freeEdict edictRef

      else do
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

        let names = [ "jail2", "jail4", "mine1", "mine2", "mine3", "mine4" , "lab"
                    , "boss1", "fact3", "biggun", "space", "command", "power2", "strike" ]

        when (any (== mapName) names) $ do
          -- invoke one of our gross, ugly, disgusting hacks
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          modifyEdictT edictRef (\v -> v & eThink .~ Just spFixCoopSpots
                                         & eNextThink .~ levelTime + Constants.frameTime)

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

{-
- This will be called once for each server frame, before running any other
- entities in the world. 
-}
clientBeginServerFrame :: EdictReference -> Quake ()
clientBeginServerFrame edictRef = do
    intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime

    unless (intermissionTime /= 0) $ do
      edict <- readEdictT edictRef
      let Just (GClientReference gClientIdx) = edict^.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      deathmatchValue <- liftM (^.cvValue) deathmatchCVar
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime

      if deathmatchValue /= 0 && (gClient^.gcPers.cpSpectator) /= (gClient^.gcResp.crSpectator) && (levelTime - gClient^.gcRespawnTime) >= 5
        then
          spectatorRespawn edictRef

        else do
          -- run weapon animations if it hasn't been done by a ucmd_t
          if not (gClient^.gcWeaponThunk) && not (gClient^.gcResp.crSpectator)
            then PlayerWeapon.thinkWeapon edictRef
            else gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponThunk .= False

          edict' <- readEdictT edictRef

          if (edict'^.eDeadFlag) /= 0
            then
              -- wait for any butotn just going down
              when (levelTime > (gClient^.gcRespawnTime)) $ do
                -- in deathmatch, only wait for attach button
                let buttonMask = if deathmatchValue /= 0
                                   then Constants.buttonAttack
                                   else -1

                dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
                when ((gClient^.gcLatchedButtons) .&. buttonMask /= 0 || (deathmatchValue /= 0 && (dmFlagsValue .&. Constants.dfForceRespawn) /= 0)) $ do
                  respawn edictRef
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcLatchedButtons .= 0

            else do
              -- add player trail so monsters can follow
              when (deathmatchValue /= 0) $ do
                lastSpotRef <- PlayerTrail.lastSpot
                visible <- GameUtil.visible edictRef lastSpotRef
                when (not visible) $
                  PlayerTrail.add (edict'^.eEntityState.esOldOrigin)

              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcLatchedButtons .= 0

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
clientUserInfoChanged edictRef userInfo = do
    -- check for malformed or illegal info strings
    if not (Info.validate userInfo)
      then
        return "\\name\\badinfo\\skin\\male/grunt"

      else do
        edict <- readEdictT edictRef
        let Just (GClientReference gClientIdx) = edict^.eClient

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

        let playerNum = (edict^.eIndex) - 1

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
clientBegin edictRef = do
    edict <- readEdictT edictRef
    let gClientRef = GClientReference ((edict^.eIndex) - 1)
    modifyEdictT edictRef (\v -> v & eClient .~ Just gClientRef)

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        clientBeginDeathmatch edictRef

      else do
        -- if there is already a body waiting for us (a loadgame), just
        -- take it, otherwise spawn one from scratch
        let inUse = edict^.eInUse

        if inUse
          then do
            -- the client has cleared the client side viewangles upon
            -- connecting to the server, which is different than the
            -- state when the game is saved, so we need to compensate
            -- with deltaangles
            Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix ((edict^.eIndex) - 1)
            gameBaseGlobals.gbGame.glClients.ix ((edict^.eIndex) - 1).gcPlayerState.psPMoveState.pmsDeltaAngles .= fmap Math3D.angleToShort (gClient^.gcPlayerState.psViewAngles)
          else do
            -- a spawn point will completely reinitialize the entity
            -- except for the persistant data that was initialized at
            -- ClientConnect() time
            GameUtil.initEdict edictRef
            modifyEdictT edictRef (\v -> v & eClassName .~ "player")
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

              edict' <- readEdictT edictRef
              Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix ((edict'^.eIndex) - 1)

              writeByte Constants.svcMuzzleFlash
              writeShort (edict'^.eIndex)
              writeByte Constants.mzLogin
              multicast (edict'^.eEntityState.esOrigin) Constants.multicastPvs
              bprintf Constants.printHigh ((gClient^.gcPers.cpNetName) `B.append` " entered the game\n")

        -- make sure all view stuff is valid
        PlayerView.clientEndServerFrame edictRef

clientBeginDeathmatch :: EdictReference -> Quake ()
clientBeginDeathmatch _ = do
    io (putStrLn "PlayerClient.clientBeginDeathmatch") >> undefined -- TODO

-- Called when a player connects to a server or respawns in a deathmatch.
putClientInServer :: EdictReference -> Quake ()
putClientInServer edictRef = do
    -- find a spawn point
    -- do it before setting health back up, so farthest
    -- ranging doesn't count this client
    (spawnOrigin, spawnAngles) <- selectSpawnPoint edictRef
    edict <- readEdictT edictRef
    let gClientIdx = (edict^.eIndex) - 1

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

    modifyEdictT edictRef (\v -> v & eGroundEntity .~ Nothing
                                   & eClient       .~ Just (GClientReference gClientIdx)
                                   & eTakeDamage   .~ Constants.damageAim
                                   & eMoveType     .~ Constants.moveTypeWalk
                                   & eViewHeight   .~ 22
                                   & eInUse        .~ True
                                   & eClassName    .~ "player"
                                   & eMass         .~ 200
                                   & eSolid        .~ Constants.solidBbox
                                   & eDeadFlag     .~ Constants.deadNo
                                   & eAirFinished  .~ levelTime + 12
                                   & eClipMask     .~ Constants.maskPlayerSolid
                                   & eiModel       .~ Just "players/male/tris.md2"
                                   & ePain         .~ Just playerPain
                                   & eDie          .~ Just playerDie
                                   & eWaterLevel   .~ 0
                                   & eWaterType    .~ 0
                                   & eFlags        %~ (.&. (complement Constants.flNoKnockback))
                                   & eSvFlags      %~ (.&. (complement Constants.svfDeadMonster))
                                   & eMins         .~ V3 (-16) (-16) (-24)
                                   & eMaxs         .~ V3 16 16 32
                                   & eVelocity     .~ V3 0 0 0)

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    fov <- if deathmatchValue /= 0 && (dmFlagsValue .&. Constants.dfFixedFov) /= 0
             then
               return 90

             else do
               v <- Info.valueForKey (saved^.cpUserInfo) "fov"
               let fov = Lib.atoi v
               return $ if | fov < 1 -> 90
                           | fov > 160 -> 160
                           | otherwise -> fov

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    let Just (GItemReference weaponIdx) = saved^.cpWeapon
    Just weaponModel <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx
    gunIndex <- modelIndex (weaponModel^.giViewModel)

    -- clear entity state values
    let spawnOrigin' = let V3 a b c = spawnOrigin in V3 a b (c + 1) -- make sure off ground
        {- TODO: decide how to better setup Angles using pitch/yaw/roll
        ent.s.angles[Defines.PITCH] = 0;
        ent.s.angles[Defines.YAW] = spawn_angles[Defines.YAW];
        ent.s.angles[Defines.ROLL] = 0;
        -}
        angles = V3 0 (spawnAngles^._y) 0
      
    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
      gcPlayerState                             .= newPlayerStateT
      gcPlayerState.psPMoveState.pmsOrigin      .= fmap (truncate . (* 8)) spawnOrigin
      gcPlayerState.psFOV                       .= fromIntegral fov
      gcPlayerState.psGunIndex                  .= gunIndex
      gcPlayerState.psPMoveState.pmsDeltaAngles .= fmap Math3D.angleToShort (spawnAngles - (resp^.crCmdAngles))
      gcPlayerState.psViewAngles                .= angles
      gcVAngle                                  .= angles

    modifyEdictT edictRef (\v -> v & eEntityState.esEffects     .~ 0
                                   & eEntityState.esModelIndex  .~ 255 -- will use the skin specified model
                                   & eEntityState.esModelIndex2 .~ 255 -- custom gun model
                                   -- sknum is player num and weapon number
                                   -- weapon number will be added in changeweapon
                                   & eEntityState.esSkinNum     .~ (edict^.eIndex) - 1
                                   & eEntityState.esFrame       .~ 0
                                   & eEntityState.esOrigin      .~ spawnOrigin'
                                   & eEntityState.esOldOrigin   .~ spawnOrigin'
                                   & eEntityState.esAngles      .~ angles)

    -- spawn a spectator
    if saved^.cpSpectator
      then do
        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcChaseTarget .= Nothing
          gcResp.crSpectator .= True
          gcPlayerState.psGunIndex .= 0

        modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNoClip
                                       & eSolid .~ Constants.solidNot
                                       & eSvFlags %~ (.|. Constants.svfNoClient))

        linkEntity edictRef

      else do
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp.crSpectator .= False

        void $ GameUtil.killBox edictRef

        linkEntity edictRef

        -- force the current weapon up
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= saved^.cpWeapon
        PlayerWeapon.changeWeapon edictRef

-- Chooses a player start, deathmatch start, coop start, etc.
selectSpawnPoint :: EdictReference -> Quake (V3 Float, V3 Float)
selectSpawnPoint edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    coopValue <- liftM (^.cvValue) coopCVar
    spawnPoint <- use $ gameBaseGlobals.gbGame.glSpawnPoint

    spot <- if | deathmatchValue /= 0 -> selectDeathmatchSpawnPoint
               | coopValue /= 0 -> selectCoopSpawnPoint edictRef
               | otherwise -> do
                   -- find a single player start spot
                   spot <- findPlayerStart (BC.map toLower spawnPoint) Nothing
                   case spot of
                     Nothing -> do
                       if B.length spawnPoint == 0
                         then
                           -- there wasn't a spawnpoint without a target,
                           -- so use any
                           GameBase.gFind Nothing GameBase.findByClass "info_player_start"
                         else
                           return Nothing
                     Just _ ->
                       return spot

    case spot of
      Nothing -> do
        err <- use $ gameBaseGlobals.gbGameImport.giError
        err ("Couldn't find spawn point " `B.append` spawnPoint `B.append` "\n")
        return (V3 0 0 0, V3 0 0 0)

      Just entRef -> do
        ent <- readEdictT entRef

        let entityState = ent^.eEntityState
            origin = let V3 a b c = entityState^.esOrigin in V3 a b (c + 9)
            angles = entityState^.esAngles

        return (origin, angles)

  where findPlayerStart :: B.ByteString -> Maybe EdictReference -> Quake (Maybe EdictReference)
        findPlayerStart spawnPoint eRef = do
          es <- GameBase.gFind eRef GameBase.findByClass "info_player_start"

          case es of
            Nothing -> return Nothing
            Just ref -> do
              e <- readEdictT ref
              let targetName = e^.eTargetName

              if | B.length spawnPoint == 0 && isNothing targetName ->
                     return $ Just ref
                 | B.length spawnPoint == 0 || isNothing targetName ->
                     findPlayerStart spawnPoint es
                 | spawnPoint == BC.map toLower (fromJust targetName) ->
                     return $ Just ref
                 | otherwise ->
                     findPlayerStart spawnPoint es

selectDeathmatchSpawnPoint :: Quake (Maybe EdictReference)
selectDeathmatchSpawnPoint = do
    io (putStrLn "PlayerClient.selectDeathmatchSpawnPoint") >> undefined -- TODO

selectCoopSpawnPoint :: EdictReference -> Quake (Maybe EdictReference)
selectCoopSpawnPoint _ = do
    io (putStrLn "PlayerClient.selectCoopSpawnPoint") >> undefined -- TODO

fetchClientEntData :: EdictReference -> Quake ()
fetchClientEntData edictRef = do
    coopValue <- liftM (^.cvValue) coopCVar
    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    modifyEdictT edictRef (\v -> v & eHealth .~ gClient^.gcPers.cpHealth
                                   & eMaxHealth .~ gClient^.gcPers.cpMaxHealth
                                   & eFlags %~ (.|. (gClient^.gcPers.cpSavedFlags)))

    when (coopValue /= 0) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp.crScore .= (gClient^.gcPers.cpScore)

playerPain :: EntPain
playerPain = 
  GenericEntPain "player_pain" $ \_ _ _ _ -> return ()

playerDie :: EntDie
playerDie =
  GenericEntDie "player_die" $ \selfRef inflictorRef attackerRef damage _ -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound
        linkEntity = gameImport^.giLinkEntity

    modifyEdictT selfRef (\v -> v & eAVelocity .~ V3 0 0 0
                                  & eTakeDamage .~ Constants.damageYes
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eEntityState.esModelIndex2 .~ 0 -- remove linked weapon model
                                  & eEntityState.esAngles._x .~ 0
                                  & eEntityState.esAngles._z .~ 0
                                  & eEntityState.esSound .~ 0
                                  & eMaxs._z .~ (-8)
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster))

    self <- readEdictT selfRef
    let Just gClientRef@(GClientReference gClientIdx) = self^.eClient

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponSound .= 0


    when ((self^.eDeadFlag) == 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime

      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcRespawnTime .= levelTime + 1.0

      lookAtKiller selfRef inflictorRef attackerRef

      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState.pmsPMType .= Constants.pmDead

      clientObituary selfRef inflictorRef attackerRef
      tossClientWeapon selfRef

      deathmatchValue <- liftM (^.cvValue) deathmatchCVar
      when (deathmatchValue /= 0) $
        Cmd.helpF selfRef -- show scores

      -- clear inventory
      -- this is kind of ugly, but it's how we want to handle keys in coop
      numItems <- use $ gameBaseGlobals.gbGame.glNumItems
      coopValue <- liftM (^.cvValue) coopCVar
      itemList <- use $ gameBaseGlobals.gbItemList
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      clearInventory gClientRef gClient coopValue itemList 0 numItems

    -- remove powerups
    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
      gcQuadFrameNum .= 0
      gcInvincibleFrameNum .= 0
      gcBreatherFrameNum .= 0
      gcEnviroFrameNum .= 0

    modifyEdictT selfRef (\v -> v & eFlags %~ (.&. (complement Constants.flPowerArmor)))

    if (self^.eHealth) < (-40) -- gib
      then do
        soundIdx <- soundIndex (Just "misc/udeath.wav")
        sound (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

        GameMisc.throwClientHead selfRef damage

        modifyEdictT selfRef (\v -> v & eTakeDamage .~ Constants.damageNo)

      else -- normal death
        when ((self^.eDeadFlag) == 0) $ do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          gameBaseGlobals.gbPlayerDieIdx %= (\v -> (v + 1) `mod` 3)

          -- start a death animation
          playerDieIdx <- use $ gameBaseGlobals.gbPlayerDieIdx

          let (frame, animEnd) = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                                   then
                                     (MPlayer.frameCRDeath1 - 1, MPlayer.frameCRDeath5)
                                   else
                                     case playerDieIdx of
                                       0 -> (MPlayer.frameDeath101 - 1, MPlayer.frameDeath106)
                                       1 -> (MPlayer.frameDeath201 - 1, MPlayer.frameDeath206)
                                       _ -> (MPlayer.frameDeath301 - 1, MPlayer.frameDeath308)

          modifyEdictT selfRef (\v -> v & eEntityState.esFrame .~ frame)

          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
            gcAnimPriority .= Constants.animDeath
            gcAnimEnd .= animEnd

          r <- Lib.rand
          let soundName = "*death" `B.append` BC.pack (show ((r `mod` 4) + 1)) `B.append` ".wav"
          soundIdx <- soundIndex (Just soundName)
          sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

    modifyEdictT selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
    linkEntity selfRef

  where clearInventory :: GClientReference -> GClientT -> Float -> V.Vector GItemT -> Int -> Int -> Quake ()
        clearInventory gClientRef@(GClientReference gClientIdx) gClient coopValue itemList idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              when (coopValue /= 0 && ((itemList V.! idx)^.giFlags) .&. Constants.itKey /= 0) $
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp.crCoopRespawn.cpInventory.ix idx .= (gClient^.gcPers.cpInventory) UV.! idx

              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix idx .= 0
              clearInventory gClientRef gClient coopValue itemList (idx + 1) maxIdx

spectatorRespawn :: EdictReference -> Quake ()
spectatorRespawn _ = do
    io (putStrLn "PlayerClient.spectatorRespawn") >> undefined -- TODO

respawn :: EdictReference -> Quake ()
respawn _ = do
    io (putStrLn "PlayerClient.respawn") >> undefined -- TODO

{-
- This will be called once for each client frame, which will usually be a
- couple times for each server frame.
-}
clientThink :: EdictReference -> UserCmdT -> Quake ()
clientThink edictRef ucmd = do
    gameBaseGlobals.gbLevel.llCurrentEntity .= Just edictRef

    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime

    if intermissionTime /= 0
      then do
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState.pmsPMType .= Constants.pmFreeze
        -- can exit intermission after five seconds
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        when (levelTime > intermissionTime + 5 && (fromIntegral (ucmd^.ucButtons) .&. Constants.buttonAny /= 0)) $
          gameBaseGlobals.gbLevel.llExitIntermission .= True
      else do
        clientGlobals.cgPMPassEnt .= Just edictRef

        case gClient^.gcChaseTarget of
          Just chaseTargetRef ->
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp.crCmdAngles .= fmap (Math3D.shortToAngle . fromIntegral) (ucmd^.ucAngles)

          Nothing -> do
            readEdictT edictRef >>= \edict' -> do
              let pmtype = if | (edict'^.eMoveType) == Constants.moveTypeNoClip -> Constants.pmSpectator
                              | (edict'^.eEntityState.esModelIndex) /= 255 -> Constants.pmGib
                              | (edict'^.eDeadFlag) /= 0 -> Constants.pmDead
                              | otherwise -> Constants.pmNormal
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState.pmsPMType .= pmtype
              
            gravityValue <- liftM (^.cvValue) svGravityCVar
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState.pmsGravity .= truncate gravityValue

            Just pmoveState <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState
            edict' <- readEdictT edictRef
            gameImport <- use $ gameBaseGlobals.gbGameImport

            let pointContents = gameImport^.giPointContents
                pMove = gameImport^.giPMove
                sound = gameImport^.giSound
                soundIndex = gameImport^.giSoundIndex
                linkEntity = gameImport^.giLinkEntity

                pmoveState' = pmoveState { _pmsOrigin = fmap (truncate . (* 8)) (edict'^.eEntityState.esOrigin)
                                         , _pmsVelocity = fmap (truncate . (* 8)) (edict'^.eVelocity)
                                         }

                snapInitial = if (gClient^.gcOldPMove) == pmoveState'
                                then True
                                else False

                pm = newPMoveT { _pmState = pmoveState'
                               , _pmCmd = ucmd
                               , _pmTrace = defaultTrace
                               , _pmPointContents = pointContents
                               , _pmSnapInitial = snapInitial
                               }

            -- perform a pmove
            pm' <- pMove pm

            -- save results of pmove
            zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
              gcPlayerState.psPMoveState .= (pm'^.pmState)
              gcOldPMove .= (pm'^.pmState)
              gcResp.crCmdAngles .= fmap (Math3D.shortToAngle . fromIntegral) (ucmd^.ucAngles)

            modifyEdictT edictRef (\v -> v & eEntityState.esOrigin .~ fmap ((* 0.125) . fromIntegral) (pm'^.pmState.pmsOrigin)
                                           & eVelocity .~ fmap ((* 0.125) . fromIntegral) (pm'^.pmState.pmsVelocity)
                                           & eMins .~ (pm'^.pmMins)
                                           & eMaxs .~ (pm'^.pmMaxs))

            readEdictT edictRef >>= \edict'' -> 
              when (isJust (edict''^.eGroundEntity) && isNothing (pm'^.pmGroundEntity) && (pm'^.pmCmd.ucUpMove) >= 10 && (pm'^.pmWaterLevel) == 0) $ do
                sIdx <- soundIndex (Just "*jump1.wav")
                sound (Just edictRef) Constants.chanVoice sIdx 1 Constants.attnNorm 0
                PlayerWeapon.playerNoise edictRef (edict''^.eEntityState.esOrigin) Constants.pNoiseSelf

            modifyEdictT edictRef (\v -> v & eViewHeight .~ truncate (pm'^.pmViewHeight)
                                           & eWaterLevel .~ (pm'^.pmWaterLevel)
                                           & eWaterType .~ (pm'^.pmWaterType)
                                           & eGroundEntity .~ (pm'^.pmGroundEntity))

            when (isJust (pm'^.pmGroundEntity)) $ do
              let Just groundEntityRef = pm'^.pmGroundEntity
              groundEntity <- readEdictT groundEntityRef
              modifyEdictT edictRef (\v -> v & eGroundEntityLinkCount .~ (groundEntity^.eLinkCount))

            readEdictT edictRef >>= \edict'' ->
              if (edict''^.eDeadFlag) /= 0
                then
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psViewAngles .= V3 (-15) (gClient^.gcKillerYaw) 40 -- TODO: use ROLL PITCH YAW? -- TODO: we use gcKillerYaw hasn't changed?
                else
                  zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                    gcVAngle .= (pm'^.pmViewAngles)
                    gcPlayerState.psViewAngles .= (pm'^.pmViewAngles)

            linkEntity edictRef

            readEdictT edictRef >>= \edict'' ->
              when ((edict''^.eMoveType) /= Constants.moveTypeNoClip) $
                GameBase.touchTriggers edictRef

            -- touch other objects
            touchOtherObjects pm' 0 (pm'^.pmNumTouch)

        preuse (gameBaseGlobals.gbGame.glClients.ix gClientIdx) >>= \(Just gClient') ->
          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
            gcOldButtons .= (gClient'^.gcButtons)
            gcButtons .= fromIntegral (ucmd^.ucButtons)
            gcLatchedButtons .= (gClient'^.gcLatchedButtons) .|. (fromIntegral (ucmd^.ucButtons) .&. (complement (gClient'^.gcButtons)))

        -- save light level the player is standing on for
        -- monster sighting AI
        modifyEdictT edictRef (\v -> v & eLightLevel .~ fromIntegral (ucmd^.ucLightLevel))

        -- fire weapon from final position if needed
        preuse (gameBaseGlobals.gbGame.glClients.ix gClientIdx) >>= \(Just gClient') ->
          when ((gClient'^.gcLatchedButtons) .&. Constants.buttonAttack /= 0) $ do
            if gClient'^.gcResp.crSpectator
              then do
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcLatchedButtons .= 0

                case gClient^.gcChaseTarget of
                  Just targetRef -> do
                    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcChaseTarget .= Nothing
                    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState.pmsPMFlags %= (.&. (complement pmfNoPrediction))
                  Nothing -> GameChase.getChaseTarget edictRef
              else
                unless (gClient'^.gcWeaponThunk) $ do
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponThunk .= True
                  PlayerWeapon.thinkWeapon edictRef

        preuse (gameBaseGlobals.gbGame.glClients.ix gClientIdx) >>= \(Just gClient') ->
          when (gClient'^.gcResp.crSpectator) $ do
            io (putStrLn "PlayerClient.clientThink") >> undefined -- TODO

        -- update chase cam if being followed
        maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
        updateChaseCamera 1 maxClientsValue

  where touchOtherObjects :: PMoveT -> Int -> Int -> Quake ()
        touchOtherObjects pm idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let otherRef = (pm^.pmTouchEnts) V.! idx
                  duplicated = checkIfDuplicated pm otherRef 0 idx

              unless duplicated $ do
                other <- readEdictT otherRef

                when (isJust (other^.eTouch)) $ do
                  dummyPlane <- use $ gameBaseGlobals.gbDummyPlane
                  touch (fromJust $ other^.eTouch) otherRef edictRef dummyPlane Nothing

              touchOtherObjects pm (idx + 1) maxIdx

        checkIfDuplicated :: PMoveT -> EdictReference -> Int -> Int -> Bool
        checkIfDuplicated pm ref idx maxIdx
          | idx >= maxIdx = False
          | otherwise = let otherRef = (pm^.pmTouchEnts) V.! idx
                        in if ref == otherRef
                             then True
                             else checkIfDuplicated pm ref (idx + 1) maxIdx
        
        updateChaseCamera :: Int -> Int -> Quake ()
        updateChaseCamera idx maxIdx
          | idx > maxIdx = return ()
          | otherwise = do
              other <- readEdictT (newEdictReference idx)
              when (other^.eInUse) $ do
                let Just (GClientReference gClientIdx) = other^.eClient
                Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                when ((client^.gcChaseTarget) == Just edictRef) $
                  GameChase.updateChaseCam (newEdictReference idx)

              updateChaseCamera (idx + 1) maxIdx

defaultTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake (Maybe TraceT)
defaultTrace start mins maxs end = do
    Just edictRef <- use $ clientGlobals.cgPMPassEnt
    edict <- readEdictT edictRef
    trace <- use $ gameBaseGlobals.gbGameImport.giTrace

    if (edict^.eHealth) > 0
      then liftM Just $ trace start (Just mins) (Just maxs) end (Just edictRef) Constants.maskPlayerSolid
      else liftM Just $ trace start (Just mins) (Just maxs) end (Just edictRef) Constants.maskDeadSolid

tossClientWeapon :: EdictReference -> Quake ()
tossClientWeapon _ = do
    io (putStrLn "PlayerClient.tossClientWeapon") >> undefined -- TODO

lookAtKiller :: EdictReference -> EdictReference -> EdictReference -> Quake ()
lookAtKiller _ _ _ = do
    io (putStrLn "PlayerClient.lookAtKiller") >> undefined -- TODO

clientObituary :: EdictReference -> EdictReference -> EdictReference -> Quake ()
clientObituary _ _ _ = do
    io (putStrLn "PlayerClient.clientObituary") >> undefined -- TODO
