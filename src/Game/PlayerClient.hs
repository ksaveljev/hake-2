{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.PlayerClient where

import Control.Lens (use, (^.), ix, preuse, (.=), (+=), (-=), zoom, (%=), (&), (.~), (%~), (+~), (-~))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (toLower)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _x, _y, _z, norm)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV

import Game.PMoveT
import Game.EntityStateT
import Game.EdictT
import Game.UserCmdT
import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import Game.PMoveStateT
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
clientDisconnect :: Ref EdictT -> Quake ()
clientDisconnect edictRef = do
    edict <- readRef edictRef

    case edict^.eClient of
      Nothing ->
        return ()

      Just (Ref gClientIdx) -> do
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let bprintf = gameImport^.giBprintf
            writeByte = gameImport^.giWriteByte
            writeShort = gameImport^.giWriteShort
            multicast = gameImport^.giMulticast
            unlinkEntity = gameImport^.giUnlinkEntity
            configString = gameImport^.giConfigString

        bprintf Constants.printHigh ((gClient^.gcPers.cpNetName) `B.append` " disconnected\n")

        -- send effect
        writeByte Constants.svcMuzzleFlash
        writeShort (edict^.eIndex)
        writeByte Constants.mzLogout
        multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

        unlinkEntity edictRef

        modifyRef edictRef (\v -> v & eEntityState.esModelIndex .~ 0
                                       & eSolid .~ Constants.solidNot
                                       & eInUse .~ False
                                       & eClassName .~ "disconnected"
                                       )

        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpConnected .= False
        configString (Constants.csPlayerSkins + ((edict^.eIndex) - 1)) ""

{-
- Called when a player begins connecting to the server. The game can refuse
- entrance to a client by returning false. If the client is allowed, the
- connection process will continue and eventually get to ClientBegin()
- Changing levels will NOT cause this to be called again, but loadgames
- will. 
-}
clientConnect :: Ref EdictT -> B.ByteString -> Quake (Bool, B.ByteString)
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
            edict <- readRef edictRef

            -- they can connect
            let gClientIdx = (edict^.eIndex) - 1
                gClientRef = Ref gClientIdx

            modifyRef edictRef (\v -> v & eClient .~ Just gClientRef)

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

            modifyRef edictRef (\v -> v & eSvFlags .~ 0) -- make sure we start with known default
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
          modifyRef bodyRef (\v -> v & eClassName .~ "bodyque")

spInfoPlayerStart :: Ref EdictT -> Quake ()
spInfoPlayerStart edictRef = do
    coopValue <- liftM (^.cvValue) coopCVar

    unless (coopValue == 0) $ do
      mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

      when (mapName == "security") $ do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        -- invoke one of our gross, ugly, disgusting hacks
        modifyRef edictRef (\v -> v & eThink .~ Just spCreateCoopSpots
                                       & eNextThink .~ levelTime + Constants.frameTime)

{-
- QUAKED info_player_deathmatch (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for deathmatch games.
-}
spInfoPlayerDeathmatch :: Ref EdictT -> Quake ()
spInfoPlayerDeathmatch edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue == 0
      then GameUtil.freeEdict edictRef
      else void $ think GameMisc.spMiscTeleporterDest edictRef

{-
- QUAKED info_player_coop (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for coop games.
-}
spInfoPlayerCoop :: Ref EdictT -> Quake ()
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

          modifyRef edictRef (\v -> v & eThink .~ Just spFixCoopSpots
                                         & eNextThink .~ levelTime + Constants.frameTime)

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = return ()

spFixCoopSpots :: EntThink
spFixCoopSpots =
  GenericEntThink "SP_FixCoopSpots" $ \selfRef -> do
    fixSpots selfRef Nothing
    
  where fixSpots :: Ref EdictT -> Maybe (Ref EdictT) -> Quake Bool
        fixSpots selfRef es = do
          foundEdict <- GameBase.gFind es GameBase.findByClass "info_player_start"

          case foundEdict of
            Nothing ->
              return True

            Just edictRef -> do
              edict <- readRef edictRef

              case edict^.eTargetName of
                Nothing ->
                  fixSpots selfRef foundEdict

                Just targetName -> do
                  self <- readRef selfRef
                  let d = (self^.eEntityState.esOrigin) - (edict^.eEntityState.esOrigin)

                  if norm d < 384
                    then do
                      when (isNothing (self^.eTargetName) || BC.map toLower (fromJust $ self^.eTargetName) /= BC.map toLower targetName) $
                        modifyRef selfRef (\v -> v & eTargetName .~ (edict^.eTargetName))

                      return True

                    else
                      fixSpots selfRef foundEdict

spCreateCoopSpots :: EntThink
spCreateCoopSpots =
  GenericEntThink "SP_CreateCoopSpots" $ \selfRef -> do
    mapName <- use $ gameBaseGlobals.gbLevel.llMapName

    when (BC.map toLower mapName == "security") $ do
      spotRef <- GameUtil.spawn
      modifyRef spotRef (\v -> v & eClassName .~ "info_player_coop"
                                    & eEntityState.esOrigin .~ V3 (188 - 64) (-164) 80
                                    & eTargetName .~ Just "jail3"
                                    & eEntityState.esAngles._y .~ 90
                                    )

      spotRef' <- GameUtil.spawn
      modifyRef spotRef' (\v -> v & eClassName .~ "info_player_coop"
                                     & eEntityState.esOrigin .~ V3 (188 + 64) (-164) 80
                                     & eTargetName .~ Just "jail3"
                                     & eEntityState.esAngles._y .~ 90
                                     )

      spotRef'' <- GameUtil.spawn
      modifyRef spotRef'' (\v -> v & eClassName .~ "info_player_coop"
                                      & eEntityState.esOrigin .~ V3 (188 + 128) (-164) 80
                                      & eTargetName .~ Just "jail3"
                                      & eEntityState.esAngles._y .~ 90
                                      )

    return True

{-
- This will be called once for each server frame, before running any other
- entities in the world. 
-}
clientBeginServerFrame :: Ref EdictT -> Quake ()
clientBeginServerFrame edictRef = do
    intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime

    unless (intermissionTime /= 0) $ do
      edict <- readRef edictRef
      let Just (Ref gClientIdx) = edict^.eClient
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

          edict' <- readRef edictRef

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

initClientResp :: Ref GClientT -> Quake ()
initClientResp (Ref gClientIdx) = do
    frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
    Just pers <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp .= newClientRespawnT { _crEnterFrame = frameNum, _crCoopRespawn = pers }

{-
- This is only called when the game first initializes in single player, but
- is called after each death and level change in deathmatch. 
-}
initClientPersistant :: Ref GClientT -> Quake ()
initClientPersistant (Ref gClientIdx) = do
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
clientUserInfoChanged :: Ref EdictT -> B.ByteString -> Quake B.ByteString
clientUserInfoChanged edictRef userInfo = do
    -- check for malformed or illegal info strings
    if not (Info.validate userInfo)
      then
        return "\\name\\badinfo\\skin\\male/grunt"

      else do
        edict <- readRef edictRef
        let Just (Ref gClientIdx) = edict^.eClient

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
clientBegin :: Ref EdictT -> Quake ()
clientBegin edictRef = do
    edict <- readRef edictRef
    let gClientRef = Ref ((edict^.eIndex) - 1)
    modifyRef edictRef (\v -> v & eClient .~ Just gClientRef)

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
            modifyRef edictRef (\v -> v & eClassName .~ "player")
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

              edict' <- readRef edictRef
              Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix ((edict'^.eIndex) - 1)

              writeByte Constants.svcMuzzleFlash
              writeShort (edict'^.eIndex)
              writeByte Constants.mzLogin
              multicast (edict'^.eEntityState.esOrigin) Constants.multicastPvs
              bprintf Constants.printHigh ((gClient^.gcPers.cpNetName) `B.append` " entered the game\n")

        -- make sure all view stuff is valid
        PlayerView.clientEndServerFrame edictRef

{-
- A client has just connected to the server in deathmatch mode, so clear
- everything out before starting them. 
-}
clientBeginDeathmatch :: Ref EdictT -> Quake ()
clientBeginDeathmatch edictRef = do
    GameUtil.initEdict edictRef
    edict <- readRef edictRef

    let gClientIdx = (edict^.eIndex) - 1
    initClientResp (Ref gClientIdx) -- TODO: jake uses ent.client to get the client ref

    -- locate ent at a spawn point
    putClientInServer edictRef

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast
        bprintf = gameImport^.giBprintf

    intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime

    if intermissionTime /= 0
      then
        PlayerHud.moveClientToIntermission edictRef

      else do
        -- send effect
        writeByte Constants.svcMuzzleFlash
        writeShort (edict^.eIndex)
        writeByte Constants.mzLogin
        multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    bprintf Constants.printHigh ((gClient^.gcPers.cpNetName) `B.append` " entered the game\n")

    -- make sure all view stuff is valid
    PlayerView.clientEndServerFrame edictRef

-- Called when a player connects to a server or respawns in a deathmatch.
putClientInServer :: Ref EdictT -> Quake ()
putClientInServer edictRef = do
    -- find a spawn point
    -- do it before setting health back up, so farthest
    -- ranging doesn't count this client
    (spawnOrigin, spawnAngles) <- selectSpawnPoint edictRef
    edict <- readRef edictRef
    let gClientIdx = (edict^.eIndex) - 1

    -- deathmatch wipes most client data every spawn
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    coopValue <- liftM (^.cvValue) coopCVar

    resp <- if | deathmatchValue /= 0 -> do
                  Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                  let userInfo = gClient^.gcPers.cpUserInfo
                  initClientPersistant (Ref gClientIdx)
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
      initClientPersistant (Ref gClientIdx)

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp .= resp

    -- copy some data from the client to the entity
    fetchClientEntData edictRef

    -- clear entity values
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing
                                   & eClient       .~ Just (Ref gClientIdx)
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

    modifyRef edictRef (\v -> v & eEntityState.esEffects     .~ 0
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

        modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNoClip
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
selectSpawnPoint :: Ref EdictT -> Quake (V3 Float, V3 Float)
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
        ent <- readRef entRef

        let entityState = ent^.eEntityState
            origin = let V3 a b c = entityState^.esOrigin in V3 a b (c + 9)
            angles = entityState^.esAngles

        return (origin, angles)

  where findPlayerStart :: B.ByteString -> Maybe (Ref EdictT) -> Quake (Maybe (Ref EdictT))
        findPlayerStart spawnPoint eRef = do
          es <- GameBase.gFind eRef GameBase.findByClass "info_player_start"

          case es of
            Nothing -> return Nothing
            Just ref -> do
              e <- readRef ref
              let targetName = e^.eTargetName

              if | B.length spawnPoint == 0 && isNothing targetName ->
                     return $ Just ref
                 | B.length spawnPoint == 0 || isNothing targetName ->
                     findPlayerStart spawnPoint es
                 | spawnPoint == BC.map toLower (fromJust targetName) ->
                     return $ Just ref
                 | otherwise ->
                     findPlayerStart spawnPoint es

selectDeathmatchSpawnPoint :: Quake (Maybe (Ref EdictT))
selectDeathmatchSpawnPoint = do
    dmFlagsValue <- liftM (^.cvValue) dmFlagsCVar
    if (truncate dmFlagsValue) .&. Constants.dfSpawnFarthest /= 0
      then selectFarthestDeathmatchSpawnPoint
      else selectRandomDeathmatchSpawnPoint

selectRandomDeathmatchSpawnPoint :: Quake (Maybe (Ref EdictT))
selectRandomDeathmatchSpawnPoint = do
    undefined -- TODO

selectFarthestDeathmatchSpawnPoint :: Quake (Maybe (Ref EdictT))
selectFarthestDeathmatchSpawnPoint = do
    undefined -- TODO

selectCoopSpawnPoint :: Ref EdictT -> Quake (Maybe (Ref EdictT))
selectCoopSpawnPoint _ = do
    io (putStrLn "PlayerClient.selectCoopSpawnPoint") >> undefined -- TODO

fetchClientEntData :: Ref EdictT -> Quake ()
fetchClientEntData edictRef = do
    coopValue <- liftM (^.cvValue) coopCVar
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    modifyRef edictRef (\v -> v & eHealth .~ gClient^.gcPers.cpHealth
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

    modifyRef selfRef (\v -> v & eAVelocity .~ V3 0 0 0
                                  & eTakeDamage .~ Constants.damageYes
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eEntityState.esModelIndex2 .~ 0 -- remove linked weapon model
                                  & eEntityState.esAngles._x .~ 0
                                  & eEntityState.esAngles._z .~ 0
                                  & eEntityState.esSound .~ 0
                                  & eMaxs._z .~ (-8)
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster))

    self <- readRef selfRef
    let Just gClientRef@(Ref gClientIdx) = self^.eClient

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

    modifyRef selfRef (\v -> v & eFlags %~ (.&. (complement Constants.flPowerArmor)))

    if (self^.eHealth) < (-40) -- gib
      then do
        soundIdx <- soundIndex (Just "misc/udeath.wav")
        sound (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
        GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

        GameMisc.throwClientHead selfRef damage

        modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageNo)

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

          modifyRef selfRef (\v -> v & eEntityState.esFrame .~ frame)

          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
            gcAnimPriority .= Constants.animDeath
            gcAnimEnd .= animEnd

          r <- Lib.rand
          let soundName = "*death" `B.append` BC.pack (show ((r `mod` 4) + 1)) `B.append` ".wav"
          soundIdx <- soundIndex (Just soundName)
          sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

    modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
    linkEntity selfRef

  where clearInventory :: Ref GClientT -> GClientT -> Float -> V.Vector GItemT -> Int -> Int -> Quake ()
        clearInventory gClientRef@(Ref gClientIdx) gClient coopValue itemList idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              when (coopValue /= 0 && ((itemList V.! idx)^.giFlags) .&. Constants.itKey /= 0) $
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp.crCoopRespawn.cpInventory.ix idx .= (gClient^.gcPers.cpInventory) UV.! idx

              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix idx .= 0
              clearInventory gClientRef gClient coopValue itemList (idx + 1) maxIdx

{-
- Only called when pers.spectator changes note that resp.spectator should
- be the opposite of pers.spectator here
-}
spectatorRespawn :: Ref EdictT -> Quake ()
spectatorRespawn edictRef = do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let cprintf = gameImport^.giCprintf
        bprintf = gameImport^.giBprintf
        writeByte = gameImport^.giWriteByte
        writeString = gameImport^.giWriteString
        writeShort = gameImport^.giWriteShort
        unicast = gameImport^.giUnicast
        multicast = gameImport^.giMulticast

    -- if the user wants to become a spectator, make sure he doesn't
    -- exceed max_spectators
    done <- if gClient^.gcPers.cpSpectator
              then do
                pass <- Info.valueForKey (gClient^.gcPers.cpUserInfo) "spectator"
                actualPassword <- liftM (^.cvString) spectatorPasswordCVar

                if not (passwordOK actualPassword pass)
                  then do
                    cprintf (Just edictRef) Constants.printHigh "Spectator password incorrect.\n"
                    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpSpectator .= False
                    writeByte Constants.svcStuffText
                    writeString "spectator 0\n"
                    unicast edictRef True
                    return True

                  else do
                    -- count spectators
                    maxClientsValue <- liftM (^.cvValue) maxClientsCVar
                    numSpec <- countSpectators 1 (truncate maxClientsValue) 0

                    maxSpectatorsValue <- liftM (^.cvValue) maxSpectatorsCVar

                    if numSpec >= truncate maxSpectatorsValue
                      then do
                        cprintf (Just edictRef) Constants.printHigh "Server spectator limit is full."
                        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpSpectator .= False
                        writeByte Constants.svcStuffText
                        writeString "spectator 0\n"
                        unicast edictRef True
                        return True

                      else
                        return False

              else do
                -- he was a spectator and wants to join the game
                -- he must have the right password
                pass <- Info.valueForKey (gClient^.gcPers.cpUserInfo) "password"
                actualPassword <- liftM (^.cvString) spectatorPasswordCVar

                if not (passwordOK actualPassword pass)
                  then do
                    cprintf (Just edictRef) Constants.printHigh "Password incorrect.\n"
                    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpSpectator .= True
                    writeByte Constants.svcStuffText
                    writeString "spectator 1\n"
                    unicast edictRef True
                    return True

                  else
                    return False

    unless done $ do
      -- clear client on respawn
      zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
        gcResp.crScore .= 0
        gcPers.cpScore .= 0

      modifyRef edictRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient)))
      putClientInServer edictRef

      -- add a teleportation effect
      Just gClient' <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      unless (gClient'^.gcPers.cpSpectator) $ do
        -- send effect
        writeByte Constants.svcMuzzleFlash
        writeShort (edict^.eIndex)
        writeByte Constants.mzLogin
        multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

        -- hold in place briefly
        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcPlayerState.psPMoveState.pmsPMFlags .= pmfTimeTeleport
          gcPlayerState.psPMoveState.pmsPMTime .= 14

      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcRespawnTime .= levelTime

      if gClient'^.gcPers.cpSpectator
        then bprintf Constants.printHigh ((gClient'^.gcPers.cpNetName) `B.append` " has moved to the sidelines\n")
        else bprintf Constants.printHigh ((gClient'^.gcPers.cpNetName) `B.append` " joined the game\n")

  where countSpectators :: Int -> Int -> Int -> Quake Int
        countSpectators idx maxIdx acc
          | idx > maxIdx = return acc
          | otherwise = do
              let ref = Ref idx
              edict <- readRef ref

              if edict^.eInUse
                then do
                  let Just (Ref gClientIdx) = edict^.eClient
                  Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                  if gClient^.gcPers.cpSpectator
                    then countSpectators (idx + 1) maxIdx (acc + 1)
                    else countSpectators (idx + 1) maxIdx acc

                else
                  countSpectators (idx + 1) maxIdx acc

respawn :: Ref EdictT -> Quake ()
respawn selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    coopValue <- liftM (^.cvValue) coopCVar

    if deathmatchValue /= 0 || coopValue /= 0
      then do
        self <- readRef selfRef

        when ((self^.eMoveType) /= Constants.moveTypeNoClip) $
          copyToBodyQue selfRef

        modifyRef selfRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient)))
        putClientInServer selfRef

        -- add a teleportation effect
        modifyRef selfRef (\v -> v & eEntityState.esEvent .~ Constants.evPlayerTeleport)

        let Just (Ref gClientIdx) = self^.eClient
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        -- hold in place briefly
        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcPlayerState.psPMoveState.pmsPMFlags .= pmfTimeTeleport
          gcPlayerState.psPMoveState.pmsPMTime .= 14
          gcRespawnTime .= levelTime

      else do
        addCommandString <- use $ gameBaseGlobals.gbGameImport.giAddCommandString
        addCommandString "menu_loadgame\n"

copyToBodyQue :: Ref EdictT -> Quake ()
copyToBodyQue edictRef = do
    maxClientsValue <- liftM (^.cvValue) maxClientsCVar
    bodyQue <- use $ gameBaseGlobals.gbLevel.llBodyQue
    let idx = truncate maxClientsValue + bodyQue + 1
        bodyRef = Ref idx

    gameBaseGlobals.gbLevel.llBodyQue .= (bodyQue + 1) `mod` Constants.bodyQueueSize

    -- FIXME: send an effect on the removed body

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let unlinkEntity = gameImport^.giUnlinkEntity
        linkEntity = gameImport^.giLinkEntity

    unlinkEntity edictRef
    unlinkEntity bodyRef

    edict <- readRef edictRef

    modifyRef bodyRef (\v -> v & eEntityState .~ (edict^.eEntityState)
                                  & eEntityState.esNumber .~ idx
                                  & eSvFlags .~ (edict^.eSvFlags)
                                  & eMins .~ (edict^.eMins)
                                  & eMaxs .~ (edict^.eMaxs)
                                  & eAbsMin .~ (edict^.eAbsMin)
                                  & eAbsMax .~ (edict^.eAbsMax)
                                  & eSize .~ (edict^.eSize)
                                  & eSolid .~ (edict^.eSolid)
                                  & eClipMask .~ (edict^.eClipMask)
                                  & eOwner .~ (edict^.eOwner)
                                  & eMoveType .~ (edict^.eMoveType)
                                  & eDie .~ Just bodyDie
                                  & eTakeDamage .~ Constants.damageYes
                                  )

    linkEntity bodyRef

bodyDie :: EntDie
bodyDie =
  GenericEntDie "body_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef

    when ((self^.eHealth) < -40) $ do
      gameImport <- use $ gameBaseGlobals.gbGameImport
      let sound = gameImport^.giSound
          soundIndex = gameImport^.giSoundIndex

      soundIdx <- soundIndex (Just "misc/udeath.wav")
      sound (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

      GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
      GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
      GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
      GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

      modifyRef selfRef (\v -> v & eEntityState.esOrigin._z -~ 48)

      GameMisc.throwClientHead selfRef damage

      modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageNo)

{-
- This will be called once for each client frame, which will usually be a
- couple times for each server frame.
-}
clientThink :: Ref EdictT -> UserCmdT -> Quake ()
clientThink edictRef ucmd = do
    gameBaseGlobals.gbLevel.llCurrentEntity .= Just edictRef

    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
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
            readRef edictRef >>= \edict' -> do
              let pmtype = if | (edict'^.eMoveType) == Constants.moveTypeNoClip -> Constants.pmSpectator
                              | (edict'^.eEntityState.esModelIndex) /= 255 -> Constants.pmGib
                              | (edict'^.eDeadFlag) /= 0 -> Constants.pmDead
                              | otherwise -> Constants.pmNormal
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState.pmsPMType .= pmtype
              
            gravityValue <- liftM (^.cvValue) svGravityCVar
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState.pmsGravity .= truncate gravityValue

            Just pmoveState <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState
            edict' <- readRef edictRef
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

            modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ fmap ((* 0.125) . fromIntegral) (pm'^.pmState.pmsOrigin)
                                           & eVelocity .~ fmap ((* 0.125) . fromIntegral) (pm'^.pmState.pmsVelocity)
                                           & eMins .~ (pm'^.pmMins)
                                           & eMaxs .~ (pm'^.pmMaxs))

            readRef edictRef >>= \edict'' -> 
              when (isJust (edict''^.eGroundEntity) && isNothing (pm'^.pmGroundEntity) && (pm'^.pmCmd.ucUpMove) >= 10 && (pm'^.pmWaterLevel) == 0) $ do
                sIdx <- soundIndex (Just "*jump1.wav")
                sound (Just edictRef) Constants.chanVoice sIdx 1 Constants.attnNorm 0
                PlayerWeapon.playerNoise edictRef (edict''^.eEntityState.esOrigin) Constants.pNoiseSelf

            modifyRef edictRef (\v -> v & eViewHeight .~ truncate (pm'^.pmViewHeight)
                                           & eWaterLevel .~ (pm'^.pmWaterLevel)
                                           & eWaterType .~ (pm'^.pmWaterType)
                                           & eGroundEntity .~ (pm'^.pmGroundEntity))

            when (isJust (pm'^.pmGroundEntity)) $ do
              let Just groundEntityRef = pm'^.pmGroundEntity
              groundEntity <- readRef groundEntityRef
              modifyRef edictRef (\v -> v & eGroundEntityLinkCount .~ (groundEntity^.eLinkCount))

            readRef edictRef >>= \edict'' ->
              if (edict''^.eDeadFlag) /= 0
                then
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psViewAngles .= V3 (-15) (gClient^.gcKillerYaw) 40 -- TODO: use ROLL PITCH YAW? -- TODO: we use gcKillerYaw hasn't changed?
                else
                  zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                    gcVAngle .= (pm'^.pmViewAngles)
                    gcPlayerState.psViewAngles .= (pm'^.pmViewAngles)

            linkEntity edictRef

            readRef edictRef >>= \edict'' ->
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
        modifyRef edictRef (\v -> v & eLightLevel .~ fromIntegral (ucmd^.ucLightLevel))

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
                other <- readRef otherRef

                when (isJust (other^.eTouch)) $ do
                  dummyPlane <- use $ gameBaseGlobals.gbDummyPlane
                  touch (fromJust $ other^.eTouch) otherRef edictRef dummyPlane Nothing

              touchOtherObjects pm (idx + 1) maxIdx

        checkIfDuplicated :: PMoveT -> Ref EdictT -> Int -> Int -> Bool
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
              other <- readRef (Ref idx)
              when (other^.eInUse) $ do
                let Just (Ref gClientIdx) = other^.eClient
                Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                when ((client^.gcChaseTarget) == Just edictRef) $
                  GameChase.updateChaseCam (Ref idx)

              updateChaseCamera (idx + 1) maxIdx

defaultTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake (Maybe TraceT)
defaultTrace start mins maxs end = do
    Just edictRef <- use $ clientGlobals.cgPMPassEnt
    edict <- readRef edictRef
    trace <- use $ gameBaseGlobals.gbGameImport.giTrace

    if (edict^.eHealth) > 0
      then liftM Just $ trace start (Just mins) (Just maxs) end (Just edictRef) Constants.maskPlayerSolid
      else liftM Just $ trace start (Just mins) (Just maxs) end (Just edictRef) Constants.maskDeadSolid

-- Drop items and weapons in deathmatch games.
tossClientWeapon :: Ref EdictT -> Quake ()
tossClientWeapon selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    unless (deathmatchValue == 0) $ do
      self <- readRef selfRef
      let Just (Ref gClientIdx) = self^.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

      itemRef <- case gClient^.gcPers.cpWeapon of
                   Nothing ->
                     return Nothing

                   Just (GItemReference gItemIdx) -> do
                     Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

                     return $ if | (gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex) == 0 -> Nothing
                                 | (gItem^.giPickupName) == Just "Blaster" -> Nothing
                                 | otherwise -> gClient^.gcPers.cpWeapon

      frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
      dmFlagsValue <- liftM (^.cvValue) dmFlagsCVar

      let quad = if (truncate dmFlagsValue) .&. Constants.dfQuadDrop == 0
                   then False
                   else (gClient^.gcQuadFrameNum) > fromIntegral (frameNum + 10)
          spread = if isJust itemRef && quad
                     then 22.5
                     else 0.0

      case itemRef of
        Nothing ->
          return ()

        Just gItemRef -> do
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVAngle._y -= spread -- IMPROVE: use Constants.yaw instead of _y
          dropRef <- GameItems.dropItem selfRef gItemRef
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVAngle._y += spread -- IMPROVE: use Constants.yaw instead of _y
          modifyRef dropRef (\v -> v & eSpawnFlags .~ Constants.droppedPlayerItem)

      when quad $ do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVAngle._y += spread -- IMPROVE: use Constants.yaw instead of _y
        Just quadRef <- GameItems.findItemByClassname "item_quad"
        dropRef <- GameItems.dropItem selfRef quadRef
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVAngle._y -= spread -- IMPROVE: use Constants.yaw instead of _y
        modifyRef dropRef (\v -> v & eSpawnFlags %~ (.|. Constants.droppedPlayerItem)
                                      & eTouch .~ Just GameItems.touchItem
                                      & eNextThink .~ levelTime + ((gClient^.gcQuadFrameNum) - fromIntegral frameNum) * Constants.frameTime
                                      & eThink .~ Just GameUtil.freeEdictA
                                      )

-- Changes the camera view to look at the killer.
lookAtKiller :: Ref EdictT -> Ref EdictT -> Ref EdictT -> Quake ()
lookAtKiller selfRef inflictorRef attackerRef = do
    self <- readRef selfRef

    let worldRef = Ref 0
        Just (Ref gClientIdx) = self^.eClient

    -- TODO: jake2 checks attacker and inflictor for != null, do we need that?
    mDir <- if | attackerRef /= worldRef && attackerRef /= selfRef -> do
                   attacker <- readRef attackerRef
                   return $ Just ((attacker^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin))

               | inflictorRef /= worldRef && inflictorRef /= selfRef -> do
                   inflictor <- readRef inflictorRef
                   return $ Just ((inflictor^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin))

               | otherwise -> do
                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcKillerYaw .= (self^.eEntityState.esAngles._y) -- IMPROVE: use Constants.yaw instead of _y
                   return Nothing

    case mDir of
      Nothing ->
        return ()

      Just dir -> do
        let killerYaw = if (dir^._x) /= 0
                          then 180 / pi * (atan2 (dir^._y) (dir^._x))
                          else if | (dir^._y) > 0 -> 90
                                  | (dir^._y) < 0 -> -90
                                  | otherwise -> 0

            killerYaw' = if killerYaw < 0 then killerYaw + 360 else killerYaw

        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcKillerYaw .= killerYaw'

clientObituary :: Ref EdictT -> Ref EdictT -> Ref EdictT -> Quake ()
clientObituary selfRef inflictorRef attackerRef = do
    self <- readRef selfRef
    attacker <- readRef attackerRef

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    coopValue <- liftM (^.cvValue) coopCVar

    when (coopValue /= 0 && isJust (attacker^.eClient)) $
      gameBaseGlobals.gbMeansOfDeath %= (.|. Constants.modFriendlyFire)

    done <- if deathmatchValue /= 0 || coopValue /= 0
              then do
                meansOfDeath <- use $ gameBaseGlobals.gbMeansOfDeath
                let ff = meansOfDeath .&. Constants.modFriendlyFire /= 0
                    mod' = meansOfDeath .&. (complement Constants.modFriendlyFire)

                let message = if | mod' == Constants.modSuicide -> Just "suicides"
                                 | mod' == Constants.modFalling -> Just "cratered"
                                 | mod' == Constants.modCrush -> Just "was squished"
                                 | mod' == Constants.modWater -> Just "sank like a rock"
                                 | mod' == Constants.modSlime -> Just "melted"
                                 | mod' == Constants.modLava -> Just "does a back flip into the lava"
                                 | mod' == Constants.modExplosive || mod' == Constants.modBarrel -> Just "blew up"
                                 | mod' == Constants.modExit -> Just "found a way out"
                                 | mod' == Constants.modTargetLaser -> Just "saw the light"
                                 | mod' == Constants.modTargetBlaster -> Just "got blasted"
                                 | mod' `elem` [ Constants.modBomb, Constants.modSplash, Constants.modTriggerHurt ] -> Just "was in the wrong place"
                                 | otherwise -> Nothing

                message' <- if attackerRef == selfRef
                              then do
                                neutral <- isNeutral selfRef
                                female <- isFemale selfRef

                                if | mod' == Constants.modHeldGrenade -> return (Just "tried to put the pin back in")
                                   | mod' == Constants.modHgSplash || mod' == Constants.modGSplash ->
                                       return $ Just $ if | neutral -> "tripped on its own grenade"
                                                          | female -> "tripped on her own grenade"
                                                          | otherwise -> "tripped on his own grenade"
                                   | mod' == Constants.modRSplash ->
                                       return $ Just $ if | neutral -> "blew itself up"
                                                          | female -> "blew herself up"
                                                          | otherwise -> "blew himself up"
                                   | mod' == Constants.modBFGBlast -> return (Just "should have used a smaller gun")
                                   | otherwise ->
                                       return $ Just $ if | neutral -> "killed itself"
                                                          | female -> "killed herself"
                                                          | otherwise -> "killed himself"

                              else
                                return message

                case message' of
                  Just msg -> do
                    let Just (Ref gClientIdx) = self^.eClient
                    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                    bprintf <- use $ gameBaseGlobals.gbGameImport.giBprintf
                    bprintf Constants.printMedium ((gClient^.gcPers.cpNetName) `B.append` " " `B.append` msg `B.append` ".\n")
                    
                    when (deathmatchValue /= 0) $
                      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp.crScore -= 1

                    modifyRef selfRef (\v -> v & eEnemy .~ Nothing)
                    return True

                  Nothing -> do
                    modifyRef selfRef (\v -> v & eEnemy .~ Just attackerRef)

                    case attacker^.eClient of
                      Just _ -> do
                        let (message'', message2) = if | mod' == Constants.modBlaster -> (Just "was blasted by", "")
                                                       | mod' == Constants.modShotgun -> (Just "was gunned down by", "")
                                                       | mod' == Constants.modSshotgun -> (Just "was blown away by", "'s super shotgun")
                                                       | mod' == Constants.modMachinegun -> (Just "was machinegunned by", "")
                                                       | mod' == Constants.modChaingun -> (Just "was cut in half by", "'s chaingun")
                                                       | mod' == Constants.modGrenade -> (Just "was popped by", "'s grenade")
                                                       | mod' == Constants.modGSplash -> (Just "was shredded by", "'s shrapnel")
                                                       | mod' == Constants.modRocket -> (Just "ate", "'s rocket")
                                                       | mod' == Constants.modRSplash -> (Just "almost dodged", "'s rocket")
                                                       | mod' == Constants.modHyperblaster -> (Just "was melted by", "'s hyperblaster")
                                                       | mod' == Constants.modRailgun -> (Just "was railed by", "")
                                                       | mod' == Constants.modBFGLaser -> (Just "saw the pretty lights from", "'s BFG")
                                                       | mod' == Constants.modBFGBlast -> (Just "was disintegrated by", "'s BFG blast")
                                                       | mod' == Constants.modBFGEffect -> (Just "couldn't hide from", "'s BFG")
                                                       | mod' == Constants.modHandgrenade -> (Just "caught", "'s handgrenade")
                                                       | mod' == Constants.modHgSplash -> (Just "didn't see", "'s handgrenade")
                                                       | mod' == Constants.modHeldGrenade -> (Just "feels", "'s pain")
                                                       | mod' == Constants.modTelefrag -> (Just "tried to invade", "'s personal space")
                                                       | otherwise -> (Nothing, "")

                        case message'' of
                          Nothing ->
                            return False

                          Just msg -> do
                            let Just (Ref gClientIdx) = self^.eClient
                            Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
                            let Just (Ref attackerClientIdx) = attacker^.eClient
                            Just attackerClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix attackerClientIdx

                            bprintf <- use $ gameBaseGlobals.gbGameImport.giBprintf
                            bprintf Constants.printMedium ((gClient^.gcPers.cpNetName) `B.append` " " `B.append` msg `B.append` " " `B.append` (attackerClient^.gcPers.cpNetName) `B.append` " " `B.append` message2 `B.append` "\n")

                            when (deathmatchValue /= 0) $
                              if ff
                                then gameBaseGlobals.gbGame.glClients.ix attackerClientIdx.gcResp.crScore -= 1
                                else gameBaseGlobals.gbGame.glClients.ix attackerClientIdx.gcResp.crScore += 1

                            return True

                      Nothing ->
                        return False

              else
                return False

    unless done $ do
      let Just (Ref gClientIdx) = self^.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

      bprintf <- use $ gameBaseGlobals.gbGameImport.giBprintf
      bprintf Constants.printMedium ((gClient^.gcPers.cpNetName) `B.append` " died.\n")

      when (deathmatchValue /= 0) $
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp.crScore -= 1

isNeutral :: Ref EdictT -> Quake Bool
isNeutral _ = do
    io (putStrLn "PlayerClient.isNeutral") >> undefined -- TODO

isFemale :: Ref EdictT -> Quake Bool
isFemale _ = do
    io (putStrLn "PlayerClient.isFemale") >> undefined -- TODO
