module Game.PlayerClient
    ( clientBegin
    , clientBeginServerFrame
    , clientConnect
    , clientThink
    , clientUserInfoChanged
    , initBodyQue
    , saveClientData
    , spInfoPlayerCoop
    , spInfoPlayerDeathmatch
    , spInfoPlayerIntermission
    , spInfoPlayerStart
    ) where

import           Control.Lens           (use, ix, (^.), (.=), (%=), (&), (.~), (+~), (%~))
import           Control.Monad          (when, unless, void)
import           Data.Bits              (complement, (.&.), (.|.))
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import           Data.Char              (toLower)
import           Data.Maybe             (fromMaybe, isNothing, isJust)
import qualified Data.Vector            as V
import           Linear                 (V3(..), _y, _z)

import qualified Constants
import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameChase         as GameChase
import qualified Game.GameItems         as GameItems
import           Game.GameLocalsT
import qualified Game.GameMisc          as GameMisc
import qualified Game.GameSVCmds        as GameSVCmds
import qualified Game.GameUtil          as GameUtil
import           Game.GClientT
import           Game.GItemT
import qualified Game.Info              as Info
import           Game.LevelLocalsT
import qualified Game.PlayerHud         as PlayerHud
import           Game.PlayerStateT
import qualified Game.PlayerTrail       as PlayerTrail
import qualified Game.PlayerView        as PlayerView
import qualified Game.PlayerWeapon      as PlayerWeapon
import           Game.PMoveStateT
import           Game.PMoveT
import           Game.UserCmdT
import qualified QCommon.Com            as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib               as Lib
import qualified Util.Math3D            as Math3D

import {-# SOURCE #-} qualified Game.GameBase as GameBase

saveClientData :: Quake ()
saveClientData = do
    maxClients <- use (gameBaseGlobals.gbGame.glMaxClients)
    coop <- coopCVar
    mapM_ (\idx -> updateClient coop idx =<< readRef (Ref (idx + 1))) [0..maxClients-1]

updateClient :: CVarT -> Int -> EdictT -> Quake ()
updateClient coop idx edict
    | edict^.eInUse = do
        score <- updateClientScore <$> readRef (Ref idx)
        modifyRef (Ref idx) (\v -> v & gcPers.cpHealth .~ (edict^.eHealth)
                                     & gcPers.cpMaxHealth .~ (edict^.eMaxHealth)
                                     & gcPers.cpSavedFlags .~ (edict^.eFlags) .&. (Constants.flGodMode .|. Constants.flNoTarget .|. Constants.flPowerArmor)
                                     & gcPers.cpScore .~ score)
    | otherwise = return ()
  where
    updateClientScore client
        | (coop^.cvValue) /= 0 = client^.gcResp.crScore
        | otherwise = client^.gcPers.cpScore

spInfoPlayerCoop :: Ref EdictT -> Quake ()
spInfoPlayerCoop edictRef = do
    coop <- fmap (^.cvValue) coopCVar
    doInfoPlayerCoop coop
  where
    names = [ "jail2", "jail4", "mine1", "mine2", "mine3", "mine4" , "lab"
            , "boss1", "fact3", "biggun", "space", "command", "power2", "strike" ]
    doInfoPlayerCoop coop
        | coop == 0 = GameUtil.freeEdict edictRef
        | otherwise = do
            mapName <- fmap (BC.map toLower) (use (gameBaseGlobals.gbLevel.llMapName))
            when (any (== mapName) names) $ do
                -- invoke one of our gross, ugly, disgusting hacks
                levelTime <- use (gameBaseGlobals.gbLevel.llTime)
                modifyRef edictRef (\v -> v & eThink .~ Just spFixCoopSpots
                                            & eNextThink .~ levelTime + Constants.frameTime)

spFixCoopSpots :: EntThink
spFixCoopSpots = error "PlayerClient.spFixCoopSpots" -- TODO

spInfoPlayerDeathmatch :: Ref EdictT -> Quake ()
spInfoPlayerDeathmatch edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawn deathmatch
  where
    doSpawn deathmatch
        | deathmatch == 0 = GameUtil.freeEdict edictRef
        | otherwise = void (entThink GameMisc.spMiscTeleporterDest edictRef)

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = return ()

spInfoPlayerStart :: Ref EdictT -> Quake ()
spInfoPlayerStart edictRef = do
    coop <- fmap (^.cvValue) coopCVar
    unless (coop == 0) $ do
        mapName <- fmap (BC.map toLower) (use (gameBaseGlobals.gbLevel.llMapName))
        when (mapName == "security") $ do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            -- invoke one of our gross, ugly, disgusting hacks
            modifyRef edictRef (\v -> v & eThink .~ Just spCreateCoopSpots
                                        & eNextThink .~ levelTime + Constants.frameTime)

spCreateCoopSpots :: EntThink
spCreateCoopSpots = EntThink "SP_CreateCoopSpots" $ \selfRef -> do
    mapName <- use (gameBaseGlobals.gbLevel.llMapName)
    when (BC.map toLower mapName == "security") $ do
        spotRef1 <- GameUtil.spawn
        modifyRef spotRef1 (\v -> v & eClassName .~ "info_player_coop"
                                    & eEntityState.esOrigin .~ V3 (188 - 64) (-164) 80
                                    & eTargetName .~ Just "jail3"
                                    & eEntityState.esAngles._y .~ 90)
        spotRef2 <- GameUtil.spawn
        modifyRef spotRef2 (\v -> v & eClassName .~ "info_player_coop"
                                    & eEntityState.esOrigin .~ V3 (188 + 64) (-164) 80
                                    & eTargetName .~ Just "jail3"
                                    & eEntityState.esAngles._y .~ 90)
        spotRef3 <- GameUtil.spawn
        modifyRef spotRef3 (\v -> v & eClassName .~ "info_player_coop"
                                    & eEntityState.esOrigin .~ V3 (188 + 128) (-164) 80
                                    & eTargetName .~ Just "jail3"
                                    & eEntityState.esAngles._y .~ 90)
    return True

clientBeginServerFrame :: Ref EdictT -> Quake ()
clientBeginServerFrame edictRef = do
    intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
    unless (intermissionTime /= 0) $ do
        let (Ref tmpIdx) = edictRef
        io (putStrLn ("tmpIdx = " ++ show tmpIdx))
        edict <- readRef edictRef
        maybe clientError doBeginServerFrame (edict^.eClient)
  where
    clientError = Com.fatalError "PlayerClient.clientBeginServerFrame edict^.eClient is Nothing"
    doBeginServerFrame gClientRef = do
        gClient <- readRef gClientRef
        deathmatch <- fmap (^.cvValue) deathmatchCVar
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        proceedServerFrame gClientRef gClient deathmatch levelTime
    proceedServerFrame gClientRef gClient deathmatch levelTime
        | deathmatch /= 0 && (gClient^.gcPers.cpSpectator) /= (gClient^.gcResp.crSpectator) && (levelTime - gClient^.gcRespawnTime) >= 5 =
            spectatorRespawn edictRef
        | otherwise = do
            runWeaponAnimations gClientRef gClient
            edict <- readRef edictRef
            serverFrame edict gClientRef gClient deathmatch levelTime
    serverFrame edict gClientRef gClient deathmatch levelTime
        | (edict^.eDeadFlag) /= 0 =
            -- wait for any button just going down
            when (levelTime > (gClient^.gcRespawnTime)) $ do
                -- in deathmatch, only wait for attach button
                let buttonMask | deathmatch /= 0 = Constants.buttonAttack
                               | otherwise       = -1
                dmFlags <- fmap (truncate . (^.cvValue)) dmFlagsCVar
                when ((gClient^.gcLatchedButtons) .&. buttonMask /= 0 || (deathmatch /= 0 && (dmFlags .&. Constants.dfForceRespawn) /= 0)) $ do
                    respawn edictRef
                    modifyRef gClientRef (\v -> v & gcLatchedButtons .~ 0)
        | otherwise = do
            -- add player trail so monsters can follow
            when (deathmatch /= 0) $ do
                lastSpotRef <- PlayerTrail.lastSpot
                visible <- GameUtil.visible edictRef lastSpotRef
                unless visible $
                    PlayerTrail.add (edict^.eEntityState.esOldOrigin)
            modifyRef gClientRef (\v -> v & gcLatchedButtons .~ 0)
    -- run weapon animations if it hasn't been done by a ucmd_t
    runWeaponAnimations gClientRef gClient
        | not (gClient^.gcWeaponThunk) && not (gClient^.gcResp.crSpectator) = PlayerWeapon.thinkWeapon edictRef
        | otherwise = modifyRef gClientRef (\v -> v & gcWeaponThunk .~ False)

clientConnect :: Ref EdictT -> B.ByteString -> Quake (Bool, B.ByteString)
clientConnect edictRef userInfo = do
    value <- Info.valueForKey userInfo "ip"
    banned <- GameSVCmds.filterPacket value
    checkBanned banned
  where
    checkBanned banned
        | banned = do
            userInfo' <- Info.setValueForKey userInfo "rejmsg" "Banned."
            return (False, userInfo')
        | otherwise = do
            value <- Info.valueForKey userInfo "spectator"
            deathmatch <- deathmatchCVar
            (done, userInfo') <- checkSpectator value deathmatch
            doClientConnect edictRef userInfo' done
    checkSpectator value deathmatch
        | (deathmatch^.cvValue) /= 0 && not (B.null value) && value /= "0" =
            error "PlayerClient.clientConnect" -- TODO
        | otherwise = do
            password <- Info.valueForKey userInfo "password"
            actualPassword <- fmap (^.cvString) spectatorPasswordCVar
            verifySpectatorPassword password actualPassword
    verifySpectatorPassword password actualPassword
        | not (passwordOK actualPassword password) = do
            userInfo' <- Info.setValueForKey userInfo "rejmsg" "Password required or incorrect."
            return (True, userInfo')
        | otherwise = return (False, userInfo)

doClientConnect :: Ref EdictT -> B.ByteString -> Bool -> Quake (Bool, B.ByteString)
doClientConnect _ userInfo True = return (False, userInfo)
doClientConnect edictRef@(Ref idx) userInfo False = do
    edict <- readRef edictRef
    modifyRef edictRef (\v -> v & eClient .~ Just gClientRef)
    gClient <- readRef gClientRef
    checkEdictInUse gClient (edict^.eInUse)
    userInfo' <- clientUserInfoChanged edictRef userInfo
    maxClients <- use (gameBaseGlobals.gbGame.glMaxClients)
    checkMaxClients gClient maxClients
    modifyRef edictRef (\v -> v & eSvFlags .~ 0)
    modifyRef gClientRef (\v -> v & gcPers.cpConnected .~ True)
    return (True, userInfo')
  where
    gClientRef = Ref (idx - 1)
    checkEdictInUse _ True = return ()
    checkEdictInUse gClient False = do
        initClientResp gClientRef
        autoSaved <- use (gameBaseGlobals.gbGame.glAutosaved)
        checkClientPersistant autoSaved (gClient^.gcPers.cpWeapon)
    checkClientPersistant True (Just _) = return ()
    checkClientPersistant _ _ = initClientPersistant gClientRef
    checkMaxClients gClient maxClients
        | maxClients > 1 = do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf ((gClient^.gcPers.cpNetName) `B.append` " connected\n")
        | otherwise = return ()

passwordOK :: B.ByteString -> B.ByteString -> Bool
passwordOK p1 p2
    | B.length p1 > 0 && p1 /= "none" && p1 /= p2 = False
    | otherwise = True

initClientPersistant :: Ref GClientT -> Quake ()
initClientPersistant gClientRef = do
    modifyRef gClientRef (\v -> v & gcPers .~ newClientPersistantT)
    itemRef <- GameItems.findItem "Blaster"
    maybe itemRefError doInitClientPersistant itemRef
  where
    itemRefError = Com.fatalError "PlayerClient.initClientPersistant itemRef is Nothing"
    doInitClientPersistant itemRef@(Ref itemIdx) =
        modifyRef gClientRef (\v -> v & gcPers.cpSelectedItem .~ itemIdx
                                      & gcPers.cpInventory.ix itemIdx .~ 1
                                      & gcPers.cpWeapon .~ Just itemRef
                                      & gcPers.cpHealth .~ 100
                                      & gcPers.cpMaxHealth .~ 100
                                      & gcPers.cpMaxBullets .~ 200
                                      & gcPers.cpMaxShells .~ 100
                                      & gcPers.cpMaxRockets .~ 50
                                      & gcPers.cpMaxGrenades .~ 50
                                      & gcPers.cpMaxCells .~ 200
                                      & gcPers.cpMaxSlugs .~ 50
                                      & gcPers.cpConnected .~ True)

clientUserInfoChanged :: Ref EdictT -> B.ByteString -> Quake B.ByteString
clientUserInfoChanged edictRef userInfo
    | not (Info.validate userInfo) = return badInfoString
    | otherwise = do
        edict <- readRef edictRef
        maybe gClientRefError (proceedClientUserInfoChanged edict) (edict^.eClient)
  where
    gClientRefError = do
        Com.fatalError "PlayerClient.clientUserInfoChanged edict^.eClient is Nothing"
        return badInfoString
    badInfoString = "\\name\\badinfo\\skin\\male/grunt"
    proceedClientUserInfoChanged edict gClientRef = do
        name <- Info.valueForKey userInfo "name"
        deathmatch <- deathmatchCVar
        modifyRef gClientRef (\v -> v & gcPers.cpNetName .~ name)
        checkSpectator gClientRef deathmatch
        setSkin name ((edict^.eIndex) - 1)
        setFOV gClientRef deathmatch =<< fmap (truncate . (^.cvValue)) dmFlagsCVar
        setHandedness gClientRef
        modifyRef gClientRef (\v -> v & gcPers.cpUserInfo .~ userInfo)
        return userInfo
    checkSpectator gClientRef deathmatch = do
        str <- Info.valueForKey userInfo "spectator"
        modifyRef gClientRef (\v -> v & gcPers.cpSpectator .~ ((deathmatch^.cvValue) /= 0 && str /= "0"))
    setSkin name playerNum = do
        skin <- Info.valueForKey userInfo "skin"
        configString <- use (gameBaseGlobals.gbGameImport.giConfigString)
        configString (Constants.csPlayerSkins + playerNum) (B.concat [name, "\\", skin])
    setFOV gClientRef deathmatch dmFlags
        | (deathmatch^.cvValue) /= 0 && (dmFlags .&. Constants.dfFixedFov) /= 0 =
            modifyRef gClientRef (\v -> v & gcPlayerState.psFOV .~ 90)
        | otherwise = do
            fov <- Info.valueForKey userInfo "fov"
            modifyRef gClientRef (\v -> v & gcPlayerState.psFOV .~ fromIntegral (calcFov (Lib.atoi fov)))
    calcFov fov | fov < 1 = 90
                | fov > 160 = 160
                | otherwise = fov
    setHandedness gClientRef = do
        hand <- Info.valueForKey userInfo "hand"
        when (B.length hand > 0) $
            modifyRef gClientRef (\v -> v & gcPers.cpHand .~ Lib.atoi hand)

initClientResp :: Ref GClientT -> Quake ()
initClientResp gClientRef = do
    frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
    gClient <- readRef gClientRef
    modifyRef gClientRef (\v -> v & gcResp .~ (newClientRespawnT & crEnterFrame .~ frameNum
                                                                 & crCoopRespawn .~ (gClient^.gcPers)))

clientBegin :: Ref EdictT -> Quake ()
clientBegin edictRef = do
    edict <- readRef edictRef
    modifyRef edictRef (\v -> v & eClient .~ Just (Ref ((edict^.eIndex) - 1)))
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doClientBegin edict (Ref ((edict^.eIndex) - 1)) deathmatch
  where
    doClientBegin edict gClientRef deathmatch
        | deathmatch /= 0 = clientBeginDeathmatch edictRef
        | otherwise = do
            -- if there is already a body waiting for us (a loadgame), just
            -- take it, otherwise spawn one from scratch
            if (edict^.eInUse)
                then do
                    -- the client has cleared the client side viewangles upon
                    -- connecting to the server, which is different than the
                    -- state when the game is saved, so we need to compensate
                    -- with deltaangles
                    gClient <- readRef gClientRef
                    modifyRef gClientRef (\v -> v & gcPlayerState.psPMoveState.pmsDeltaAngles .~ fmap Math3D.angleToShort (gClient^.gcPlayerState.psViewAngles))
                else do
                    -- a spawn point will completely reinitialize the entity
                    -- except for the persistant data that was initialized at
                    -- ClientConnect() time
                    GameUtil.initEdict edictRef
                    modifyRef edictRef (\v -> v & eClassName .~ "player")
                    initClientResp gClientRef
                    putClientInServer edictRef
            intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
            if intermissionTime /= 0
                then
                    PlayerHud.moveClientToIntermission edictRef
                else do
                    maxClients <- use (gameBaseGlobals.gbGame.glMaxClients)
                    when (maxClients > 1) $ do
                        gameImport <- use (gameBaseGlobals.gbGameImport)
                        updatedEdict <- readRef edictRef
                        gClient <- readRef gClientRef
                        (gameImport^.giWriteByte) Constants.svcMuzzleFlash
                        (gameImport^.giWriteShort) (updatedEdict^.eIndex)
                        (gameImport^.giWriteByte) Constants.mzLogin
                        (gameImport^.giMulticast) (updatedEdict^.eEntityState.esOrigin) Constants.multicastPvs
                        (gameImport^.giBprintf) Constants.printHigh ((gClient^.gcPers.cpNetName) `B.append` " entered the game\n")
            -- make sure all view stuff is valid
            PlayerView.clientEndServerFrame edictRef

clientThink :: Ref EdictT -> UserCmdT -> Quake ()
clientThink edictRef ucmd = do
    gameBaseGlobals.gbLevel.llCurrentEntity .= Just edictRef
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    gClient <- readRef gClientRef
    intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
    doClientThink intermissionTime gClientRef
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerClient.clientThink edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    doClientThink intermissionTime gClientRef
        | intermissionTime /= 0 = do
            modifyRef gClientRef (\v -> v & gcPlayerState.psPMoveState.pmsPMType .~ Constants.pmFreeze)
            -- can exit intermission after five seconds
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            when (levelTime > intermissionTime + 5 && (fromIntegral (ucmd^.ucButtons) .&. Constants.buttonAny /= 0)) $
                gameBaseGlobals.gbLevel.llExitIntermission .= True
        | otherwise = do
            gClient <- readRef gClientRef
            clientGlobals.cgPMPassEnt .= Just edictRef
            maybe (noChaseTarget gClientRef) (setCmdAngles gClientRef) (gClient^.gcChaseTarget)
            updateButtons gClientRef
            -- save light level the player is standing on for monster sighting AI
            modifyRef edictRef (\v -> v & eLightLevel .~ fromIntegral (ucmd^.ucLightLevel))
            -- fire weapon from final position if needed
            fireWeaponIfNeeded gClientRef
            spectatorThink gClientRef
            -- update chase cam if being followed
            maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
            updateChaseCamera 1 maxClients
    setCmdAngles gClientRef _ =
        modifyRef gClientRef (\v -> v & gcResp.crCmdAngles .~ fmap (Math3D.shortToAngle . fromIntegral) (ucmd^.ucAngles))
    noChaseTarget gClientRef = do
        updatePMTypeAndGravity gClientRef =<< readRef edictRef
        gameImport <- use (gameBaseGlobals.gbGameImport)
        edict <- readRef edictRef
        gClient <- readRef gClientRef
        let pointContents = gameImport^.giPointContents
            pMove = gameImport^.giPMove
            linkEntity = gameImport^.giLinkEntity
            pmoveState = (gClient^.gcPlayerState.psPMoveState) & pmsOrigin .~ fmap (truncate . (* 8)) (edict^.eEntityState.esOrigin)
                                                               & pmsVelocity .~ fmap (truncate . (* 8)) (edict^.eVelocity)
            snapInitial = (gClient^.gcOldPMove) == pmoveState
        -- perform a pmove
        pm <- pMove (newPMoveT & pmState         .~ pmoveState
                               & pmCmd           .~ ucmd
                               & pmTrace         .~ defaultTrace
                               & pmPointContents .~ pointContents
                               & pmSnapInitial   .~ snapInitial)
        -- save results of pmove
        modifyRef gClientRef (\v -> v & gcPlayerState.psPMoveState .~ (pm^.pmState)
                                      & gcOldPMove .~ (pm^.pmState)
                                      & gcResp.crCmdAngles .~ fmap (Math3D.shortToAngle . fromIntegral) (ucmd^.ucAngles))
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ fmap ((* 0.125) . fromIntegral) (pm^.pmState.pmsOrigin)
                                    & eVelocity .~ fmap ((* 0.125) . fromIntegral) (pm^.pmState.pmsVelocity)
                                    & eMins .~ (pm^.pmMins)
                                    & eMaxs .~ (pm^.pmMaxs))
        playJumpSound pm =<< readRef edictRef
        modifyRef edictRef (\v -> v & eViewHeight .~ truncate (pm^.pmViewHeight)
                                    & eWaterLevel .~ (pm^.pmWaterLevel)
                                    & eWaterType .~ (pm^.pmWaterType)
                                    & eGroundEntity .~ (pm^.pmGroundEntity))
        maybe (return ()) updateGroundEntityLinkCount (pm^.pmGroundEntity)
        updateViewAngles gClientRef pm =<< readRef edictRef
        linkEntity edictRef
        touchTriggers =<< readRef edictRef
        -- touch other objects
        touchOtherObjects pm 0 (pm^.pmNumTouch)
    updatePMTypeAndGravity gClientRef edict = do
        let pmType | (edict^.eMoveType) == Constants.moveTypeNoClip = Constants.pmSpectator
                   | (edict^.eEntityState.esModelIndex) /= 255      = Constants.pmGib
                   | (edict^.eDeadFlag) /= 0                        = Constants.pmDead
                   | otherwise                                      = Constants.pmNormal
        gravity <- fmap (^.cvValue) svGravityCVar
        modifyRef gClientRef (\v -> v & gcPlayerState.psPMoveState.pmsPMType .~ pmType
                                      & gcPlayerState.psPMoveState.pmsGravity .~ truncate gravity)
    playJumpSound pm edict = do
        when (isJust (edict^.eGroundEntity) && isNothing (pm^.pmGroundEntity) && (pm^.pmCmd.ucUpMove) >= 10 && (pm^.pmWaterLevel) == 0) $ do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "*jump1.wav")
            (gameImport^.giSound) (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf
    updateGroundEntityLinkCount groundEntityRef = do
        groundEntity <- readRef groundEntityRef
        modifyRef edictRef (\v -> v & eGroundEntityLinkCount .~ (groundEntity^.eLinkCount))
    updateViewAngles gClientRef pm edict
        | (edict^.eDeadFlag) /= 0 = do
            gClient <- readRef gClientRef
            modifyRef gClientRef (\v -> v & gcPlayerState.psViewAngles .~ V3 (-15) (gClient^.gcKillerYaw) 40)
        | otherwise =
            modifyRef gClientRef (\v -> v & gcVAngle .~ (pm^.pmViewAngles)
                                          & gcPlayerState.psViewAngles .~ (pm^.pmViewAngles))
    touchTriggers edict =
        when ((edict^.eMoveType) /= Constants.moveTypeNoClip) $
            GameBase.touchTriggers edictRef
    touchOtherObjects pm idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            let Just otherRef = (pm^.pmTouchEnts) V.! idx -- TODO: FIX THIS!
                duplicated = checkIfDuplicated pm (Just otherRef) 0 idx
            unless duplicated $ do
                other <- readRef otherRef
                maybe (return ()) (touchObject otherRef) (other^.eTouch)
            touchOtherObjects pm (idx + 1) maxIdx
    touchObject otherRef touchF = do
        dummyPlane <- use (gameBaseGlobals.gbDummyPlane)
        entTouch touchF otherRef edictRef dummyPlane Nothing
    checkIfDuplicated pm ref idx maxIdx
        | idx >= maxIdx = False
        | otherwise = let otherRef = (pm^.pmTouchEnts) V.! idx
                      in ref == otherRef || checkIfDuplicated pm ref (idx + 1) maxIdx
    updateChaseCamera idx maxIdx
        | idx > maxIdx = return ()
        | otherwise = do
            other <- readRef (Ref idx)
            when (other^.eInUse) $ do
                maybe otherGClientError (checkChaseCam idx) (other^.eClient)
            updateChaseCamera (idx + 1) maxIdx
    otherGClientError = Com.fatalError "PlayerClient.clientThink#updateChaseCam other^.eClient is Nothing"
    checkChaseCam idx gClientRef = do
        gClient <- readRef gClientRef
        when ((gClient^.gcChaseTarget) == Just edictRef) $
            GameChase.updateChaseCam (Ref idx)
    updateButtons gClientRef = do
        gClient <- readRef gClientRef
        modifyRef gClientRef (\v -> v & gcOldButtons .~ (gClient^.gcButtons)
                                      & gcButtons .~ fromIntegral (ucmd^.ucButtons)
                                      & gcLatchedButtons .~ (gClient^.gcLatchedButtons) .|. (fromIntegral (ucmd^.ucButtons) .&. (complement (gClient^.gcButtons))))
    spectatorThink gClientRef = do
        gClient <- readRef gClientRef
        when (gClient^.gcResp.crSpectator) $
            error "PlayerClient.clientThink#spectatorThink" -- TODO
    fireWeaponIfNeeded gClientRef = do
        gClient <- readRef gClientRef
        when ((gClient^.gcLatchedButtons) .&. Constants.buttonAttack /= 0) $
            if gClient^.gcResp.crSpectator
                then do
                    modifyRef gClientRef (\v -> v & gcLatchedButtons .~ 0)
                    case gClient^.gcChaseTarget of
                        Just _ ->
                            modifyRef gClientRef (\v -> v & gcChaseTarget .~ Nothing
                                                          & gcPlayerState.psPMoveState.pmsPMFlags %~ (.&. (complement Constants.pmfNoPrediction)))
                        Nothing -> GameChase.getChaseTarget edictRef
                else
                    unless (gClient^.gcWeaponThunk) $ do
                        modifyRef gClientRef (\v -> v & gcWeaponThunk .~ True)
                        PlayerWeapon.thinkWeapon edictRef

initBodyQue :: Quake ()
initBodyQue = do
    gameBaseGlobals.gbLevel.llBodyQue .= 0
    mapM_ spawnBodyQue [0..Constants.bodyQueueSize-1]
  where
    spawnBodyQue _ = do
        bodyRef <- GameUtil.spawn
        modifyRef bodyRef (\v -> v & eClassName .~ "bodyque")

spectatorRespawn :: Ref EdictT -> Quake ()
spectatorRespawn = error "PlayerClient.spectatorRespawn" -- TODO

respawn :: Ref EdictT -> Quake ()
respawn = error "PlayerClient.respawn" -- TODO

clientBeginDeathmatch :: Ref EdictT -> Quake ()
clientBeginDeathmatch = error "PlayerClient.clientBeginDeathmatch" -- TODO

putClientInServer :: Ref EdictT -> Quake ()
putClientInServer edictRef = do
    -- find a spawn point
    -- do it before setting health back up, so farthest
    -- ranging doesn't count this client
    (spawnOrigin, spawnAngles) <- selectSpawnPoint edictRef
    edict <- readRef edictRef
    let gClientRef = Ref ((edict^.eIndex) - 1)
    -- deathmatch wipes most client data every spawn
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    coop <- fmap (^.cvValue) coopCVar
    clientRespawn <- getClientRespawn gClientRef deathmatch coop
    -- clear everything but the persistant data
    saved <- fmap (^.gcPers) (readRef gClientRef)
    writeRef gClientRef ((newGClientT ((edict^.eIndex) - 1)) & gcPers .~ saved)
    when ((saved^.cpHealth) <= 0) $
        initClientPersistant gClientRef
    modifyRef gClientRef (\v -> v & gcResp .~ clientRespawn)
    -- copy some data from the client to the entity
    fetchClientEntData edictRef
    -- clear entity values
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing
                                & eClient       .~ Just gClientRef
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
    dmFlags <- fmap (truncate . (^.cvValue)) dmFlagsCVar
    fov <- getFov saved deathmatch dmFlags
    gameImport <- use (gameBaseGlobals.gbGameImport)
    weaponRef <- getWeaponRef (saved^.cpWeapon)
    weaponModel <- readRef weaponRef
    gunIndex <- (gameImport^.giModelIndex) (weaponModel^.giViewModel)
    -- clear entity state values
    let updatedSpawnOrigin = spawnOrigin & _z +~ 1 -- make sure off ground
        angles = V3 0 (spawnAngles^._y) 0
    modifyRef gClientRef (\v -> v & gcPlayerState                             .~ newPlayerStateT
                                  & gcPlayerState.psPMoveState.pmsOrigin      .~ fmap (truncate . (* 8)) spawnOrigin
                                  & gcPlayerState.psFOV                       .~ fromIntegral fov
                                  & gcPlayerState.psGunIndex                  .~ gunIndex
                                  & gcPlayerState.psPMoveState.pmsDeltaAngles .~ fmap Math3D.angleToShort (spawnAngles - (clientRespawn^.crCmdAngles))
                                  & gcPlayerState.psViewAngles                .~ angles
                                  & gcVAngle                                  .~ angles)
    modifyRef edictRef (\v -> v & eEntityState.esEffects     .~ 0
                                & eEntityState.esModelIndex  .~ 255 -- will use the skin specified model
                                & eEntityState.esModelIndex2 .~ 255 -- custom gun model
                                -- sknum is player num and weapon number
                                -- weapon number will be added in changeweapon
                                & eEntityState.esSkinNum     .~ (edict^.eIndex) - 1
                                & eEntityState.esFrame       .~ 0
                                & eEntityState.esOrigin      .~ updatedSpawnOrigin
                                & eEntityState.esOldOrigin   .~ updatedSpawnOrigin
                                & eEntityState.esAngles      .~ angles)
    -- spawn a spectator
    if saved^.cpSpectator
      then do
        modifyRef gClientRef (\v -> v & gcChaseTarget            .~ Nothing
                                      & gcResp.crSpectator       .~ True
                                      & gcPlayerState.psGunIndex .~ 0)
        modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNoClip
                                    & eSolid    .~ Constants.solidNot
                                    & eSvFlags  %~ (.|. Constants.svfNoClient))
        (gameImport^.giLinkEntity) edictRef
      else do
        modifyRef gClientRef (\v -> v & gcResp.crSpectator .~ False)
        void (GameUtil.killBox edictRef)
        (gameImport^.giLinkEntity) edictRef
        -- force the current weapon up
        modifyRef gClientRef (\v -> v & gcNewWeapon .~ saved^.cpWeapon)
        PlayerWeapon.changeWeapon edictRef
  where
    getClientRespawn gClientRef deathmatch coop
        | deathmatch /= 0 = do
           gClient <- readRef gClientRef
           initClientPersistant gClientRef
           void (clientUserInfoChanged edictRef (gClient^.gcPers.cpUserInfo))
           return (gClient^.gcResp)
        | coop /= 0 = do
            gClient <- readRef gClientRef
            let userInfo = gClient^.gcPers.cpUserInfo
                coopResp = (gClient^.gcResp.crCoopRespawn) & cpGameHelpChanged .~ (gClient^.gcPers.cpGameHelpChanged)
                                                           & cpHelpChanged .~ (gClient^.gcPers.cpHelpChanged)
                resp = (gClient^.gcResp) & crCoopRespawn .~ coopResp
            modifyRef gClientRef (\v -> v & gcPers .~ (resp^.crCoopRespawn))
            void (clientUserInfoChanged edictRef userInfo)
            when ((resp^.crScore) > (resp^.crCoopRespawn.cpScore)) $
                modifyRef gClientRef (\v -> v & gcPers.cpScore .~ (resp^.crScore))
            return resp
        | otherwise = return newClientRespawnT
    getFov saved deathmatch dmFlags
        | deathmatch /= 0 && (dmFlags .&. Constants.dfFixedFov) /= 0 = return 90
        | otherwise = do
            v <- Info.valueForKey (saved^.cpUserInfo) "fov"
            return (doGetFov (Lib.atoi v))
    doGetFov fov
        | fov < 1   = 90
        | fov > 160 = 160
        | otherwise = fov
    getWeaponRef Nothing = do
        Com.fatalError "PlayerClient.putClientInServer saved^.cpWeapon is Nothing"
        return (Ref (-1))
    getWeaponRef (Just weaponRef) = return weaponRef

selectSpawnPoint :: Ref EdictT -> Quake (V3 Float, V3 Float)
selectSpawnPoint edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    coop <- fmap (^.cvValue) coopCVar
    spawnPoint <- use (gameBaseGlobals.gbGame.glSpawnPoint)
    spot <- findSpot deathmatch coop spawnPoint
    maybe (noSpot spawnPoint) spotFound spot
  where
    findSpot deathmatch coop spawnPoint
        | deathmatch /= 0 = selectDeathmatchSpawnPoint
        | coop /= 0 = selectCoopSpawnPoint edictRef
        | otherwise = do
            -- find a single player start spot
            spot <- findPlayerStart (BC.map toLower spawnPoint) Nothing
            maybe (findPlayerSpot spawnPoint) (return . Just) spot
    findPlayerStart spawnPoint entRef = do
        es <- GameBase.gFind entRef GameBase.findByClass "info_player_start"
        maybe (return Nothing) (doFindPlayerStart spawnPoint) es
    doFindPlayerStart spawnPoint ref = do
        e <- readRef ref
        proceedFindPlayerStart spawnPoint ref e
    proceedFindPlayerStart spawnPoint ref e
        | B.null spawnPoint && isNothing (e^.eTargetName) = return (Just ref)
        | B.null spawnPoint || isNothing (e^.eTargetName) = findPlayerStart spawnPoint (Just ref)
        | spawnPoint == BC.map toLower (fromMaybe "" (e^.eTargetName)) = return (Just ref)
        | otherwise = findPlayerStart spawnPoint (Just ref) 
    findPlayerSpot spawnPoint
        | B.null spawnPoint = GameBase.gFind Nothing GameBase.findByClass "info_player_start"
        | otherwise = return Nothing
    noSpot spawnPoint = do
        err <- use (gameBaseGlobals.gbGameImport.giError)
        err (B.concat ["Couldn't find spawn point ", spawnPoint, "\n"])
        return (V3 0 0 0, V3 0 0 0)
    spotFound entRef = do
        ent <- readRef entRef
        let origin = (ent^.eEntityState.esOrigin) & _z +~ 9
            angles = ent^.eEntityState.esAngles
        return (origin, angles)

selectDeathmatchSpawnPoint :: Quake (Maybe (Ref EdictT))
selectDeathmatchSpawnPoint = error "PlayerClient.selectDeathmatchSpawnPoint" -- TODO

selectCoopSpawnPoint :: Ref EdictT -> Quake (Maybe (Ref EdictT))
selectCoopSpawnPoint = error "PlayerClient.selectCoopSpawnPoint" -- TODO

fetchClientEntData :: Ref EdictT -> Quake ()
fetchClientEntData edictRef = do
    coop <- fmap (^.cvValue) coopCVar
    edict <- readRef edictRef
    maybe gClientError (doFetchClientEntData coop) (edict^.eClient)
  where
    gClientError = Com.fatalError "PlayerClient.fetchClientEntData edict^.eClient is Nothing"
    doFetchClientEntData coop gClientRef = do
        gClient <- readRef gClientRef
        modifyRef edictRef (\v -> v & eHealth .~ gClient^.gcPers.cpHealth
                                    & eMaxHealth .~ gClient^.gcPers.cpMaxHealth
                                    & eFlags %~ (.|. (gClient^.gcPers.cpSavedFlags)))
        when (coop /= 0) $
            modifyRef gClientRef (\v -> v & gcResp.crScore .~ (gClient^.gcPers.cpScore))

playerPain :: EntPain
playerPain = error "PlayerClient.playerPain" -- TODO

playerDie :: EntDie
playerDie = error "PlayerClient.playerDie" -- TODO

defaultTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake TraceT
defaultTrace start mins maxs end = do
    edictRef <- use (clientGlobals.cgPMPassEnt)
    doDefaultTrace edictRef
  where
    doDefaultTrace Nothing = do
        Com.fatalError "PlayerClient.defaultTrace clientGlobals.cgPMPassEnt is Nothing"
        error "PlayerClient.defaultTrace" -- TODO: return something more valid
    doDefaultTrace (Just edictRef) = do
        edict <- readRef edictRef
        trace <- use (gameBaseGlobals.gbGameImport.giTrace)
        trace start (Just mins) (Just maxs) end (Just edictRef) (if (edict^.eHealth) > 0 then Constants.maskPlayerSolid else Constants.maskDeadSolid)
