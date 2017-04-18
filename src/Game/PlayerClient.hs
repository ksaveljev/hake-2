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

import           Control.Lens           (use, ix, (^.), (.=), (&), (.~))
import           Control.Monad          (when, unless)
import           Data.Bits              ((.&.), (.|.))
import qualified Data.ByteString        as B

import qualified Constants
import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameItems         as GameItems
import           Game.GameLocalsT
import qualified Game.GameSVCmds        as GameSVCmds
import qualified Game.GameUtil          as GameUtil
import           Game.GClientT
import qualified Game.Info              as Info
import           Game.LevelLocalsT
import           Game.PlayerStateT
import qualified Game.PlayerTrail       as PlayerTrail
import qualified Game.PlayerWeapon      as PlayerWeapon
import qualified QCommon.Com            as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib               as Lib

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
spInfoPlayerCoop = error "PlayerClient.spInfoPlayerCoop" -- TODO

spInfoPlayerDeathmatch :: Ref EdictT -> Quake ()
spInfoPlayerDeathmatch = error "PlayerClient.spInfoPlayerDeathmatch" -- TODO

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = error "PlayerClient.spInfoPlayerIntermission" -- TODO

spInfoPlayerStart :: Ref EdictT -> Quake ()
spInfoPlayerStart = error "PlayerClient.spInfoPlayerStart" -- TODO

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
clientBegin = error "PlayerClient.clientBegin" -- TODO

clientThink :: Ref EdictT -> UserCmdT -> Quake ()
clientThink = error "PlayerClient.clientThink" -- TODO

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