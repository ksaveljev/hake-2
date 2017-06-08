module Game.PlayerWeapon
    ( changeWeapon
    , dropWeapon
    , pickupWeapon
    , playerNoise
    , thinkWeapon
    , useWeapon
    , weaponBFG
    , weaponBlaster
    , weaponChaingun
    , weaponGrenade
    , weaponGrenadeLauncher
    , weaponHyperBlaster
    , weaponMachinegun
    , weaponRailgun
    , weaponRocketLauncher
    , weaponShotgun
    , weaponSuperShotgun
    ) where

import           Control.Lens           (use, (^.), (.=), (%=), (&), (.~), (+~), (-~), (%~))
import           Control.Monad          (when, unless, void)
import           Data.Bits              (complement, shiftL, (.&.), (.|.))
import           Data.Maybe             (isJust, isNothing)
import qualified Data.Vector.Unboxed    as UV
import           Linear                 (V3(..))

import qualified Constants
import           Game.ClientPersistantT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameItems         as GameItems
import qualified Game.GameUtil          as GameUtil
import           Game.GClientT
import           Game.GItemT
import           Game.LevelLocalsT
import qualified Game.Monsters.MPlayer  as MPlayer
import           Game.PlayerStateT
import           Game.PMoveStateT
import qualified QCommon.Com            as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib               as Lib
import qualified Util.Math3D            as Math3D

import {-# SOURCE #-} qualified Game.GameWeapon as GameWeapon

useWeapon :: ItemUse
useWeapon = ItemUse "PlayerWeapon.useWeapon" useWeaponF

useWeaponF :: Ref EdictT -> Ref GItemT -> Quake ()
useWeaponF = error "PlayerWeapon.useWeaponF"

pickupWeapon :: EntInteract
pickupWeapon = error "PlayerWeapon.pickupWeapon" -- TODO

dropWeapon :: ItemDrop
dropWeapon = error "PlayerWeapon.dropWeapon" -- TODO

weaponBlaster :: EntThink
weaponBlaster = EntThink "Weapon_Blaster" $ \edictRef -> do
    let pauseFrames = UV.fromList [19, 32, 0]
        fireFrames = UV.fromList [5, 0]
    weaponGeneric edictRef 4 8 52 55 pauseFrames fireFrames weaponBlasterFire
    return True

weaponBlasterFire :: EntThink
weaponBlasterFire = EntThink "Weapon_Blaster_Fire" $ \edictRef -> do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    v3o <- use (globals.gVec3Origin)
    let damage = if deathmatch /= 0 then 15 else 10
    blasterFire edictRef v3o damage False Constants.efBlaster
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame +~ 1)
    return True
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerWeapon.weaponBlasterFire edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef

blasterFire :: Ref EdictT -> V3 Float -> Int -> Bool -> Int -> Quake ()
blasterFire edictRef gOffset dmg hyper effect = do
    isQuad <- use (gameBaseGlobals.gbIsQuad)
    isSilenced <- use (gameBaseGlobals.gbIsSilenced)
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    gClient <- readRef gClientRef
    let damage = if isQuad then dmg * 4 else dmg
        (forward, right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False
        offset = (V3 24 8 (fromIntegral (edict^.eViewHeight) - 8)) + gOffset
        start = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right
        V3 _ b c = fmap (* (-2)) forward
    modifyRef gClientRef (\v -> v & gcKickOrigin .~ V3 (-1) b c)
    GameWeapon.fireBlaster edictRef start forward damage 1000 effect hyper
    -- send muzzle flash
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast
    writeByte Constants.svcMuzzleFlash
    writeShort (edict^.eIndex)
    if hyper
        then writeByte (Constants.mzHyperblaster .|. isSilenced)
        else writeByte (Constants.mzBlaster .|. isSilenced)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs
    playerNoise edictRef start Constants.pNoiseWeapon
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerWeapon.blasterFire edict^.eClient is Nohting"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef

weaponShotgun :: EntThink
weaponShotgun = error "PlayerWeapon.weaponShotgun" -- TODO

weaponSuperShotgun :: EntThink
weaponSuperShotgun = error "PlayerWeapon.weaponShotgun" -- TODO

weaponMachinegun :: EntThink
weaponMachinegun = error "PlayerWeapon.weaponMachinegun" -- TODO

weaponChaingun :: EntThink
weaponChaingun = error "PlayerWeapon.weaponChaingun" -- TODO

weaponGrenade :: EntThink
weaponGrenade = error "PlayerWeapon.weaponGrenade" -- TODO

weaponGrenadeLauncher :: EntThink
weaponGrenadeLauncher = error "PlayerWeapon.weaponGrenadeLauncher" -- TODO

weaponRocketLauncher :: EntThink
weaponRocketLauncher = error "PlayerWeapon.weaponRocketLauncher" -- TODO

weaponHyperBlaster :: EntThink
weaponHyperBlaster = error "PlayerWeapon.weaponHyperBlaster" -- TODO

weaponRailgun :: EntThink
weaponRailgun = error "PlayerWeapon.weaponRailgun" -- TODO

weaponBFG :: EntThink
weaponBFG = error "PlayerWeapon.weaponBFG" -- TODO

thinkWeapon :: Ref EdictT -> Quake ()
thinkWeapon edictRef = do
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    -- if just died, put the weapon away
    when ((edict^.eHealth) < 1) $ do
        modifyRef gClientRef (\v -> v & gcNewWeapon .~ Nothing)
        changeWeapon edictRef
    -- call active weapon think routine
    gClient <- readRef gClientRef
    maybe (return ()) (doThinkWeapon gClient) (gClient^.gcPers.cpWeapon)
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerWeapon.thinkWeapon edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    doThinkWeapon gClient weaponRef = do
        weapon <- readRef weaponRef
        maybe (return ()) (proceedThinkWeapon gClient) (weapon^.giWeaponThink)
    proceedThinkWeapon gClient thinkF = do
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        gameBaseGlobals.gbIsQuad .= (truncate (gClient^.gcQuadFrameNum) > frameNum)
        gameBaseGlobals.gbIsSilenced .= if (gClient^.gcSilencerShots) /= 0 then Constants.mzSilenced else 0
        void (entThink thinkF edictRef)

changeWeapon :: Ref EdictT -> Quake ()
changeWeapon edictRef = do
    edict <- readRef edictRef
    maybe gClientError doChangeWeapon (edict^.eClient)
  where
    gClientError = Com.fatalError "PlayerWeapon.changeWeapon edict^.eClient is Nothing"
    doChangeWeapon gClientRef = do
        checkGrenadeTime gClientRef
        gClient <- readRef gClientRef
        modifyRef gClientRef (\v -> v & gcPers.cpLastWeapon .~ gClient^.gcPers.cpWeapon
                                      & gcPers.cpWeapon     .~ gClient^.gcNewWeapon
                                      & gcNewWeapon         .~ Nothing
                                      & gcMachinegunShots   .~ 0)
        -- set visible model
        setVisibleModel gClientRef
        done <- setAmmoAndGunIndex gClientRef
        unless done $ do
            modifyRef gClientRef (\v -> v & gcAnimPriority .~ Constants.animPain)
            updatedGClient <- readRef gClientRef
            if (updatedGClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0
                then do
                    modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRPain1)
                    modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameCRPain4)
                else do
                    modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain301)
                    modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.framePain304)
    checkGrenadeTime gClientRef = do
        gClient <- readRef gClientRef
        when (gClient^.gcGrenadeTime /= 0) $ do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef gClientRef (\v -> v & gcGrenadeTime .~ levelTime
                                          & gcWeaponSound .~ 0)
            weaponGrenadeFire edictRef False
            modifyRef gClientRef (\v -> v & gcGrenadeTime .~ 0)
    setVisibleModel gClientRef = do
        edict <- readRef edictRef
        when ((edict^.eEntityState.esModelIndex) == 255) $ do
            gClient <- readRef gClientRef
            i <- getWeaponModel (gClient^.gcPers.cpWeapon)
            modifyRef edictRef (\v -> v & eEntityState.esSkinNum .~ ((edict^.eIndex) - 1) .|. i)
    getWeaponModel Nothing = return 0
    getWeaponModel (Just weaponRef) = do
        weaponModel <- fmap (^.giWeaponModel) (readRef weaponRef)
        return ((weaponModel .&. 0xFF) `shiftL` 8)
    setAmmoAndGunIndex gClientRef= do
        gClient <- readRef gClientRef
        setAmmoIndex gClientRef (gClient^.gcPers.cpWeapon)
        setGunIndex gClientRef (gClient^.gcPers.cpWeapon)
    setAmmoIndex gClientRef Nothing =
        modifyRef gClientRef (\v -> v & gcAmmoIndex .~ 0)
    setAmmoIndex gClientRef (Just weaponRef) = do
        weapon <- readRef weaponRef
        doSetAmmoIndex gClientRef (weapon^.giAmmo)
    doSetAmmoIndex gClientRef Nothing =
        modifyRef gClientRef (\v -> v & gcAmmoIndex .~ 0)
    doSetAmmoIndex gClientRef (Just ammo) = do
        ammoItem <- GameItems.findItem ammo
        maybe ammoItemError (setItemAmmoIndex gClientRef) ammoItem
    ammoItemError = Com.fatalError "PlayerWeapon.changeWeapon#doSetAmmoIndex ammoItem is Nothing"
    setItemAmmoIndex gClientRef itemRef = do
        ammoItem <- readRef itemRef
        modifyRef gClientRef (\v -> v & gcAmmoIndex .~ ammoItem^.giIndex)
    setGunIndex gClientRef Nothing = do
        modifyRef gClientRef (\v -> v & gcPlayerState.psGunIndex .~ 0)
        return True
    setGunIndex gClientRef (Just weaponRef) = do
        weapon <- readRef weaponRef
        modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
        gunIndex <- modelIndex (weapon^.giViewModel)
        modifyRef gClientRef (\v -> v & gcWeaponState            .~ Constants.weaponActivating
                                      & gcPlayerState.psGunFrame .~ 0
                                      & gcPlayerState.psGunIndex .~ gunIndex)
        return False

weaponGrenadeFire :: Ref EdictT -> Bool -> Quake ()
weaponGrenadeFire = error "PlayerWeapon.weaponGrenadeFire" -- TODO

playerNoise :: Ref EdictT -> V3 Float -> Int -> Quake ()
playerNoise whoRef noiseLocation noiseType = do
    who <- readRef whoRef
    gClientRef <- getGClientRef (who^.eClient)
    gClient <- readRef gClientRef
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doPlayerNoise gClientRef gClient who deathmatch
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerWeapon.playerNoise who^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    doPlayerNoise gClientRef gClient who deathmatch
        | noiseType == Constants.pNoiseWeapon && (gClient^.gcSilencerShots) > 0 =
            modifyRef gClientRef (\v -> v & gcSilencerShots -~ 1)
        | deathmatch /= 0 || (who^.eFlags) .&. Constants.flNoTarget /= 0 =
            return ()
        | otherwise = do
            when (isNothing (who^.eMyNoise)) $
                spawnNoises
            frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
            Just noiseRef <- getNoiseRef frameNum =<< readRef whoRef -- IMPROVE: looks bad really
            noise <- readRef noiseRef
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef noiseRef (\v -> v & eEntityState.esOrigin .~ noiseLocation
                                        & eAbsMin .~ (noiseLocation - (noise^.eMaxs))
                                        & eAbsMax .~ (noiseLocation + (noise^.eMaxs))
                                        & eTeleportTime .~ levelTime)
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity noiseRef
    spawnNoises = do
        noiseRef <- GameUtil.spawn
        modifyRef noiseRef (\v -> v & eClassName .~ "player_noise"
                                    & eMins .~ V3 (-8) (-8) (-8)
                                    & eMaxs .~ V3 8 8 8
                                    & eOwner .~ Just whoRef
                                    & eSvFlags .~ Constants.svfNoClient)
        anotherNoiseRef <- GameUtil.spawn
        modifyRef anotherNoiseRef (\v -> v & eClassName .~ "player_noise"
                                           & eMins .~ V3 (-8) (-8) (-8)
                                           & eMaxs .~ V3 8 8 8
                                           & eOwner .~ Just whoRef
                                           & eSvFlags .~ Constants.svfNoClient)
        modifyRef whoRef (\v -> v & eMyNoise .~ Just noiseRef
                                  & eMyNoise2 .~ Just anotherNoiseRef)
    getNoiseRef :: Int -> EdictT -> Quake (Maybe (Ref EdictT))
    getNoiseRef frameNum who
        | noiseType == Constants.pNoiseSelf || noiseType == Constants.pNoiseWeapon = do
            gameBaseGlobals.gbLevel %= (\v -> v & llSoundEntity .~ (who^.eMyNoise)
                                                & llSoundEntityFrameNum .~ frameNum)
            return (who^.eMyNoise)
        | otherwise = do
            gameBaseGlobals.gbLevel %= (\v -> v & llSound2Entity .~ (who^.eMyNoise2)
                                                & llSound2EntityFrameNum .~ frameNum)
            return (who^.eMyNoise2)

weaponGeneric :: Ref EdictT -> Int -> Int -> Int -> Int -> UV.Vector Int -> UV.Vector Int -> EntThink -> Quake ()
weaponGeneric edictRef frameActiveLast frameFireLast frameIdleLast frameDeactivateLast pauseFrames fireFrames fire = do
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    gClient <- readRef gClientRef
    doWeaponGeneric edict gClientRef gClient
  where
    frameFireFirst = frameActiveLast + 1
    frameIdleFirst = frameFireLast + 1
    frameDeactivateFirst = frameIdleLast + 1
    getGClientRef Nothing = do
        Com.fatalError "PlayerWeapon.weaponGeneric edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    doWeaponGeneric edict gClientRef gClient
        | (edict^.eDeadFlag) /= 0 || (edict^.eEntityState.esModelIndex) /= 255 =
            return ()
        | (gClient^.gcWeaponState) == Constants.weaponDropping =
            weaponDropping gClientRef gClient
        | (gClient^.gcWeaponState) == Constants.weaponActivating =
            updateGunFrame gClientRef gClient
        | isJust (gClient^.gcNewWeapon) && (gClient^.gcWeaponState) /= Constants.weaponFiring = do
            modifyRef gClientRef (\v -> v & gcWeaponState .~ Constants.weaponDropping
                                          & gcPlayerState.psGunFrame .~ frameDeactivateFirst)
            when (frameDeactivateLast - frameDeactivateFirst < 4) $ do
                modifyRef gClientRef (\v -> v & gcAnimPriority .~ Constants.animReverse)
                setPainFrame gClientRef gClient
        | otherwise = do
            done <- checkWeaponReady gClientRef gClient
            unless done $ do
              when ((gClient^.gcWeaponState) == Constants.weaponFiring) $ do
                  idx <- checkFireFrames gClientRef 0
                  when (fireFrames UV.! idx == 0) $
                      modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame +~ 1)
                  gunFrame <- fmap (^.gcPlayerState.psGunFrame) (readRef gClientRef)
                  when (gunFrame == frameIdleFirst + 1) $
                      modifyRef gClientRef (\v -> v & gcWeaponState .~ Constants.weaponReady)
    weaponDropping gClientRef gClient
        | (gClient^.gcPlayerState.psGunFrame) == frameDeactivateLast =
            changeWeapon edictRef
        | frameDeactivateLast - (gClient^.gcPlayerState.psGunFrame) == 4 = do
            modifyRef gClientRef (\v -> v & gcAnimPriority .~ Constants.animReverse)
            setPainFrame gClientRef gClient
            modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame +~ 1)
        | otherwise =
            modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame +~ 1)
    setPainFrame gClientRef gClient
        | (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0 = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRPain4 + 1)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameCRPain1)
        | otherwise = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain304 + 1)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.framePain301)
    updateGunFrame gClientRef gClient
        | (gClient^.gcPlayerState.psGunFrame) == frameActiveLast =
            modifyRef gClientRef (\v -> v & gcWeaponState .~ Constants.weaponReady
                                          & gcPlayerState.psGunFrame .~ frameIdleFirst)
        | otherwise =
            modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame +~ 1)
    checkWeaponReady gClientRef gClient
        | (gClient^.gcWeaponState) == Constants.weaponReady = weaponReady gClientRef gClient
        | otherwise = return False
    weaponReady gClientRef gClient
        | ((gClient^.gcLatchedButtons) .|. (gClient^.gcButtons)) .&. Constants.buttonAttack /= 0 = do
            modifyRef gClientRef (\v -> v & gcLatchedButtons %~ (.&. (complement Constants.buttonAttack)))
            weaponRef <- getWeaponRef (gClient^.gcPers.cpWeapon)
            weapon <- readRef weaponRef
            fireWeapon gClientRef gClient weapon
            return False
        | (gClient^.gcPlayerState.psGunFrame) == frameIdleLast = do
            modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame .~ frameIdleFirst)
            return True
        | otherwise = do
            -- TODO: do we need this?
            -- if (pause_frames != null) {
            done <- checkPauseFrames gClient 0
            unless done $
                modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame +~ 1)
            return True
    getWeaponRef Nothing = do
        Com.fatalError "PlayerWeapon.weaponGeneric#weaponReady gClient^.gcPers.cpWeapon is Nothing"
        return (Ref (-1))
    getWeaponRef (Just weaponRef) = return weaponRef
    setAttackFrame gClientRef gClient
        | (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0 = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRAttack1 - 1)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameCRAttack9)
        | otherwise = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameAttack1 - 1)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameAttack8)
    fireWeapon gClientRef gClient weapon
        | (gClient^.gcAmmoIndex) == 0 || ((gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex)) >= (weapon^.giQuantity) = do
            modifyRef gClientRef (\v -> v & gcPlayerState.psGunFrame .~ frameFireFirst
                                          & gcWeaponState .~ Constants.weaponFiring
                                          & gcAnimPriority .~ Constants.animAttack)
            setAttackFrame gClientRef gClient
        | otherwise = do
            edict <- readRef edictRef
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            when (levelTime >= (edict^.ePainDebounceTime)) $ do
                gameImport <- use (gameBaseGlobals.gbGameImport)
                soundIdx <- (gameImport^.giSoundIndex) (Just "weapons/noammo.wav")
                (gameImport^.giSound) (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
                modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)
            noAmmoWeaponChange edictRef
    checkFireFrames gClientRef idx
        | fireFrames UV.! idx == 0 = return idx
        | otherwise = do
            gClient <- readRef gClientRef
            if (gClient^.gcPlayerState.psGunFrame) == fireFrames UV.! idx
                then do
                    frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
                    when (truncate (gClient^.gcQuadFrameNum) > frameNum) $ do
                      gameImport <- use (gameBaseGlobals.gbGameImport)
                      soundIdx <- (gameImport^.giSoundIndex) (Just "items/damage3.wav")
                      (gameImport^.giSound) (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0
                    void (entThink fire edictRef)
                    return idx
                else
                    checkFireFrames gClientRef (idx + 1)
    checkPauseFrames gClient idx
        | pauseFrames UV.! idx == 0 = return False
        | (gClient^.gcPlayerState.psGunFrame) == pauseFrames UV.! idx = do
            r <- Lib.rand
            if r .&. 15 /= 0
                then return True
                else checkPauseFrames gClient (idx + 1)
        | otherwise =
            checkPauseFrames gClient (idx + 1)

projectSource :: GClientT -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
projectSource client point distance forward right =
    let V3 a b c = distance
        distance' | (client^.gcPers.cpHand) == Constants.leftHanded = V3 a (negate b) c
                  | (client^.gcPers.cpHand) == Constants.centerHanded = V3 a 0 c
                  | otherwise = distance
    in Math3D.projectSource point distance' forward right

noAmmoWeaponChange :: Ref EdictT -> Quake ()
noAmmoWeaponChange edictRef = do
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    gClient <- readRef gClientRef
    weaponRef <- checkSlugsAndRailgun gClient
    modifyRef gClientRef (\v -> v & gcNewWeapon .~ weaponRef)
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerWeapon.noAmmoWeaponChange edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    getItem Nothing = do
        Com.fatalError "PlayerWeapon.noAmmoWeaponChange getItem on Nothing"
        error "PlayerWeapon.noAmmoWeaponChange" -- TODO: return something more related
    getItem (Just itemRef) = readRef itemRef
    checkSlugsAndRailgun gClient = do
        slugsRef <- GameItems.findItem "slugs"
        railgunRef <- GameItems.findItem "railgun"
        slugs <- getItem slugsRef
        railgun <- getItem railgunRef
        let inventory = gClient^.gcPers.cpInventory
        if inventory UV.! (slugs^.giIndex) /= 0 && inventory UV.! (railgun^.giIndex) /= 0
            then return railgunRef
            else checkCellsAndHyperblaster gClient
    checkCellsAndHyperblaster gClient = do
        cellsRef <- GameItems.findItem "cells"
        hyperblasterRef <- GameItems.findItem "hyperblaster"
        cells <- getItem cellsRef
        hyperblaster <- getItem hyperblasterRef
        let inventory = gClient^.gcPers.cpInventory
        if inventory UV.! (cells^.giIndex) /= 0 && inventory UV.! (hyperblaster^.giIndex) /= 0
            then return hyperblasterRef
            else checkBulletsAndChaingun gClient
    checkBulletsAndChaingun gClient = do
        bulletsRef <- GameItems.findItem "bullets"
        chaingunRef <- GameItems.findItem "chaingun"
        bullets <- getItem bulletsRef
        chaingun <- getItem chaingunRef
        let inventory = gClient^.gcPers.cpInventory
        if inventory UV.! (bullets^.giIndex) /= 0 && inventory UV.! (chaingun^.giIndex) /= 0
            then return chaingunRef
            else checkBulletsAndMachinegun gClient
    checkBulletsAndMachinegun gClient = do
        bulletsRef <- GameItems.findItem "bullets"
        machinegunRef <- GameItems.findItem "machinegun"
        bullets <- getItem bulletsRef
        machinegun <- getItem machinegunRef
        let inventory = gClient^.gcPers.cpInventory
        if inventory UV.! (bullets^.giIndex) /= 0 && inventory UV.! (machinegun^.giIndex) /= 0
            then return machinegunRef
            else checkShellsAndSuperShotgun gClient
    checkShellsAndSuperShotgun gClient = do
        shellsRef <- GameItems.findItem "shells"
        superShotgunRef <- GameItems.findItem "super shotgun"
        shells <- getItem shellsRef
        superShotgun <- getItem superShotgunRef
        let inventory = gClient^.gcPers.cpInventory
        if inventory UV.! (shells^.giIndex) > 1 && inventory UV.! (superShotgun^.giIndex) /= 0
            then return superShotgunRef
            else checkShellsAndShotgun gClient
    checkShellsAndShotgun gClient = do
        shellsRef <- GameItems.findItem "shells"
        shotgunRef <- GameItems.findItem "shotgun"
        blasterRef <- GameItems.findItem "blaster"
        shells <- getItem shellsRef
        shotgun <- getItem shotgunRef
        let inventory = gClient^.gcPers.cpInventory
        if inventory UV.! (shells^.giIndex) /= 0 && inventory UV.! (shotgun^.giIndex) /= 0
            then return shotgunRef
            else return blasterRef
