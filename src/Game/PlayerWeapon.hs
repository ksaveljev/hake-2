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

import           Control.Lens           (use, (^.), (.=), (&), (.~), (+~))
import           Control.Monad          (when, unless, void)
import           Data.Bits              (shiftL, (.&.), (.|.))
import qualified Data.Vector.Unboxed    as UV
import           Linear                 (V3(..))

import qualified Constants
import           Game.ClientPersistantT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameItems         as GameItems
import qualified Game.GameWeapon        as GameWeapon
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
import qualified Util.Math3D            as Math3D

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
playerNoise = error "PlayerWeapon.playerNoise" -- TODO

weaponGeneric :: Ref EdictT -> Int -> Int -> Int -> Int -> UV.Vector Int -> UV.Vector Int -> EntThink -> Quake ()
weaponGeneric = error "PlayerWeapon.weaponGeneric" -- TODO

projectSource :: GClientT -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
projectSource client point distance forward right =
    let V3 a b c = distance
        distance' | (client^.gcPers.cpHand) == Constants.leftHanded = V3 a (negate b) c
                  | (client^.gcPers.cpHand) == Constants.centerHanded = V3 a 0 c
                  | otherwise = distance
    in Math3D.projectSource point distance' forward right
