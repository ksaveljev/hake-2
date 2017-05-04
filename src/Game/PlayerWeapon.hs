module Game.PlayerWeapon
    ( changeWeapon
    , dropWeapon
    , pickupWeapon
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

import           Control.Lens           (use, (^.), (&), (.~))
import           Control.Monad          (when, unless)
import           Data.Bits              (shiftL, (.&.), (.|.))

import qualified Constants
import           Game.ClientPersistantT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameItems         as GameItems
import           Game.GClientT
import           Game.GItemT
import           Game.LevelLocalsT
import qualified Game.Monsters.MPlayer  as MPlayer
import           Game.PlayerStateT
import           Game.PMoveStateT
import qualified QCommon.Com            as Com
import           QuakeRef
import           QuakeState
import           Types

useWeapon :: ItemUse
useWeapon = ItemUse "PlayerWeapon.useWeapon" useWeaponF

useWeaponF :: Ref EdictT -> Ref GItemT -> Quake ()
useWeaponF = error "PlayerWeapon.useWeaponF"

pickupWeapon :: EntInteract
pickupWeapon = error "PlayerWeapon.pickupWeapon" -- TODO

dropWeapon :: ItemDrop
dropWeapon = error "PlayerWeapon.dropWeapon" -- TODO

weaponBlaster :: EntThink
weaponBlaster = error "PlayerWeapon.weaponBlaster" -- TODO

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
thinkWeapon = error "PlayerWeapon.thinkWeapon" -- TODO

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
