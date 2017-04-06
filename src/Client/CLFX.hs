module Client.CLFX
    ( addDLights
    , addLightStyles
    , addParticles
    , bfgExplosionParticles
    , bfgParticles
    , bigTeleportParticles
    , blasterParticles
    , blasterTrail
    , bubbleTrail
    , clearEffects
    , diminishingTrail
    , entityEvent
    , explosionParticles
    , flagTrail
    , flyEffect
    , ionRipperTrail
    , parseMuzzleFlash
    , parseMuzzleFlash2
    , particleEffect
    , particleEffect2
    , particleEffect3
    , railTrail
    , rocketTrail
    , runDLights
    , runLightStyles
    , setLightStyle
    , teleporterParticles
    , teleportParticles
    , trapParticles
    ) where

import           Control.Lens          (preuse, use, ix)
import           Control.Lens          ((^.), (%=), (.=), (&), (.~), (%~), (-~))
import           Control.Monad         (unless, when)
import           Data.Bits             (complement, (.&.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (ord)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as UV
import qualified Data.Vector.Mutable   as MV
import           Linear                (V3(..), norm, normalize, _x, _y, _z)

import           Client.CDLightT
import           Client.CEntityT
import           Client.ClientStateT
import           Client.CLightStyleT
import qualified Client.CLTEntShared   as CLTEnt
import           Client.CParticleT
import qualified Client.VShared        as ClientV
import qualified Constants
import           Game.CVarT
import           Game.EntityStateT
import qualified Game.Monsters.MFlash  as MFlash
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import qualified QCommon.MSG           as MSG
import           QuakeRef
import           QuakeState
import qualified Sound.S               as S
import           Types
import           Util.Binary           (encode)
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

particleGravity :: Float
particleGravity = 40

instantParticle :: Float
instantParticle = -10000.0

runDLights :: Quake ()
runDLights = do
    time <- use (globals.gCl.csTime)
    mapM_ (runDLight time) (fmap Ref [0..Constants.maxDLights - 1])

runDLight :: Int -> Ref CDLightT -> Quake ()
runDLight time dLightRef = doRunDLight =<< readRef dLightRef
  where
    doRunDLight dl
        | dl^.cdlRadius == 0 = return ()
        | dl^.cdlDie < (fromIntegral time) = writeRef dLightRef (dl & cdlRadius .~ 0)
        | otherwise = return ()
        -- TODO: original quake2 code does have something else
        -- here (jake2 is missing a part of this function)

runLightStyles :: Quake ()
runLightStyles = do
    time <- use (globals.gCl.csTime)
    lastOfs <- use $ clientGlobals.cgLastOfs
    doRunLightStyles time lastOfs

doRunLightStyles :: Int -> Int -> Quake ()
doRunLightStyles time lastOfs =
    unless (ofs == lastOfs) $ do
        clientGlobals.cgLastOfs .= ofs
        clientGlobals.cgLightStyle %= V.map (runLightStyle ofs)
  where
    ofs = time `div` 100

runLightStyle :: Int -> CLightStyleT -> CLightStyleT
runLightStyle ofs ls
    | ls^.clsLength == 0 = ls & clsValue .~ V3 1 1 1
    | ls^.clsLength == 1 = let v = (ls^.clsMap) UV.! 0
                           in ls & clsValue .~ V3 v v v
    | otherwise = let v = (ls^.clsMap) UV.! (ofs `mod` (ls^.clsLength))
                  in ls & clsValue .~ V3 v v v

parseMuzzleFlash :: Quake ()
parseMuzzleFlash = do
    idx <- getEntityIdx
    w <- MSG.readByte (globals.gNetMessage)
    pl <- preuse (globals.gClEntities.ix idx)
    maybe badEntityError (proceedParseMuzzleFlash idx w) pl
  where
    getEntityIdx = do
        idx <- MSG.readShort (globals.gNetMessage)
        when (idx < 1 || idx >= Constants.maxEdicts) badEntityError
        return idx
    badEntityError =
        Com.comError Constants.errDrop "CL_ParseMuzzleFlash: bad entity"

proceedParseMuzzleFlash :: Int -> Int -> CEntityT -> Quake ()
proceedParseMuzzleFlash idx w pl = do
    dLightRef <- allocDLight idx
    r <- Lib.rand
    time <- use (globals.gCl.csTime)
    modifyRef dLightRef (\v -> v & cdlOrigin .~ origin
                                 & cdlRadius .~ radius r
                                 & cdlMinLight .~ 32
                                 & cdlDie .~ fromIntegral time)
    muzzleFlashSound dLightRef (Ref idx) pl weapon volume
  where
    silenced = w .&. Constants.mzSilenced
    weapon = w .&. (complement Constants.mzSilenced)
    (fv, rv, _) = Math3D.angleVectors (pl^.ceCurrent.esAngles) True True False
    origin = (pl^.ceCurrent.esOrigin) + fmap (* 18) fv + fmap (* 16) rv
    radius r | silenced /= 0 = 100 + fromIntegral (r .&. 31)
             | otherwise = 200 + fromIntegral (r .&. 31)
    volume | silenced /= 0 = 0.2
           | otherwise = 1

muzzleFlashSound :: Ref CDLightT -> Ref EdictT -> CEntityT -> Int -> Float -> Quake ()
muzzleFlashSound dLightRef edictRef pl weapon volume
    | weapon == Constants.mzBlaster = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "weapons/blastf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzBlueHyperblaster = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0 0 1)
        soundRef <- S.registerSound "weapons/hyprbf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzHyperblaster = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "weapons/hyprbf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzMachinegun = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        r <- Lib.rand
        soundRef <- S.registerSound (B.concat ["weapons/machgf", encode ((r `mod` 5) + 1), "b.wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzShotgun = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef1 <- S.registerSound "weapons/shotgf1b.wav"
        soundRef2 <- S.registerSound "weapons/shotgr1b.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef1 volume Constants.attnNorm 0
        S.startSound Nothing edictRef Constants.chanAuto soundRef2 volume Constants.attnNorm 0.1
    | weapon == Constants.mzSShotgun = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "weapons/sshotf1b.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzChaingun1 = do
        r <- Lib.rand
        modifyRef dLightRef (\v -> v & cdlRadius .~ 200 + fromIntegral (r .&. 31)
                                     & cdlColor .~ V3 1 0.25 0)
        r' <- Lib.rand
        soundRef <- S.registerSound (B.concat ["weapons/machgf", encode ((r' `mod` 5) + 1), "b.wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzChaingun2 = do
        r <- Lib.rand
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlRadius .~ 225 + fromIntegral (r .&. 31)
                                     & cdlColor .~ V3 1 0.5 0
                                     & cdlDie .~ fromIntegral time + 0.1) -- long delay
        r' <- Lib.rand
        soundRef <- S.registerSound (B.concat ["weapons/machgf", encode ((r' `mod` 5) + 1), "b.wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
        r'' <- Lib.rand
        soundRef' <- S.registerSound (B.concat ["weapons/machgf", encode ((r'' `mod` 5) + 1), "b.wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef' volume Constants.attnNorm 0.05
    | weapon == Constants.mzChaingun3 = do
        r <- Lib.rand
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlRadius .~ 250 + fromIntegral (r .&. 31)
                                     & cdlColor .~ V3 1 1 0
                                     & cdlDie .~ fromIntegral time + 0.1) -- long delay
        r' <- Lib.rand
        soundRef <- S.registerSound (B.concat ["weapons/machgf", encode ((r' `mod` 5) + 1), "b.wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
        r'' <- Lib.rand
        soundRef' <- S.registerSound (B.concat ["weapons/machgf", encode ((r'' `mod` 5) + 1), "b.wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef' volume Constants.attnNorm 0.033
        r''' <- Lib.rand
        soundRef'' <- S.registerSound (B.concat ["weapons/machgf", encode ((r''' `mod` 5) + 1), "b.wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef'' volume Constants.attnNorm 0.066
    | weapon == Constants.mzRailgun = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0.5 0.5 1)
        soundRef <- S.registerSound "weapons/railgf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzRocket = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0.2)
        soundRef1 <- S.registerSound "weapons/rocklf1a.wav"
        soundRef2 <- S.registerSound "weapons/rocklr1b.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef1 volume Constants.attnNorm 0
        S.startSound Nothing edictRef Constants.chanAuto soundRef2 volume Constants.attnNorm 0.1
    | weapon == Constants.mzGrenade = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0)
        soundRef1 <- S.registerSound "weapons/grenlf1a.wav"
        soundRef2 <- S.registerSound "weapons/grenlr1b.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef1 volume Constants.attnNorm 0
        S.startSound Nothing edictRef Constants.chanAuto soundRef2 volume Constants.attnNorm 0.1
    | weapon == Constants.mzBFG = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0 1 0)
        soundRef <- S.registerSound "weapons/bfg__f1y.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzLogin = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0 1 0
                                     & cdlDie .~ fromIntegral time + 1.0)
        soundRef <- S.registerSound "weapons/grenlf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
        logoutEffect (pl^.ceCurrent.esOrigin) weapon
    | weapon == Constants.mzLogout = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0 0
                                     & cdlDie .~ fromIntegral time + 1.0)
        soundRef <- S.registerSound "weapons/grenlf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
        logoutEffect (pl^.ceCurrent.esOrigin) weapon
    | weapon == Constants.mzRespawn = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0
                                     & cdlDie .~ fromIntegral time + 1.0)
        soundRef <- S.registerSound "weapons/grenlf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
        logoutEffect (pl^.ceCurrent.esOrigin) weapon
    | weapon == Constants.mzPhalanx = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0.5)
        soundRef <- S.registerSound "weapons/plasshot.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzIonRipper = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0.5)
        soundRef <- S.registerSound "weapons/rippfire.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzEtfRifle = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0.9 0.7 0)
        soundRef <- S.registerSound "weapons/nail1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzShotgun2 = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "weapons/shotg2.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzHeatBeam = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0
                                     & cdlDie .~ fromIntegral time + 100)
    | weapon == Constants.mzBlaster2 = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0 1 0)
        -- FIXME: different sound for blaster2 ??
        soundRef <- S.registerSound "weapons/blastf1a.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzTracker = do
        -- negative flashes handled the same in gl/soft until CL_AddDLights
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 (-1) (-1) (-1))
        soundRef <- S.registerSound "weapons/disint2.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef volume Constants.attnNorm 0
    | weapon == Constants.mzNuke1 = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0 0
                                     & cdlDie .~ fromIntegral time + 100)
    | weapon == Constants.mzNuke2 = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0
                                     & cdlDie .~ fromIntegral time + 100)
    | weapon == Constants.mzNuke4 = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0 0 1
                                     & cdlDie .~ fromIntegral time + 100)
    | weapon == Constants.mzNuke8 = do
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0 1 1
                                     & cdlDie .~ fromIntegral time + 100)
    | otherwise = return () -- TODO: some error should be thrown?

parseMuzzleFlash2 :: Quake ()
parseMuzzleFlash2 = do
    idx <- getEntityIdx
    flashNumber <- MSG.readByte (globals.gNetMessage)
    cent <- preuse (globals.gClEntities.ix idx)
    maybe badEntityError (proceedParseMuzzleFlash2 idx flashNumber) cent
  where
    getEntityIdx = do
        idx <- MSG.readShort (globals.gNetMessage)
        when (idx < 1 || idx >= Constants.maxEdicts) badEntityError
        return idx
    badEntityError =
        Com.comError Constants.errDrop "CL_ParseMuzzleFlash2: bad entity"

proceedParseMuzzleFlash2 :: Int -> Int -> CEntityT -> Quake ()
proceedParseMuzzleFlash2 idx flashNumber cent = do
    dLightRef <- allocDLight idx
    r <- Lib.rand
    time <- use (globals.gCl.csTime)
    modifyRef dLightRef (\v -> v & cdlOrigin .~ origin
                                 & cdlRadius .~ 200 + fromIntegral (r .&. 31)
                                 & cdlMinLight .~ 32
                                 & cdlDie .~ fromIntegral time)
    muzzleFlashSound2 dLightRef (Ref idx) origin flashNumber
  where
    (forward, right, _) = Math3D.angleVectors (cent^.ceCurrent.esAngles) True True False
    a = (cent^.ceCurrent.esOrigin._x) + (forward^._x) * ((MFlash.monsterFlashOffset V.! flashNumber)^._x) + (right^._x) * ((MFlash.monsterFlashOffset V.! flashNumber)^._y)
    b = (cent^.ceCurrent.esOrigin._y) + (forward^._y) * ((MFlash.monsterFlashOffset V.! flashNumber)^._x) + (right^._y) * ((MFlash.monsterFlashOffset V.! flashNumber)^._y)
    c = (cent^.ceCurrent.esOrigin._z) + (forward^._z) * ((MFlash.monsterFlashOffset V.! flashNumber)^._x) + (right^._z) * ((MFlash.monsterFlashOffset V.! flashNumber)^._y) + ((MFlash.monsterFlashOffset V.! flashNumber)^._z)
    origin = V3 a b c

muzzleFlashSound2 :: Ref CDLightT -> Ref EdictT -> V3 Float -> Int -> Quake ()
muzzleFlashSound2 dLightRef edictRef origin flashNumber
    | flashNumber `elem` [ Constants.mz2InfantryMachinegun1
                         , Constants.mz2InfantryMachinegun2
                         , Constants.mz2InfantryMachinegun3
                         , Constants.mz2InfantryMachinegun4
                         , Constants.mz2InfantryMachinegun5
                         , Constants.mz2InfantryMachinegun6
                         , Constants.mz2InfantryMachinegun7
                         , Constants.mz2InfantryMachinegun8
                         , Constants.mz2InfantryMachinegun9
                         , Constants.mz2InfantryMachinegun10
                         , Constants.mz2InfantryMachinegun11
                         , Constants.mz2InfantryMachinegun12
                         , Constants.mz2InfantryMachinegun13
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
        soundRef <- S.registerSound "infantry/infatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2SoldierMachinegun1
                         , Constants.mz2SoldierMachinegun2
                         , Constants.mz2SoldierMachinegun3
                         , Constants.mz2SoldierMachinegun4
                         , Constants.mz2SoldierMachinegun5
                         , Constants.mz2SoldierMachinegun6
                         , Constants.mz2SoldierMachinegun7
                         , Constants.mz2SoldierMachinegun8
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
        soundRef <- S.registerSound "soldier/solatck3.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2GunnerMachinegun1
                         , Constants.mz2GunnerMachinegun2
                         , Constants.mz2GunnerMachinegun3
                         , Constants.mz2GunnerMachinegun4
                         , Constants.mz2GunnerMachinegun5
                         , Constants.mz2GunnerMachinegun6
                         , Constants.mz2GunnerMachinegun7
                         , Constants.mz2GunnerMachinegun8
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
        soundRef <- S.registerSound "gunner/gunatck2.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2ActorMachinegun1
                         , Constants.mz2SupertankMachinegun1
                         , Constants.mz2SupertankMachinegun2
                         , Constants.mz2SupertankMachinegun3
                         , Constants.mz2SupertankMachinegun4
                         , Constants.mz2SupertankMachinegun5
                         , Constants.mz2SupertankMachinegun6
                         , Constants.mz2TurretMachinegun
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
        soundRef <- S.registerSound "infantry/infatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2Boss2MachinegunL1
                         , Constants.mz2Boss2MachinegunL2
                         , Constants.mz2Boss2MachinegunL3
                         , Constants.mz2Boss2MachinegunL4
                         , Constants.mz2Boss2MachinegunL5
                         , Constants.mz2CarrierMachinegunL1
                         , Constants.mz2CarrierMachinegunL2
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
        soundRef <- S.registerSound "infantry/infatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNone 0
    | flashNumber `elem` [ Constants.mz2SoldierBlaster1
                         , Constants.mz2SoldierBlaster2
                         , Constants.mz2SoldierBlaster3
                         , Constants.mz2SoldierBlaster4
                         , Constants.mz2SoldierBlaster5
                         , Constants.mz2SoldierBlaster6
                         , Constants.mz2SoldierBlaster7
                         , Constants.mz2SoldierBlaster8
                         , Constants.mz2TurretBlaster
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "soldier/solatck2.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2FlyerBlaster1
                         , Constants.mz2FlyerBlaster2
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "flyer/flyatck3.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber == Constants.mz2MedicBlaster1 = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "medic/medatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber == Constants.mz2HoverBlaster1 = do
        modifyRef dLightRef (\v -> v { _cdlColor = V3 1 1 0 })
        soundRef <- S.registerSound "hover/hovatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber == Constants.mz2FloatBlaster1 = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "floater/fltatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2SoldierShotgun1
                         , Constants.mz2SoldierShotgun2
                         , Constants.mz2SoldierShotgun3
                         , Constants.mz2SoldierShotgun4
                         , Constants.mz2SoldierShotgun5
                         , Constants.mz2SoldierShotgun6
                         , Constants.mz2SoldierShotgun7
                         , Constants.mz2SoldierShotgun8
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        CLTEnt.smokeAndFlash origin
        soundRef <- S.registerSound "soldier/solatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0

    | flashNumber `elem` [ Constants.mz2TankBlaster1
                         , Constants.mz2TankBlaster2
                         , Constants.mz2TankBlaster3
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "tank/tnkatck3.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2TankMachinegun1
                         , Constants.mz2TankMachinegun2
                         , Constants.mz2TankMachinegun3
                         , Constants.mz2TankMachinegun4
                         , Constants.mz2TankMachinegun5
                         , Constants.mz2TankMachinegun6
                         , Constants.mz2TankMachinegun7
                         , Constants.mz2TankMachinegun8
                         , Constants.mz2TankMachinegun9
                         , Constants.mz2TankMachinegun10
                         , Constants.mz2TankMachinegun11
                         , Constants.mz2TankMachinegun12
                         , Constants.mz2TankMachinegun13
                         , Constants.mz2TankMachinegun14
                         , Constants.mz2TankMachinegun15
                         , Constants.mz2TankMachinegun16
                         , Constants.mz2TankMachinegun17
                         , Constants.mz2TankMachinegun18
                         , Constants.mz2TankMachinegun19
                         ] = do
        modifyRef dLightRef (\v -> v { _cdlColor = V3 1 1 0 })
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
        r <- Lib.rand
        soundRef <- S.registerSound (B.concat ["tank/tnkatk2", B.singleton (97 + fromIntegral (r `mod` 5)), ".wav"])
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2ChickRocket1
                         , Constants.mz2TurretRocket
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0.2)
        soundRef <- S.registerSound "chick/chkatck2.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2TankRocket1
                         , Constants.mz2TankRocket2
                         , Constants.mz2TankRocket3
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0.2)
        soundRef <- S.registerSound "tank/tnkatck1.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2SupertankRocket1
                         , Constants.mz2SupertankRocket2
                         , Constants.mz2SupertankRocket3
                         , Constants.mz2Boss2Rocket1
                         , Constants.mz2Boss2Rocket2
                         , Constants.mz2Boss2Rocket3
                         , Constants.mz2Boss2Rocket4
                         , Constants.mz2CarrierRocket1
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0.2)
        soundRef <- S.registerSound "tank/rocket.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2GunnerGrenade1
                         , Constants.mz2GunnerGrenade2
                         , Constants.mz2GunnerGrenade3
                         , Constants.mz2GunnerGrenade4
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 0.5 0)
        soundRef <- S.registerSound "gunner/gunatck3.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2GladiatorRailgun1
                         , Constants.mz2CarrierRailgun
                         , Constants.mz2WidowRail
                         ] =
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0.5 0.5 1.0)
    | flashNumber == Constants.mz2MakronBfg =
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0.5 1 0.5)
    | flashNumber `elem` [ Constants.mz2MakronBlaster1
                         , Constants.mz2MakronBlaster2
                         , Constants.mz2MakronBlaster3
                         , Constants.mz2MakronBlaster4
                         , Constants.mz2MakronBlaster5
                         , Constants.mz2MakronBlaster6
                         , Constants.mz2MakronBlaster7
                         , Constants.mz2MakronBlaster8
                         , Constants.mz2MakronBlaster9
                         , Constants.mz2MakronBlaster10
                         , Constants.mz2MakronBlaster11
                         , Constants.mz2MakronBlaster12
                         , Constants.mz2MakronBlaster13
                         , Constants.mz2MakronBlaster14
                         , Constants.mz2MakronBlaster15
                         , Constants.mz2MakronBlaster16
                         , Constants.mz2MakronBlaster17
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        soundRef <- S.registerSound "makron/blaster.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2JorgMachinegunL1
                         , Constants.mz2JorgMachinegunL2
                         , Constants.mz2JorgMachinegunL3
                         , Constants.mz2JorgMachinegunL4
                         , Constants.mz2JorgMachinegunL5
                         , Constants.mz2JorgMachinegunL6
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
        soundRef <- S.registerSound "boss3/xfire.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2JorgMachinegunR1
                         , Constants.mz2JorgMachinegunR2
                         , Constants.mz2JorgMachinegunR3
                         , Constants.mz2JorgMachinegunR4
                         , Constants.mz2JorgMachinegunR5
                         , Constants.mz2JorgMachinegunR6
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
    | flashNumber == Constants.mz2JorgBfg1 =
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0.5 1 0.5)
    | flashNumber `elem` [ Constants.mz2Boss2MachinegunR1
                         , Constants.mz2Boss2MachinegunR2
                         , Constants.mz2Boss2MachinegunR3
                         , Constants.mz2Boss2MachinegunR4
                         , Constants.mz2Boss2MachinegunR5
                         , Constants.mz2CarrierMachinegunR1
                         , Constants.mz2CarrierMachinegunR2
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0)
        v3o <- use (globals.gVec3Origin)
        particleEffect origin v3o 0 40
        CLTEnt.smokeAndFlash origin
    | flashNumber `elem` [ Constants.mz2StalkerBlaster
                         , Constants.mz2DaedalusBlaster
                         , Constants.mz2MedicBlaster2
                         , Constants.mz2WidowBlaster
                         , Constants.mz2WidowBlasterSweep1
                         , Constants.mz2WidowBlasterSweep2
                         , Constants.mz2WidowBlasterSweep3
                         , Constants.mz2WidowBlasterSweep4
                         , Constants.mz2WidowBlasterSweep5
                         , Constants.mz2WidowBlasterSweep6
                         , Constants.mz2WidowBlasterSweep7
                         , Constants.mz2WidowBlasterSweep8
                         , Constants.mz2WidowBlasterSweep9
                         , Constants.mz2WidowBlaster100
                         , Constants.mz2WidowBlaster90
                         , Constants.mz2WidowBlaster80
                         , Constants.mz2WidowBlaster70
                         , Constants.mz2WidowBlaster60
                         , Constants.mz2WidowBlaster50
                         , Constants.mz2WidowBlaster40
                         , Constants.mz2WidowBlaster30
                         , Constants.mz2WidowBlaster20
                         , Constants.mz2WidowBlaster10
                         , Constants.mz2WidowBlaster0
                         , Constants.mz2WidowBlaster10L
                         , Constants.mz2WidowBlaster20L
                         , Constants.mz2WidowBlaster30L
                         , Constants.mz2WidowBlaster40L
                         , Constants.mz2WidowBlaster50L
                         , Constants.mz2WidowBlaster60L
                         , Constants.mz2WidowBlaster70L
                         , Constants.mz2WidowRun1
                         , Constants.mz2WidowRun2
                         , Constants.mz2WidowRun3
                         , Constants.mz2WidowRun4
                         , Constants.mz2WidowRun5
                         , Constants.mz2WidowRun6
                         , Constants.mz2WidowRun7
                         , Constants.mz2WidowRun8
                         ] = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 0 1 0)
        soundRef <- S.registerSound "tank/tnkatck3.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber == Constants.mz2WidowDisruptor = do
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 (-1) (-1) (-1))
        soundRef <- S.registerSound "weapons/disint2.wav"
        S.startSound Nothing edictRef Constants.chanWeapon soundRef 1 Constants.attnNorm 0
    | flashNumber `elem` [ Constants.mz2WidowPlasmaBeam
                         , Constants.mz2Widow2Beamer1
                         , Constants.mz2Widow2Beamer2
                         , Constants.mz2Widow2Beamer3
                         , Constants.mz2Widow2Beamer4
                         , Constants.mz2Widow2Beamer5
                         , Constants.mz2Widow2BeamSweep1
                         , Constants.mz2Widow2BeamSweep2
                         , Constants.mz2Widow2BeamSweep3
                         , Constants.mz2Widow2BeamSweep4
                         , Constants.mz2Widow2BeamSweep5
                         , Constants.mz2Widow2BeamSweep6
                         , Constants.mz2Widow2BeamSweep7
                         , Constants.mz2Widow2BeamSweep8
                         , Constants.mz2Widow2BeamSweep9
                         , Constants.mz2Widow2BeamSweep10
                         , Constants.mz2Widow2BeamSweep11
                         ] = do
        r <- Lib.rand
        time <- use (globals.gCl.csTime)
        modifyRef dLightRef (\v -> v & cdlColor .~ V3 1 1 0
                                     & cdlRadius .~ 300 + fromIntegral (r .&. 100)
                                     & cdlDie .~ fromIntegral time + 200)
       | otherwise = return () -- TODO: throw some kind of error ??

clearEffects :: Quake ()
clearEffects = do
    clearParticles
    clearDLights
    clearLightStyles

clearParticles :: Quake ()
clearParticles = do
    clientGlobals.cgFreeParticles .= Just (Ref 0)
    clientGlobals.cgActiveParticles .= Nothing
    doClearParticles

doClearParticles :: Quake ()
doClearParticles = do
    particles <- use (clientGlobals.cgParticles)
    io (mapM_ (clearParticle particles) [0..MV.length particles-1])
  where
    clearParticle particles idx
        | idx == MV.length particles - 1 =
            MV.modify particles (\v -> v & cpNext .~ Nothing) idx
        | otherwise =
            MV.modify particles (\v -> v & cpNext .~ Just (Ref (idx + 1))) idx 

clearDLights :: Quake ()
clearDLights = doClearDLights

doClearDLights :: Quake ()
doClearDLights = do
    dlights <- use (clientGlobals.cgDLights)
    io (MV.set dlights newCDLightT)

clearLightStyles :: Quake ()
clearLightStyles = do
    clientGlobals.cgLightStyle .= V.replicate Constants.maxLightStyles newCLightStyleT
    clientGlobals.cgLastOfs .= -1

setLightStyle :: Int -> Quake ()
setLightStyle csIdx = do
    str <- preuse (globals.gCl.csConfigStrings.ix (csIdx + Constants.csLights))
    maybe configStringError doSetLightStyle str
  where
    configStringError = Com.fatalError "CLFX.setLightStyle str is Nothing"
    doSetLightStyle str = do
        when (B.length str >= Constants.maxQPath) $
          Com.comError Constants.errDrop ("svc_lightstyle length=" `B.append` encode (B.length str))
        clientGlobals.cgLightStyle.ix csIdx %= (\v -> v & clsLength .~ B.length str
                                                        & clsMap .~ UV.unfoldrN (B.length str) (buildLightStyle str (fromIntegral (ord 'm' - ord 'a'))) 0)
    buildLightStyle str d idx =
        let a = fromIntegral (ord (str `BC.index` idx) - ord 'a') :: Float
        in Just (a / d, idx + 1)

allocDLight :: Int -> Quake (Ref CDLightT)
allocDLight key = do
    exactMatch <- findExactMatch 0 Constants.maxDLights
    maybe searchForAnyDLight return exactMatch
  where
    findExactMatch idx maxIdx
        | idx >= maxIdx = return Nothing
        | otherwise = do
            dLight <- readRef (Ref idx)
            case (dLight^.cdlKey) == key of
                True -> do
                    writeRef (Ref idx) (newCDLightT & cdlKey .~ key)
                    return (Just (Ref idx))
                False -> findExactMatch (idx + 1) maxIdx
    searchForAnyDLight = do
        time <- use (globals.gCl.csTime)
        anyMatch <- findAnyDLight (fromIntegral time) 0 Constants.maxDLights
        maybe defaultDLight return anyMatch
    defaultDLight = do
        writeRef (Ref 0) (newCDLightT & cdlKey .~ key)
        return (Ref 0)
    findAnyDLight time idx maxIdx
        | idx >= maxIdx = return Nothing
        | otherwise = do
            dLight <- readRef (Ref idx)
            case (dLight^.cdlDie) < time of
                True -> do
                    writeRef (Ref idx) (newCDLightT & cdlKey .~ key)
                    return (Just (Ref idx))
                False -> findAnyDLight time (idx + 1) maxIdx

logoutEffect :: V3 Float -> Int -> Quake ()
logoutEffect org pType = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addLogoutEffect org pType freeParticles 0 500

addLogoutEffect :: V3 Float -> Int -> Maybe (Ref CParticleT) -> Int -> Int -> Quake ()
addLogoutEffect _ _ Nothing _ _ = return ()
addLogoutEffect org pType (Just pRef) idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        p <- readRef pRef
        activeParticles <- use (clientGlobals.cgActiveParticles)
        clientGlobals.cgFreeParticles .= (p^.cpNext)
        clientGlobals.cgActiveParticles .= Just pRef
        time <- use (globals.gCl.csTime)
        r <- Lib.rand
        f <- Lib.randomF
        o1 <- Lib.randomF
        o2 <- Lib.randomF
        o3 <- Lib.randomF
        v1 <- Lib.crandom
        v2 <- Lib.crandom
        v3 <- Lib.crandom
        modifyRef pRef (\v -> v & cpNext .~ activeParticles
                                & cpTime .~ fromIntegral time
                                & cpColor .~ pickColor (fromIntegral (r .&. 7))
                                & cpOrg .~ V3 ((org^._x) - 16 + o1 * 32) ((org^._y) - 16 + o2 * 32) ((org^._z) - 24 + o3 * 56)
                                & cpVel .~ fmap (* 20) (V3 v1 v2 v3)
                                & cpAccel .~ V3 0 0 (- particleGravity)
                                & cpAlpha .~ 1.0
                                & cpAlphaVel .~ (-1.0) / (1.0 + f * 0.3))
        addLogoutEffect org pType (p^.cpNext) (idx + 1) maxIdx
  where
    pickColor r
        | pType == Constants.mzLogin = 0xD0 + r -- green
        | pType == Constants.mzLogout = 0x40 + r -- red
        | otherwise = 0xE0 + r -- yellow

particleEffect :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
particleEffect org dir color count = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addParticleEffects org dir color count freeParticles 0

addParticleEffects :: V3 Float -> V3 Float -> Int -> Int -> Maybe (Ref CParticleT) -> Int -> Quake ()
addParticleEffects _ _ _ _ Nothing _ = return ()
addParticleEffects org dir color count (Just pRef) idx
    | idx >= count = return ()
    | otherwise = do
        p <- readRef pRef
        activeParticles <- use (clientGlobals.cgActiveParticles)
        clientGlobals.cgFreeParticles .= (p^.cpNext)
        clientGlobals.cgActiveParticles .= Just pRef
        time <- use (globals.gCl.csTime)
        r <- Lib.rand
        r' <- Lib.randomF
        d <- fmap (fromIntegral . (.&. 31)) Lib.rand
        o1 <- Lib.rand
        v1 <- Lib.crandom
        o2 <- Lib.rand
        v2 <- Lib.crandom
        o3 <- Lib.rand
        v3 <- Lib.crandom
        writeRef pRef (newCParticleT & cpNext .~ activeParticles
                                     & cpTime .~ fromIntegral time
                                     & cpColor .~ fromIntegral (color + fromIntegral (r .&. 7))
                                     & cpOrg .~ org + fmap (fromIntegral . (subtract 4) . (.&. 7)) (V3 o1 o2 o3) + fmap (* d) dir
                                     & cpVel .~ fmap (* 20) (V3 v1 v2 v3)
                                     & cpAccel .~ V3 0 0 (- particleGravity)
                                     & cpAlpha .~ 1.0
                                     & cpAlphaVel .~ -1 / (0.5 + r' * 0.3))
        addParticleEffects org dir color count (p^.cpNext) (idx + 1)

entityEvent :: EntityStateT -> Quake ()
entityEvent entityState
    | (entityState^.esEvent) == Constants.evItemRespawn = do
        sfx <- S.registerSound "items/respawn1.wav"
        S.startSound Nothing (Ref (entityState^.esNumber)) Constants.chanWeapon sfx 1 Constants.attnIdle 0
        itemRespawnParticles (entityState^.esOrigin)
    | (entityState^.esEvent) == Constants.evPlayerTeleport = do
        sfx <- S.registerSound "misc/tele1.wav"
        S.startSound Nothing (Ref (entityState^.esNumber)) Constants.chanWeapon sfx 1 Constants.attnIdle 0
        teleportParticles (entityState^.esOrigin)
    | (entityState^.esEvent) == Constants.evFootstep = do
        footsteps <- fmap (^.cvValue) clFootstepsCVar
        when (footsteps /= 0) $ do
          r <- Lib.rand
          sfxFootsteps <- use (clTEntGlobals.clteSfxFootsteps)
          S.startSound Nothing (Ref (entityState^.esNumber)) Constants.chanBody (sfxFootsteps V.! fromIntegral (r .&. 3)) 1 Constants.attnNorm 0
    | (entityState^.esEvent) == Constants.evFallShort = do
        sfx <- S.registerSound "player/land1.wav"
        S.startSound Nothing (Ref (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0
    | (entityState^.esEvent) == Constants.evFall = do
        sfx <- S.registerSound "*fall2.wav"
        S.startSound Nothing (Ref (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0
    | (entityState^.esEvent) == Constants.evFallFar = do
        sfx <- S.registerSound "*fall1.wav"
        S.startSound Nothing (Ref (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0
    | otherwise =
        return () -- TODO: expected?

teleporterParticles :: EntityStateT -> Quake ()
teleporterParticles ent = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addTeleporterParticles ent freeParticles 0 8

addTeleporterParticles :: EntityStateT -> Maybe (Ref CParticleT) -> Int -> Int -> Quake ()
addTeleporterParticles _ Nothing _ _ = return ()
addTeleporterParticles ent (Just pRef) idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        p <- readRef pRef
        activeParticles <- use (clientGlobals.cgActiveParticles)
        clientGlobals.cgFreeParticles .= (p^.cpNext)
        clientGlobals.cgActiveParticles .= Just pRef
        time <- use (globals.gCl.csTime)
        o1 <- Lib.rand
        o2 <- Lib.rand
        o3 <- Lib.rand
        v1 <- Lib.crandom
        v2 <- Lib.crandom
        v3 <- Lib.rand
        modifyRef pRef (\v -> v & cpNext .~ activeParticles
                                & cpTime .~ fromIntegral time
                                & cpColor .~ 0xDB
                                & cpOrg .~ V3 ((ent^.esOrigin._x) - 16 + fromIntegral (o1 .&. 31)) ((ent^.esOrigin._y) - 16 + fromIntegral (o2 .&. 31)) ((ent^.esOrigin._z) - 8 + fromIntegral (o3 .&. 7))
                                & cpVel .~ V3 (v1 * 14) (v2 * 14) (80 + fromIntegral (v3 .&. 3))
                                & cpAccel .~ V3 0 0 (- particleGravity)
                                & cpAlpha .~ 1.0
                                & cpAlphaVel .~ -0.5)
        addTeleporterParticles ent (p^.cpNext) (idx + 1) maxIdx

itemRespawnParticles :: V3 Float -> Quake ()
itemRespawnParticles org = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addItemRespawnParticles org freeParticles 0 64

addItemRespawnParticles :: V3 Float -> Maybe (Ref CParticleT) -> Int -> Int -> Quake ()
addItemRespawnParticles _ Nothing _ _ = return ()
addItemRespawnParticles org (Just pRef) idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        p <- readRef pRef
        activeParticles <- use (clientGlobals.cgActiveParticles)
        clientGlobals.cgFreeParticles .= (p^.cpNext)
        clientGlobals.cgActiveParticles .= Just pRef
        time <- use (globals.gCl.csTime)
        color <- fmap (.&. 3) Lib.rand
        o1 <- Lib.crandom
        o2 <- Lib.crandom
        o3 <- Lib.crandom
        v1 <- Lib.crandom
        v2 <- Lib.crandom
        v3 <- Lib.crandom
        f <- Lib.randomF
        modifyRef pRef (\v -> v & cpNext .~ activeParticles
                                & cpTime .~ fromIntegral time
                                & cpColor .~ 0xD4 + fromIntegral color -- green
                                & cpOrg .~ V3 ((org^._x) + o1 * 8) ((org^._y) + o2 * 8) ((org^._z) + o3 * 8)
                                & cpVel .~ V3 (v1 * 8) (v2 * 8) (v3 * 8)
                                & cpAccel .~ V3 0 0 ((-0.2) * particleGravity)
                                & cpAlpha .~ 1.0
                                & cpAlphaVel .~ (-1.0) / (1.0 + f * 0.3))
        addItemRespawnParticles org (p^.cpNext) (idx + 1) maxIdx

teleportParticles :: V3 Float -> Quake ()
teleportParticles org = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addTeleportParticles org freeParticles (-16) (-16) (-16)

addTeleportParticles :: V3 Float -> Maybe (Ref CParticleT) -> Int -> Int -> Int -> Quake ()
addTeleportParticles _ Nothing _ _ _ = return ()
addTeleportParticles org (Just pRef) i j k
    | i > 16 = return ()
    | j > 16 = addTeleportParticles org (Just pRef) (i + 4) (-16) (-16)
    | k > 32 = addTeleportParticles org (Just pRef) i (j + 4) (-16)
    | otherwise = do
        p <- readRef pRef
        activeParticles <- use (clientGlobals.cgActiveParticles)
        clientGlobals.cgFreeParticles .= (p^.cpNext)
        clientGlobals.cgActiveParticles .= Just pRef
        time <- use (globals.gCl.csTime)
        color <- fmap (.&. 7) Lib.rand
        av <- Lib.rand
        o1 <- Lib.rand
        o2 <- Lib.rand
        o3 <- Lib.rand
        vel <- fmap (\r -> 50 + fromIntegral (r .&. 63)) Lib.rand
        modifyRef pRef (\v -> v & cpNext .~ activeParticles
                                & cpTime .~ fromIntegral time
                                & cpColor .~ 7 + fromIntegral color
                                & cpAlpha .~ 1.0
                                & cpAlphaVel .~ (-1.0) / (0.3 + fromIntegral (av .&. 7) * 0.02)
                                & cpOrg .~ V3 ((org^._x) + fromIntegral i + fromIntegral (o1 .&. 3)) ((org^._y) + fromIntegral j + fromIntegral (o2 .&. 3)) ((org^._z) + fromIntegral k + fromIntegral (o3 .&. 3))
                                & cpVel .~ fmap (* vel) (normalize (fmap fromIntegral (V3 (j * 8) (i * 8) (k * 8))))
                                & cpAccel .~ V3 0 0 (- particleGravity))
        addTeleportParticles org (p^.cpNext) i j (k + 4)

addParticles :: Quake ()
addParticles = do
    cl <- use (globals.gCl)
    activeParticles <- use (clientGlobals.cgActiveParticles)
    doAddParticles cl activeParticles Nothing Nothing 0

doAddParticles :: ClientStateT -> Maybe (Ref CParticleT) -> Maybe (Ref CParticleT) -> Maybe (Ref CParticleT) -> Float -> Quake ()
doAddParticles _ Nothing activeRef _ _ = clientGlobals.cgActiveParticles .= activeRef
doAddParticles cl (Just pRef) activeRef tailRef time = do
    p <- readRef pRef
    (done, time', alpha) <- checkInstantParticle p
    proceedAddParticles p done time' alpha
  where
    checkInstantParticle p
        | (p^.cpAlphaVel) /= instantParticle = do
            let time' = (fromIntegral (cl^.csTime) - (p^.cpTime)) * 0.001
                alpha = (p^.cpAlpha) + time' * (p^.cpAlphaVel)
            checkFadedOut time' alpha
        | otherwise =
            return (False, time, p^.cpAlpha)
    checkFadedOut time' alpha
        | alpha <= 0 = do
            freeParticles <- use (clientGlobals.cgFreeParticles)
            modifyRef pRef (\v -> v & cpNext .~ freeParticles)
            clientGlobals.cgFreeParticles .= Just pRef
            return (True, time', alpha)
        | otherwise =
            return (False, time', alpha)
    proceedAddParticles p done time' alpha
        | done = doAddParticles cl (p^.cpNext) activeRef tailRef time'
        | otherwise = do
            modifyRef pRef (\v -> v & cpNext .~ Nothing)
            (activeRef', tailRef') <- checkTailRef tailRef
            let alpha' = min 1 alpha
                color = truncate (p^.cpColor) :: Int
                time2 = time' * time'
                org = (p^.cpOrg) + fmap (* time') (p^.cpVel) + fmap (* time2) (p^.cpAccel)
            ClientV.addParticle org color alpha'
            when ((p^.cpAlphaVel) == instantParticle) $
                modifyRef pRef (\v -> v & cpAlpha .~ 0
                                        & cpAlphaVel .~ 0)
            doAddParticles cl (p^.cpNext) activeRef' tailRef' time'
    checkTailRef Nothing = return (Just pRef, Just pRef)
    checkTailRef (Just ref) = do
        modifyRef ref (\v -> v & cpNext .~ Just pRef)
        return (activeRef, Just pRef)

addDLights :: Quake ()
addDLights =
    -- TODO: currently simplified version... need to update it to reflect
    -- jake2 version correctly
    mapM_ addDLight (fmap Ref [0..Constants.maxDLights-1])
  where
    addDLight dlRef = doAddDLight =<< readRef dlRef
    doAddDLight dl
        | (dl^.cdlRadius) == 0 = return ()
        | otherwise = ClientV.addLight (dl^.cdlOrigin) (dl^.cdlRadius) (dl^.cdlColor._x) (dl^.cdlColor._y) (dl^.cdlColor._z)

addLightStyles :: Quake ()
addLightStyles = do
    lightStyles <- use (clientGlobals.cgLightStyle)
    vGlobals.vgLightStyles .= V.map toMLightStyle lightStyles
  where
    toMLightStyle lightStyle = LightStyleT (lightStyle^.clsValue) (sum (lightStyle^.clsValue))

rocketTrail :: V3 Float -> V3 Float -> Int -> Quake ()
rocketTrail = error "CLFX.rocketTrail" -- TODO

blasterTrail :: V3 Float -> V3 Float -> Quake ()
blasterTrail start end = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    trailParticles freeParticles start (fmap (* 5) (normalize (end - start))) (norm (end - start))
  where
    trailParticles Nothing _ _ _ = return ()
    trailParticles (Just pRef) move vec len
        | len <= 0 = return ()
        | otherwise = do
            activeParticles <- use (clientGlobals.cgActiveParticles)
            time <- use (globals.gCl.csTime)
            p <- readRef pRef
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            modifyRef pRef (\v -> v & cpNext .~ activeParticles)
            clientGlobals.cgActiveParticles .= Just pRef
            f <- Lib.randomF
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            v1 <- Lib.crandom
            v2 <- Lib.crandom
            v3 <- Lib.crandom
            modifyRef pRef (\v -> v & cpTime     .~ fromIntegral time
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (0.3 + f * 0.2)
                                    & cpColor    .~ 0xE0
                                    & cpOrg      .~ move + V3 o1 o2 o3
                                    & cpVel      .~ V3 (5 * v1) (5 * v2) (5 * v3)
                                    & cpAccel    .~ V3 0 0 0)

            trailParticles (p^.cpNext) (move + vec) vec (len - 5)

diminishingTrail :: V3 Float -> V3 Float -> Ref CEntityT -> Int -> Quake ()
diminishingTrail start end oldRef flags = do
    old <- readRef oldRef
    let (orgScale, velScale) = checkTrailCount old
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addDiminishingTrail freeParticles old (fmap (* 0.5) (normalize (end - start))) start orgScale velScale (norm (end - start))
  where
    checkTrailCount old
        | (old^.ceTrailCount) > 900 = (4, 15)
        | (old^.ceTrailCount) > 800 = (2, 10)
        | otherwise                 = (1, 5)
    addDiminishingTrail Nothing _ _ _ _ _ _ = return ()
    addDiminishingTrail (Just pRef) old vec move orgScale velScale len
        | len <= 0 = return ()
        | otherwise = do
            r <- Lib.rand
            doAddDiminishingTrail pRef old vec move orgScale velScale len r
    doAddDiminishingTrail pRef old vec move orgScale velScale len r
        | fromIntegral (r .&. 1023) < (old^.ceTrailCount) = do
            p <- readRef pRef
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            f <- Lib.randomF
            color <- Lib.rand
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            v1 <- Lib.crandom
            v2 <- Lib.crandom
            v3 <- Lib.crandom
            updateParticle pRef activeParticles move orgScale velScale time f color o1 o2 o3 v1 v2 v3
            modifyRef oldRef (\v -> v & ceTrailCount %~ (\v -> if v - 5 < 100 then 100 else v - 5)) 
            addDiminishingTrail (p^.cpNext) old vec (move + vec) orgScale velScale (len - 0.5)
        | otherwise = do
            modifyRef oldRef (\v -> v & ceTrailCount %~ (\v -> if v - 5 < 100 then 100 else v - 5))
            addDiminishingTrail (Just pRef) old vec (move + vec) orgScale velScale (len - 0.5)
    updateParticle pRef activeParticles move orgScale velScale time f color o1 o2 o3 v1 v2 v3
        | flags .&. Constants.efGib /= 0 =
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 0
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (1.0 + f * 0.4)
                                    & cpColor    .~ 0xE8 + fromIntegral (color .&. 7)
                                    & cpOrg      .~ move + fmap (* orgScale) (V3 o1 o2 o3)
                                    & cpVel      .~ ((fmap (* velScale) (V3 v1 v2 v3)) & _z -~ particleGravity))
        | flags .&. Constants.efGreenGib /= 0 =
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 0
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (1.0 + f * 0.4)
                                    & cpColor    .~ 0xDB + fromIntegral (color .&. 7)
                                    & cpOrg      .~ move + fmap (* orgScale) (V3 o1 o2 o3)
                                    & cpVel      .~ ((fmap (* velScale) (V3 v1 v2 v3)) & _z -~ particleGravity))
        | otherwise =
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 20
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (1.0 + f * 0.2)
                                    & cpColor    .~ 4 + fromIntegral (color .&. 7)
                                    & cpOrg      .~ move + fmap (* orgScale) (V3 o1 o2 o3)
                                    & cpVel      .~ fmap (* velScale) (V3 v1 v2 v3))

flyEffect :: Int -> V3 Float -> Quake ()
flyEffect = error "CLFX.flyEffect" -- TODO

flagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
flagTrail = error "CLFX.flagTrail" -- TODO

bfgParticles :: EntityT -> Quake ()
bfgParticles = error "CLFX.bfgParticles" -- TODO

trapParticles :: EntityT -> Quake ()
trapParticles = error "CLFX.trapParticles" -- TODO

ionRipperTrail :: V3 Float -> V3 Float -> Quake ()
ionRipperTrail = error "CLFX.ionRipperTrail" -- TODO

particleEffect2 :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
particleEffect2 = error "CLFX.particleEffect2" -- TODO

particleEffect3 :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
particleEffect3 = error "CLFX.particleEffect3" -- TODO

blasterParticles :: V3 Float -> V3 Float -> Quake ()
blasterParticles org dir = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    drawParticles freeParticles 0 40
  where
    drawParticles :: Maybe (Ref CParticleT) -> Int -> Int -> Quake ()
    drawParticles Nothing _ _ = return ()
    drawParticles (Just pRef) idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            modifyRef pRef (\v -> v & cpNext .~ activeParticles)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            r <- Lib.rand
            d <- fmap (fromIntegral . (.&. 15)) Lib.rand
            o1 <- Lib.rand
            o2 <- Lib.rand
            o3 <- Lib.rand
            v1 <- Lib.crandom
            v2 <- Lib.crandom
            v3 <- Lib.crandom
            f <- Lib.randomF
            modifyRef pRef (\v -> v & cpTime     .~ fromIntegral time
                                    & cpColor    .~ 0xE0 + fromIntegral (r .&. 7)
                                    & cpOrg      .~ org + (V3 (fromIntegral ((o1 .&. 7) - 4)) (fromIntegral ((o2 .&. 7) - 4)) (fromIntegral ((o3 .&. 7) - 4))) + fmap (* d) dir
                                    & cpVel      .~ fmap (* 30) dir + fmap (* 40) (V3 v1 v2 v3)
                                    & cpAccel    .~ V3 0 0 (- particleGravity)
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (0.5 + f * 0.3))
            drawParticles (p^.cpNext) (idx + 1) maxIdx

railTrail :: V3 Float -> V3 Float -> Quake ()
railTrail = error "CLFX.railTrail" -- TODO

explosionParticles :: V3 Float -> Quake ()
explosionParticles = error "CLFX.explosionParticles" -- TODO

bfgExplosionParticles :: V3 Float -> Quake ()
bfgExplosionParticles = error "CLFX.bfgExplosionParticles" -- TODO

bubbleTrail :: V3 Float -> V3 Float -> Quake ()
bubbleTrail = error "CLFX.bubbleTrail" -- TODO

bigTeleportParticles :: V3 Float -> Quake ()
bigTeleportParticles = error "CLFX.bigTeleportParticles" -- TODO
