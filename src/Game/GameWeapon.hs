module Game.GameWeapon
    ( fireBlaster
    , fireHit
    ) where

import           Control.Lens          (use, (^.), (&), (.~), (%~))
import           Control.Monad         (when, unless)
import           Data.Bits             ((.|.), (.&.))
import           Data.Maybe            (isJust)
import           Linear                (V3(..), normalize, norm, _x)

import qualified Constants
import           Game.CPlaneT
import           Game.CSurfaceT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameCombat       as GameCombat
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import           Game.MonsterInfoT
import qualified Game.PlayerWeapon     as PlayerWeapon
import           Game.TraceT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib

fireBlaster :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()
fireBlaster selfRef start direction damage speed effect hyper = do
    self <- readRef selfRef
    boltRef <- GameUtil.spawn
    gameImport <- use (gameBaseGlobals.gbGameImport)
    -- yes, I know it looks weird that projectiles are deadmonsters
    -- what this means is that when prediction is used against the object
    -- (blaster/hyperblaster shots), the player won't be solid clipped
    -- against the object. Right now trying to run into a firing hyperblaster
    -- is very jerky since you are predicted 'against' the shots.
    modelIdx <- (gameImport^.giModelIndex) (Just "models/objects/laser/tris.md2")
    soundIdx <- (gameImport^.giSoundIndex) (Just "misc/lasfly.wav")
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef boltRef (\v -> v & eSvFlags .~ Constants.svfDeadMonster
                               & eEntityState.esOrigin .~ start
                               & eEntityState.esOldOrigin .~ start
                               & eEntityState.esAngles .~ dir
                               & eVelocity .~ fmap (* (fromIntegral speed)) dir
                               & eMoveType .~ Constants.moveTypeFlyMissile
                               & eClipMask .~ Constants.maskShot
                               & eSolid .~ Constants.solidBbox
                               & eEntityState.esEffects %~ (.|. effect)
                               & eMins .~ V3 0 0 0
                               & eMaxs .~ V3 0 0 0
                               & eEntityState.esModelIndex .~ modelIdx
                               & eEntityState.esSound .~ soundIdx
                               & eOwner .~ Just selfRef
                               & eTouch .~ Just blasterTouch
                               & eNextThink .~ levelTime + 2
                               & eThink .~ Just GameUtil.freeEdictA
                               & eDmg .~ damage
                               & eClassName .~ "bolt")
    when hyper $
        modifyRef boltRef (\v -> v & eSpawnFlags .~ 1)
    (gameImport^.giLinkEntity) boltRef
    when (isJust (self^.eClient)) $
        checkDodge selfRef start dir speed
    traceT <- (gameImport^.giTrace) (self^.eEntityState.esOrigin) Nothing Nothing start (Just boltRef) Constants.maskShot
    when ((traceT^.tFraction) < 1.0) $ do
        modifyRef boltRef (\v -> v & eEntityState.esOrigin .~ start + fmap (* (-10)) dir)
        dummyPlane <- use (gameBaseGlobals.gbDummyPlane)
        traceEntRef <- getTraceEntRef (traceT^.tEnt)
        entTouch blasterTouch boltRef traceEntRef dummyPlane Nothing
  where
    dir = normalize direction
    getTraceEntRef Nothing = do
        Com.fatalError "GameWeapon.fireBlaster traceT^.tEnt is Nothing"
        return (Ref (-1))
    getTraceEntRef (Just traceEntRef) = return traceEntRef

blasterTouch :: EntTouch
blasterTouch = EntTouch "blaster_touch" $ \selfRef otherRef plane surf -> do
    self <- readRef selfRef
    unless (Just otherRef == (self^.eOwner)) $ do
        let flags = maybe 0 (^.csFlags) surf
        if isJust surf && flags .&. Constants.surfSky /= 0
            then GameUtil.freeEdict selfRef
            else do
                ownerRef <- getOwnerRef (self^.eOwner)
                owner <- readRef ownerRef
                when (isJust (owner^.eClient)) $
                    PlayerWeapon.playerNoise ownerRef (self^.eEntityState.esOrigin) Constants.pNoiseImpact
                other <- readRef otherRef
                takeDamage selfRef self otherRef other ownerRef plane
                GameUtil.freeEdict selfRef
  where
    getOwnerRef Nothing = do
        Com.fatalError "GameWeapon.blasterTouch self^.eOwner is Nothing"
        return (Ref (-1))
    getOwnerRef (Just ownerRef) = return ownerRef
    takeDamage selfRef self otherRef other ownerRef plane
        | (other^.eTakeDamage) /= 0 = do
            let mod' = if self^.eSpawnFlags .&. 1 /= 0 then Constants.modHyperblaster else Constants.modBlaster
                normal = plane^.cpNormal -- TODO: jake2 has check for NULL here
            GameCombat.damage otherRef selfRef ownerRef (self^.eVelocity) (self^.eEntityState.esOrigin) normal (self^.eDmg) 1 Constants.damageEnergy mod'
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            (gameImport^.giWriteByte) Constants.svcTempEntity
            (gameImport^.giWriteByte) Constants.teBlaster
            (gameImport^.giWritePosition) (self^.eEntityState.esOrigin)
            (gameImport^.giWriteDir) (plane^.cpNormal) -- TODO: jake2 has check for NULL here
            (gameImport^.giMulticast) (self^.eEntityState.esOrigin) Constants.multicastPvs

checkDodge :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Quake ()
checkDodge selfRef start dir speed = do
    skill <- fmap (^.cvValue) skillCVar
    -- easy mode only ducks one quarter of the time
    r <- Lib.randomF
    unless (skill == 0 && r > 0.25) $ do
        trace <- use (gameBaseGlobals.gbGameImport.giTrace)
        traceT <- trace start Nothing Nothing (start + fmap (* 8192) dir) (Just selfRef) Constants.maskShot
        maybe (return ()) (doCheckDodge traceT) (traceT^.tEnt)
  where
    doCheckDodge traceT traceEntRef = do
        traceEnt <- readRef traceEntRef
        self <- readRef selfRef
        doDodge traceEntRef traceEnt self traceT (traceEnt^.eMonsterInfo.miDodge)
    doDodge _ _ _ _ Nothing = return ()
    doDodge traceEntRef traceEnt self traceT (Just dodgeF)
        | (traceEnt^.eSvFlags) .&. Constants.svfMonster /= 0 && (traceEnt^.eHealth) > 0 && GameUtil.inFront traceEnt self = do
            let v = (traceT^.tEndPos) - start
                eta = (norm v - (traceEnt^.eMaxs._x)) / fromIntegral speed
            entDodge dodgeF traceEntRef selfRef eta
        | otherwise = return ()

fireHit :: Ref EdictT -> V3 Float -> Int -> Int -> Quake Bool
fireHit = error "GameWeapon.fireHit" -- TODO
