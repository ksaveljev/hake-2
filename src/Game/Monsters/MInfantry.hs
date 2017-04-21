module Game.Monsters.MInfantry where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~), (+~), (-~))
import           Control.Monad         (void, when, unless)
import           Data.Bits             ((.&.), (.|.), complement)
import           Data.Int              (Int16)
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V
import           Linear                (V3(..), normalize, _z)

import qualified Client.M              as M
import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameUtil         as GameUtil
import qualified Game.GameWeapon       as GameWeapon
import           Game.LevelLocalsT
import qualified Game.Monster          as Monster
import           Game.MonsterInfoT
import qualified Game.Monsters.MFlash  as MFlash
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

modelScale :: Float
modelScale = 1

frameStand01 :: Int
frameStand01 = 1

frameStand49 :: Int
frameStand49 = 49

frameStand50 :: Int
frameStand50 = 50

frameStand71 :: Int
frameStand71 = 71

frameWalk03 :: Int
frameWalk03 = 74

frameWalk14 :: Int
frameWalk14 = 85

frameRun01 :: Int
frameRun01 = 92

frameRun08 :: Int
frameRun08 = 99

framePain101 :: Int
framePain101 = 100

framePain110 :: Int
framePain110 = 109

framePain201 :: Int
framePain201 = 110

framePain210 :: Int
framePain210 = 119

frameDuck01 :: Int
frameDuck01 = 120

frameDuck05 :: Int
frameDuck05 = 124

frameDeath101 :: Int
frameDeath101 = 125

frameDeath120 :: Int
frameDeath120 = 144

frameDeath201 :: Int
frameDeath201 = 145

frameDeath211 :: Int
frameDeath211 = 155

frameDeath225 :: Int
frameDeath225 = 169

frameDeath301 :: Int
frameDeath301 = 170

frameDeath309 :: Int
frameDeath309 = 178

frameAttack101 :: Int
frameAttack101 = 184

frameAttack111 :: Int
frameAttack111 = 194

frameAttack115 :: Int
frameAttack115 = 198

frameAttack201 :: Int
frameAttack201 = 199

frameAttack208 :: Int
frameAttack208 = 206

infantryFramesStand :: V.Vector MFrameT
infantryFramesStand = V.replicate 22 (MFrameT (Just GameAI.aiStand) 0 Nothing)

infantryMoveStand :: MMoveT
infantryMoveStand = MMoveT "infantryMoveStand" frameStand50 frameStand71 infantryFramesStand Nothing

infantryStand :: EntThink
infantryStand = EntThink "infantry_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just infantryMoveStand)
    return True

infantryFramesFidget :: V.Vector MFrameT
infantryFramesFidget =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiStand) v Nothing) dists)
  where
    dists = [ 1, 0, 1, 3, 6, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0
            , 1, 0, -1, 0, 0, 1, 0, -2, 1, 1, 1, -1, 0, 0
            , -1, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, -1, -1
            , 0, -3, -2, -3, -3, -2]

infantryMoveFidget :: MMoveT
infantryMoveFidget = MMoveT "infantryMoveFidget" frameStand01 frameStand49 infantryFramesFidget (Just infantryStand)

infantryFidget :: EntThink
infantryFidget = EntThink "infantry_fidget" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just infantryMoveFidget)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle <- use (mInfantryGlobals.miSoundIdle)
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

infantryFramesWalk :: V.Vector MFrameT
infantryFramesWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dists)
  where
    dists = [5, 4, 4, 5, 4, 5, 6, 4, 4, 4, 4, 5]

infantryMoveWalk :: MMoveT
infantryMoveWalk = MMoveT "infantryMoveWalk" frameWalk03 frameWalk14 infantryFramesWalk Nothing

infantryWalk :: EntThink
infantryWalk = EntThink "infantry_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just infantryMoveWalk)
    return True

infantryFramesRun :: V.Vector MFrameT
infantryFramesRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dists)
  where
    dists = [10, 20, 5, 7, 30, 35, 2, 6]

infantryMoveRun :: MMoveT
infantryMoveRun = MMoveT "infantryMoveRun" frameRun01 frameRun08 infantryFramesRun Nothing

infantryRun :: EntThink
infantryRun = EntThink "infantry_run" $ \selfRef -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (nextMove (self^.eMonsterInfo.miAIFlags)))
    return True
  where
    nextMove aiFlags
        | aiFlags .&. Constants.aiStandGround /= 0 = infantryMoveStand
        | otherwise                                = infantryMoveRun

infantryFramesPain1 :: V.Vector MFrameT
infantryFramesPain1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-3, -2 ,-1, -2, -1, 1, -1, 1, 6, 2]

infantryMovePain1 :: MMoveT
infantryMovePain1 = MMoveT "infantryMovePain1" framePain101 framePain110 infantryFramesPain1 (Just infantryRun)

infantryFramesPain2 :: V.Vector MFrameT
infantryFramesPain2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-3, -3, 0, -1, -2, 0, 0, 2, 5, 2]

infantryMovePain2 :: MMoveT
infantryMovePain2 = MMoveT "infantryMovePain2" framePain201 framePain210 infantryFramesPain2 (Just infantryRun)

infantryPain :: EntPain
infantryPain = EntPain "infantry_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        skill <- fmap (^.cvValue) skillCVar
        unless (skill == 3) $ do -- no pain anims in nightmare
            n <- fmap (`mod` 2) Lib.rand
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            (soundPain, currentMove) <- getSoundAndMove n
            sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
  where
    getSoundAndMove :: Int16 -> Quake (Int, MMoveT)
    getSoundAndMove n
        | n == 0 = do
            soundPain <- use (mInfantryGlobals.miSoundPain1)
            return (soundPain, infantryMovePain1)
        | otherwise = do
            soundPain <- use (mInfantryGlobals.miSoundPain2)
            return (soundPain, infantryMovePain2)

infantrySight :: EntInteract
infantrySight = EntInteract "infantry_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mInfantryGlobals.miSoundSight)
    sound (Just selfRef) Constants.chanBody soundSight 1 Constants.attnNorm 0
    return True

infantryDead :: EntThink
infantryDead = EntThink "infantry_dead" $ \selfRef -> do
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster))
    linkEntity selfRef
    void (entThink M.flyCheck selfRef)
    return True

aimAngles :: V.Vector (V3 Float)
aimAngles = V.fromList
    [ V3  0  5 0
    , V3 10 15 0
    , V3 20 25 0
    , V3 25 35 0
    , V3 30 40 0
    , V3 30 45 0
    , V3 25 50 0
    , V3 20 40 0
    , V3 15 35 0
    , V3 40 35 0
    , V3 70 35 0
    , V3 90 35 0 ]

infantryMachineGun :: EntThink
infantryMachineGun = EntThink "InfantryMachineGun" $ \selfRef -> do
    self <- readRef selfRef
    (start, forward, flashNumber) <- getFireInfo self
    Monster.monsterFireBullet selfRef start forward 3 4 Constants.defaultBulletHspread Constants.defaultBulletVspread flashNumber
    return True
  where
    getFireInfo self
        | (self^.eEntityState.esFrame) == frameAttack111 = do
            let flashNumber = Constants.mz2InfantryMachinegun1
                (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
                start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
            case self^.eEnemy of
              Just enemyRef -> do
                enemy <- readRef enemyRef
                let V3 a b c = (enemy^.eEntityState.esOrigin) + fmap (* (-0.2)) (enemy^.eVelocity)
                    target = V3 a b (c + fromIntegral (enemy^.eViewHeight))
                    forward' = normalize (target - start)
                return (start, forward', flashNumber)
              Nothing -> do
                let (forward', _, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
                return (start, forward', flashNumber)
        | otherwise = do
            let flashNumber = Constants.mz2InfantryMachinegun2 + (self^.eEntityState.esFrame) - frameDeath211
                (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
                start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
                vec = (self^.eEntityState.esAngles) - (aimAngles V.! (flashNumber - Constants.mz2InfantryMachinegun2))
                (forward', _, _) = Math3D.angleVectors vec True False False
            return (start, forward', flashNumber)

infantryFramesDeath1 :: V.Vector MFrameT
infantryFramesDeath1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-4, 0, 0, -1, -4, 0, 0, 0, -1, 3, 1, 1, -2, 2, 2, 9, 9, 5, -3, -3]

infantryMoveDeath1 :: MMoveT
infantryMoveDeath1 = MMoveT "infantryMoveDeath1" frameDeath101 frameDeath120 infantryFramesDeath1 (Just infantryDead)

-- Off with his head
infantryFramesDeath2 :: V.Vector MFrameT
infantryFramesDeath2 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    5  Nothing
    , MFrameT (Just GameAI.aiMove)  (-1) Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    4  Nothing
    , MFrameT (Just GameAI.aiMove)    3  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)  (-2) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)  (-3) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)  (-1) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)  (-2) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)    0  (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)    2  (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)    2  (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)    3  (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove) (-10) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)  (-7) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)  (-8) (Just infantryMachineGun)
    , MFrameT (Just GameAI.aiMove)  (-6) Nothing
    , MFrameT (Just GameAI.aiMove)    4  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    ]

infantryMoveDeath2 :: MMoveT
infantryMoveDeath2 = MMoveT "infantryMoveDeath2" frameDeath201 frameDeath225 infantryFramesDeath2 (Just infantryDead)

infantryFramesDeath3 :: V.Vector MFrameT
infantryFramesDeath3 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [0, 0, 0, -6, -11, -3, -11, 0, 0]

infantryMoveDeath3 :: MMoveT
infantryMoveDeath3 = MMoveT "infantryMoveDeath3" frameDeath301 frameDeath309 infantryFramesDeath3 (Just infantryDead)

infantryDie :: EntDie
infantryDie = EntDie "infantry_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    doInfantryDie selfRef self damage
  where
    doInfantryDie selfRef self damage
        | (self^.eHealth) <= (self^.eGibHealth) = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            udeath <- (gameImport^.giSoundIndex) (Just "misc/udeath.wav")
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice udeath 1 Constants.attnNorm 0
            -- IMPROVE
            GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        | otherwise =
            unless ((self^.eDeadFlag) == Constants.deadDead) $ do
                modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                           & eTakeDamage .~ Constants.damageYes)
                n <- fmap (`mod` 3) Lib.rand
                soundDie1 <- use (mInfantryGlobals.miSoundDie1)
                soundDie2 <- use (mInfantryGlobals.miSoundDie2)
                let (nextMove, nextSound) | n == 0    = (infantryMoveDeath1, soundDie2)
                                          | n == 1    = (infantryMoveDeath2, soundDie1)
                                          | otherwise = (infantryMoveDeath3, soundDie2)
                modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just nextMove)
                sound <- use (gameBaseGlobals.gbGameImport.giSound)
                sound (Just selfRef) Constants.chanVoice nextSound 1 Constants.attnNorm 0

infantryDodge :: EntDodge
infantryDodge = EntDodge "infantry_dodge" $ \selfRef attackerRef _ -> do
    r <- Lib.randomF
    unless (r > 0.25) $ do
        self <- readRef selfRef
        when (isNothing (self^.eEnemy)) $
            modifyRef selfRef (\v -> v & eEnemy .~ Just attackerRef)
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just infantryMoveDuck)

infantryFramesDuck :: V.Vector MFrameT
infantryFramesDuck = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove) (-2) (Just infantryDuckDown)
    , MFrameT (Just GameAI.aiMove) (-5) (Just infantryDuckHold)
    , MFrameT (Just GameAI.aiMove)   3  Nothing
    , MFrameT (Just GameAI.aiMove)   4  (Just infantryDuckUp)
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    ]

infantryMoveDuck :: MMoveT
infantryMoveDuck = MMoveT "infantryMoveDuck" frameDuck01 frameDuck05 infantryFramesDuck (Just infantryRun)

infantryDuckDown :: EntThink
infantryDuckDown = EntThink "infantry_duck_down" $ \selfRef -> do
    self <- readRef selfRef
    doInfantryDuckDown selfRef self
  where
    doInfantryDuckDown selfRef self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked /= 0 = return True
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked)
                                       & eMaxs._z -~ 32
                                       & eTakeDamage .~ Constants.damageYes
                                       & eMonsterInfo.miPauseTime .~ levelTime + 1)
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity selfRef
            return True

infantryDuckHold :: EntThink
infantryDuckHold = EntThink "infantry_duck_hold" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    updateEdict selfRef self levelTime
    return True
  where
    updateEdict selfRef self levelTime
        | levelTime >= (self^.eMonsterInfo.miPauseTime) =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiHoldFrame)))
        | otherwise =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

infantryDuckUp :: EntThink
infantryDuckUp = EntThink "infantry_duck_up" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiDucked))
                               & eMaxs._z +~ 32
                               & eTakeDamage .~ Constants.damageAim)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

infantryAttack :: EntThink
infantryAttack = EntThink "infantry_attack" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doInfantryAttack selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MInfantry.infantryAttack self^.eEnemy is Nothing"
    doInfantryAttack selfRef self enemyRef = do
        enemy <- readRef enemyRef
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove self enemy))
    currentMove self enemy
        | GameUtil.range self enemy == Constants.rangeMelee = infantryMoveAttack2
        | otherwise                                         = infantryMoveAttack1

infantryCockGun :: EntThink
infantryCockGun = EntThink "infantry_cock_gun" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    soundWeaponCock <- use (mInfantryGlobals.miSoundWeaponCock)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanWeapon soundWeaponCock 1 Constants.attnNorm 0
    n <- Lib.rand
    modifyRef selfRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + (fromIntegral $ (n .&. 15) + 3 + 7) * Constants.frameTime)
    return True

infantryFire :: EntThink
infantryFire = EntThink "infantry_fire" $ \selfRef -> do
    void (entThink infantryMachineGun selfRef)
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    updateEdict selfRef self levelTime
    return True
  where
    updateEdict selfRef self levelTime
        | levelTime >= (self^.eMonsterInfo.miPauseTime) =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiHoldFrame)))
        | otherwise =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

infantrySwing :: EntThink
infantrySwing = EntThink "infantry_swing" $ \selfRef -> do
    soundPunchSwing <- use (mInfantryGlobals.miSoundPunchSwing)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanWeapon soundPunchSwing 1 Constants.attnNorm 0
    return True

infantrySmack :: EntThink
infantrySmack = EntThink "infantry_smack" $ \selfRef -> do
    n <- Lib.rand
    hit <- GameWeapon.fireHit selfRef (V3 (fromIntegral Constants.meleeDistance) 0 0) (5 + fromIntegral (n `mod` 5)) 50
    when hit $ do
        soundPunchHit <- use (mInfantryGlobals.miSoundPunchHit)
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        sound (Just selfRef) Constants.chanWeapon soundPunchHit 1 Constants.attnNorm 0
    return True

infantryFramesAttack1 :: V.Vector MFrameT
infantryFramesAttack1 = V.fromList --IMPROVE
    [ MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just infantryCockGun)
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)   2  Nothing
    , MFrameT (Just GameAI.aiCharge) (-2) Nothing
    , MFrameT (Just GameAI.aiCharge) (-3) Nothing
    , MFrameT (Just GameAI.aiCharge)   1  (Just infantryFire)
    , MFrameT (Just GameAI.aiCharge)   5  Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge) (-2) Nothing
    , MFrameT (Just GameAI.aiCharge) (-3) Nothing
    ]

infantryMoveAttack1 :: MMoveT
infantryMoveAttack1 = MMoveT "infantryMoveAttack1" frameAttack101 frameAttack115 infantryFramesAttack1 (Just infantryRun)

infantryFramesAttack2 :: V.Vector MFrameT
infantryFramesAttack2 = V.fromList --IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 3 Nothing
    , MFrameT (Just GameAI.aiCharge) 6 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just infantrySwing)
    , MFrameT (Just GameAI.aiCharge) 8 Nothing
    , MFrameT (Just GameAI.aiCharge) 5 Nothing
    , MFrameT (Just GameAI.aiCharge) 8 (Just infantrySmack)
    , MFrameT (Just GameAI.aiCharge) 6 Nothing
    , MFrameT (Just GameAI.aiCharge) 3 Nothing
    ]

infantryMoveAttack2 :: MMoveT
infantryMoveAttack2 = MMoveT "infantryMoveAttack2" frameAttack201 frameAttack208 infantryFramesAttack2 (Just infantryRun)

spMonsterInfantry :: Ref EdictT -> Quake ()
spMonsterInfantry selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnMonsterInfantry deathmatch
  where
    doSpawnMonsterInfantry deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let soundIndex = gameImport^.giSoundIndex
                modelIndex = gameImport^.giModelIndex
                linkEntity = gameImport^.giLinkEntity
            soundIndex (Just "infantry/infpain1.wav") >>= (mInfantryGlobals.miSoundPain1 .=)
            soundIndex (Just "infantry/infpain2.wav") >>= (mInfantryGlobals.miSoundPain2 .=)
            soundIndex (Just "infantry/infdeth1.wav") >>= (mInfantryGlobals.miSoundDie1 .=)
            soundIndex (Just "infantry/infdeth2.wav") >>= (mInfantryGlobals.miSoundDie2 .=)
            soundIndex (Just "infantry/infatck1.wav") >>= (mInfantryGlobals.miSoundGunShot .=)
            soundIndex (Just "infantry/infatck3.wav") >>= (mInfantryGlobals.miSoundWeaponCock .=)
            soundIndex (Just "infantry/infatck2.wav") >>= (mInfantryGlobals.miSoundPunchSwing .=)
            soundIndex (Just "infantry/melee2.wav") >>= (mInfantryGlobals.miSoundPunchHit .=)
            soundIndex (Just "infantry/infsght1.wav") >>= (mInfantryGlobals.miSoundSight .=)
            soundIndex (Just "infantry/infsrch1.wav") >>= (mInfantryGlobals.miSoundSearch .=)
            soundIndex (Just "infantry/infidle1.wav") >>= (mInfantryGlobals.miSoundIdle .=)
            modelIdx <- modelIndex (Just "models/monsters/infantry/tris.md2")
            modifyRef selfRef (\v -> v & eMoveType                 .~ Constants.moveTypeStep
                                       & eSolid                    .~ Constants.solidBbox
                                       & eEntityState.esModelIndex .~ modelIdx
                                       & eMins                     .~ V3 (-16) (-16) (-24)
                                       & eMaxs                     .~ V3 16 16 32
                                       & eHealth                   .~ 100
                                       & eGibHealth                .~ -40
                                       & eMass                     .~ 200
                                       & ePain                     .~ Just infantryPain
                                       & eDie                      .~ Just infantryDie
                                       & eMonsterInfo.miStand      .~ Just infantryStand
                                       & eMonsterInfo.miWalk       .~ Just infantryWalk
                                       & eMonsterInfo.miRun        .~ Just infantryRun
                                       & eMonsterInfo.miDodge      .~ Just infantryDodge
                                       & eMonsterInfo.miAttack     .~ Just infantryAttack
                                       & eMonsterInfo.miMelee      .~ Nothing
                                       & eMonsterInfo.miSight      .~ Just infantrySight
                                       & eMonsterInfo.miIdle       .~ Just infantryFidget)
            linkEntity selfRef
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just infantryMoveStand
                                       & eMonsterInfo.miScale       .~ modelScale)
            void (entThink GameAI.walkMonsterStart selfRef)
