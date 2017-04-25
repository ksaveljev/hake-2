module Game.Monsters.MGunner
    ( spMonsterGunner
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~), (+~), (-~))
import           Control.Monad         (when, unless, void)
import           Data.Bits             ((.&.), (.|.), complement)
import           Data.Int              (Int16)
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V
import           Linear                (V3(..), _z, normalize)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import qualified Game.Monster          as Monster
import           Game.MonsterInfoT
import qualified Game.Monsters.MFlash  as MFlash
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

modelScale :: Float
modelScale = 1.15

frameStand01 :: Int
frameStand01 = 0

frameStand30 :: Int
frameStand30 = 29

frameStand31 :: Int
frameStand31 = 30

frameStand70 :: Int
frameStand70 = 69

frameWalk07 :: Int
frameWalk07 = 76

frameWalk19 :: Int
frameWalk19 = 88

frameRun01 :: Int
frameRun01 = 94

frameRun08 :: Int
frameRun08 = 101

frameRunShoot01 :: Int
frameRunShoot01 = 102

frameRunShoot06 :: Int
frameRunShoot06 = 107

frameAttack101 :: Int
frameAttack101 = 108

frameAttack105 :: Int
frameAttack105 = 112

frameAttack108 :: Int
frameAttack108 = 115

frameAttack111 :: Int
frameAttack111 = 118

frameAttack121 :: Int
frameAttack121 = 128

frameAttack209 :: Int
frameAttack209 = 137

frameAttack215 :: Int
frameAttack215 = 143

frameAttack216 :: Int
frameAttack216 = 144

frameAttack223 :: Int
frameAttack223 = 151

frameAttack224 :: Int
frameAttack224 = 152

frameAttack230 :: Int
frameAttack230 = 158

framePain101 :: Int
framePain101 = 159

framePain118 :: Int
framePain118 = 176

framePain201 :: Int
framePain201 = 177

framePain208 :: Int
framePain208 = 184

framePain301 :: Int
framePain301 = 185

framePain305 :: Int
framePain305 = 189

frameDeath01 :: Int
frameDeath01 = 190

frameDeath11 :: Int
frameDeath11 = 200

frameDuck01 :: Int
frameDuck01 = 201

frameDuck08 :: Int
frameDuck08 = 208

gunnerIdleSound :: EntThink
gunnerIdleSound = EntThink "gunner_idlesound" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle <- use (mGunnerGlobals.mGunnerSoundIdle)
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

gunnerSight :: EntInteract
gunnerSight = EntInteract "gunner_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mGunnerGlobals.mGunnerSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

gunnerSearch :: EntThink
gunnerSearch = EntThink "gunner_search" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSearch <- use (mGunnerGlobals.mGunnerSoundSearch)
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

gunnerFramesFidget :: V.Vector MFrameT
gunnerFramesFidget = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 (Just gunnerIdleSound)
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    ]

gunnerStand :: EntThink
gunnerStand = EntThink "gunner_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveStand)
    return True

gunnerMoveFidget :: MMoveT
gunnerMoveFidget = MMoveT "gunnerMoveFidget" frameStand31 frameStand70 gunnerFramesFidget (Just gunnerStand)

gunnerFidget :: EntThink
gunnerFidget = EntThink "gunner_fidget" $ \selfRef -> do
    self <- readRef selfRef
    unless ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0) $ do
        r <- Lib.randomF
        when (r <= 0.05) $
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveFidget)
    return True

gunnerFramesStand :: V.Vector MFrameT
gunnerFramesStand = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 (Just gunnerFidget)
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 (Just gunnerFidget)
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 (Just gunnerFidget)
    ]

gunnerMoveStand :: MMoveT
gunnerMoveStand = MMoveT "gunnerMoveStand" frameStand01 frameStand30 gunnerFramesStand Nothing

gunnerFramesWalk :: V.Vector MFrameT
gunnerFramesWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dists)
  where
    dists = [0, 3, 4, 5, 7, 2, 6, 4, 2, 7, 5, 7, 4]

gunnerMoveWalk :: MMoveT
gunnerMoveWalk = MMoveT "gunnerMoveWalk" frameWalk07 frameWalk19 gunnerFramesWalk Nothing

gunnerWalk :: EntThink
gunnerWalk = EntThink "gunner_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveWalk)
    return True

gunnerFramesRun :: V.Vector MFrameT
gunnerFramesRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dists)
  where
    dists = [26, 9, 9, 9, 15, 10, 13, 6]

gunnerMoveRun :: MMoveT
gunnerMoveRun = MMoveT "gunnerMoveRun" frameRun01 frameRun08 gunnerFramesRun Nothing

gunnerRun :: EntThink
gunnerRun = EntThink "gunner_run" $ \selfRef -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (nextMove self))
    return True
  where
    nextMove self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 = gunnerMoveStand
        | otherwise                                                       = gunnerMoveRun

gunnerFramesRunAndShoot :: V.Vector MFrameT
gunnerFramesRunAndShoot =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dists)
  where
    dists = [32, 15, 10, 18, 8, 20]

gunnerMoveRunAndShoot :: MMoveT
gunnerMoveRunAndShoot = MMoveT "gunnerMoveRunAndShoot" frameRunShoot01 frameRunShoot06 gunnerFramesRunAndShoot Nothing

gunnerRunAndShoot :: EntThink
gunnerRunAndShoot = EntThink "gunner_runandshoot" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveRunAndShoot)
    return True

gunnerFramesPain3 :: V.Vector MFrameT
gunnerFramesPain3 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-3, 1, 1, 0, 1]

gunnerMovePain3 :: MMoveT
gunnerMovePain3 = MMoveT "gunnerMovePain3" framePain301 framePain305 gunnerFramesPain3 (Just gunnerRun)

gunnerFramesPain2 :: V.Vector MFrameT
gunnerFramesPain2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-2, 11, 6, 2, -1, -7, -2, -7]

gunnerMovePain2 :: MMoveT
gunnerMovePain2 = MMoveT "gunnerMovePain2" framePain201 framePain208 gunnerFramesPain2 (Just gunnerRun)

gunnerFramesPain1 :: V.Vector MFrameT
gunnerFramesPain1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [2, 0, -5, 3, -1, 0, 0, 0, 0, 1, 1, 2, 1, 0, -2, -2, 0, 0]

gunnerMovePain1 :: MMoveT
gunnerMovePain1 = MMoveT "gunnerMovePain1" framePain101 framePain118 gunnerFramesPain1 (Just gunnerRun)

gunnerPain :: EntPain
gunnerPain = EntPain "gunner_pain" $ \selfRef _ _ damage -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        r <- Lib.rand
        soundPain <- getSoundPain r
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
        skill <- fmap (^.cvValue) skillCVar
        unless (skill == 3) $ do -- no pain anims in nighmare
          modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove damage))
  where
    getSoundPain :: Int16 -> Quake Int
    getSoundPain r
        | r .&. 1 /= 0 = use (mGunnerGlobals.mGunnerSoundPain)
        | otherwise    = use (mGunnerGlobals.mGunnerSoundPain2)
    currentMove damage
        | damage <= 10 = gunnerMovePain3
        | damage <= 25 = gunnerMovePain2
        | otherwise    = gunnerMovePain1

gunnerDead :: EntThink
gunnerDead = EntThink "gunner_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

gunnerFramesDeath :: V.Vector MFrameT
gunnerFramesDeath =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [0, 0, 0, -7, -3, -5, 8, 6, 0, 0, 0]

gunnerMoveDeath :: MMoveT
gunnerMoveDeath = MMoveT "gunnerMoveDeath" frameDeath01 frameDeath11 gunnerFramesDeath (Just gunnerDead)

gunnerDie :: EntDie
gunnerDie = EntDie "gunner_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    doGunnerDie selfRef self damage
  where
    doGunnerDie selfRef self damage
        | (self^.eHealth) <= (self^.eGibHealth) = do -- check for gib
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "misc/udeath.wav")
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            -- IMPROVE
            GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        | (self^.eDeadFlag) == Constants.deadDead = return ()
        | otherwise = do -- regular death
            soundDeath <- use (mGunnerGlobals.mGunnerSoundDeath)
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                       & eTakeDamage .~ Constants.damageYes
                                       & eMonsterInfo.miCurrentMove .~ Just gunnerMoveDeath)

gunnerDuckDown :: EntThink
gunnerDuckDown = EntThink "gunner_duck_down" $ \selfRef -> do
    self <- readRef selfRef
    unless ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked /= 0) $ do
        modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked))
        skill <- fmap (^.cvValue) skillCVar
        when (skill >= 2) $ do
            r <- Lib.randomF
            when (r > 0.5) $
                void (entThink gunnerGrenade selfRef)
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eMaxs._z -~ 32
                                   & eTakeDamage .~ Constants.damageYes
                                   & eMonsterInfo.miPauseTime .~ levelTime + 1)
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity selfRef
    return True

gunnerDuckHold :: EntThink
gunnerDuckHold = EntThink "gunner_duck_hold" $ \selfRef -> do
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

gunnerDuckUp :: EntThink
gunnerDuckUp = EntThink "gunner_duck_up" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiDucked))
                               & eMaxs._z +~ 32
                               & eTakeDamage .~ Constants.damageAim)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

gunnerFramesDuck :: V.Vector MFrameT
gunnerFramesDuck = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove)   1  (Just gunnerDuckDown)
    , MFrameT (Just GameAI.aiMove)   1  Nothing
    , MFrameT (Just GameAI.aiMove)   1  (Just gunnerDuckHold)
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove) (-1) Nothing
    , MFrameT (Just GameAI.aiMove) (-1) Nothing
    , MFrameT (Just GameAI.aiMove)   0  (Just gunnerDuckUp)
    , MFrameT (Just GameAI.aiMove) (-1) Nothing
    ]

gunnerMoveDuck :: MMoveT
gunnerMoveDuck = MMoveT "gunnerMoveDuck" frameDuck01 frameDuck08 gunnerFramesDuck (Just gunnerRun)

gunnerDodge :: EntDodge
gunnerDodge = EntDodge "gunner_dodge" $ \selfRef attackerRef _ -> do
    r <- Lib.randomF
    unless (r > 0.25) $ do
        self <- readRef selfRef
        when (isNothing (self^.eEnemy)) $
            modifyRef selfRef (\v -> v & eEnemy .~ Just attackerRef)
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveDuck)

gunnerOpenGun :: EntThink
gunnerOpenGun = EntThink "gunner_opengun" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundOpen <- use (mGunnerGlobals.mGunnerSoundOpen)
    sound (Just selfRef) Constants.chanVoice soundOpen 1 Constants.attnIdle 0
    return True

gunnerFire :: EntThink
gunnerFire = EntThink "GunnerFire" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doGunnerFire selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MGunner.gunnerFire self^.eEnemy is Nothing"
    doGunnerFire selfRef self enemyRef = do
        enemy <- readRef enemyRef
        let flashNumber = Constants.mz2GunnerMachinegun1 + (self^.eEntityState.esFrame) - frameAttack216
            (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
            start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
            V3 a b c = (enemy^.eEntityState.esOrigin) + fmap (* (-0.2)) (enemy^.eVelocity)
            target = V3 a b (c + fromIntegral (enemy^.eViewHeight))
            aim = normalize (target - start)
        Monster.monsterFireBullet selfRef start aim 3 4 Constants.defaultBulletHspread Constants.defaultBulletVspread flashNumber

gunnerGrenade :: EntThink
gunnerGrenade = EntThink "GunnerGrenade" $ \selfRef -> do
    self <- readRef selfRef
    let flashNumber | (self^.eEntityState.esFrame) == frameAttack105 = Constants.mz2GunnerGrenade1
                    | (self^.eEntityState.esFrame) == frameAttack108 = Constants.mz2GunnerGrenade2
                    | (self^.eEntityState.esFrame) == frameAttack111 = Constants.mz2GunnerGrenade3
                    | otherwise                                      = Constants.mz2GunnerGrenade4
        (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
        -- FIXME: do a spread -225 -75 75 255 degrees around forward
        aim = forward
    Monster.monsterFireGrenade selfRef start aim 50 600 flashNumber
    return True

gunnerAttack :: EntThink
gunnerAttack = EntThink "gunner_attack" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doGunnerAttack selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MGunner.gunnerAttack self^.eEnemy is Nothing"
    doGunnerAttack selfRef self enemyRef = do
        enemy <- readRef enemyRef
        r <- Lib.randomF
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove self enemy r))
    currentMove self enemy r
        | GameUtil.range self enemy == Constants.rangeMelee = gunnerMoveAttackChain
        | r <= 0.5                                          = gunnerMoveAttackGrenade
        | otherwise                                         = gunnerMoveAttackChain

gunnerFireChain :: EntThink
gunnerFireChain = EntThink "gunner_fire_chain" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveFireChain)
    return True

gunnerFramesAttackChain :: V.Vector MFrameT
gunnerFramesAttackChain = V.fromList --IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 (Just gunnerOpenGun)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    ]

gunnerMoveAttackChain :: MMoveT
gunnerMoveAttackChain = MMoveT "gunnerMoveAttackChain" frameAttack209 frameAttack215 gunnerFramesAttackChain (Just gunnerFireChain)

gunnerFramesFireChain :: V.Vector MFrameT
gunnerFramesFireChain = V.replicate 8 (MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire))

gunnerReFireChain :: EntThink
gunnerReFireChain = EntThink "gunner_refire_chain" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doGunnerReFireChain selfRef) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MGunner.gunnerReFireChain self^.eEnemy is Nothing"
    doGunnerReFireChain selfRef enemyRef = do
        enemy <- readRef enemyRef
        currentMove <- getCurrentMove selfRef enemyRef enemy
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
    getCurrentMove selfRef enemyRef enemy
        | (enemy^.eHealth) > 0 = do
            visible <- GameUtil.visible selfRef enemyRef
            r <- Lib.randomF
            return (nextMove visible r)
        | otherwise = return gunnerMoveEndFireChain
    nextMove visible r
        | visible && r <= 0.5 = gunnerMoveFireChain
        | otherwise           = gunnerMoveEndFireChain

gunnerMoveFireChain :: MMoveT
gunnerMoveFireChain = MMoveT "gunnerMoveFireChain" frameAttack216 frameAttack223 gunnerFramesFireChain (Just gunnerReFireChain)

gunnerFramesEndFireChain :: V.Vector MFrameT
gunnerFramesEndFireChain = V.replicate 7 (MFrameT (Just GameAI.aiCharge) 0 Nothing)

gunnerMoveEndFireChain :: MMoveT
gunnerMoveEndFireChain = MMoveT "gunnerMoveEndFireChain" frameAttack224 frameAttack230 gunnerFramesEndFireChain (Just gunnerRun)

gunnerFramesAttackGrenade :: V.Vector MFrameT
gunnerFramesAttackGrenade = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    ]

gunnerMoveAttackGrenade :: MMoveT
gunnerMoveAttackGrenade = MMoveT "gunnerMoveAttackGrenade" frameAttack101 frameAttack121 gunnerFramesAttackGrenade (Just gunnerRun)

{-
- QUAKED monster_gunner (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterGunner :: Ref EdictT -> Quake ()
spMonsterGunner selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnMonsterGunner deathmatch
  where
    doSpawnMonsterGunner deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let soundIndex = gameImport^.giSoundIndex
                modelIndex = gameImport^.giModelIndex
                linkEntity = gameImport^.giLinkEntity
            soundIndex (Just "gunner/death1.wav") >>= (mGunnerGlobals.mGunnerSoundDeath .=)
            soundIndex (Just "gunner/gunpain2.wav") >>= (mGunnerGlobals.mGunnerSoundPain .=)
            soundIndex (Just "gunner/gunpain1.wav") >>= (mGunnerGlobals.mGunnerSoundPain2 .=)
            soundIndex (Just "gunner/gunidle1.wav") >>= (mGunnerGlobals.mGunnerSoundIdle .=)
            soundIndex (Just "gunner/gunatck1.wav") >>= (mGunnerGlobals.mGunnerSoundOpen .=)
            soundIndex (Just "gunner/gunsrch1.wav") >>= (mGunnerGlobals.mGunnerSoundSearch .=)
            soundIndex (Just "gunner/sight1.wav") >>= (mGunnerGlobals.mGunnerSoundSight .=)
            void (soundIndex (Just "gunner/gunatck2.wav"))
            void (soundIndex (Just "gunner/gunatck3.wav"))
            modelIdx <- modelIndex (Just "models/monsters/gunner/tris.md2")
            modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                       & eSolid .~ Constants.solidBbox
                                       & eEntityState.esModelIndex .~ modelIdx
                                       & eMins .~ V3 (-16) (-16) (-24)
                                       & eMaxs .~ V3 16 16 32
                                       & eHealth .~ 175
                                       & eGibHealth .~ (-70)
                                       & eMass .~ 200
                                       & ePain .~ Just gunnerPain
                                       & eDie .~ Just gunnerDie
                                       & eMonsterInfo.miStand .~ Just gunnerStand
                                       & eMonsterInfo.miWalk .~ Just gunnerWalk
                                       & eMonsterInfo.miRun .~ Just gunnerRun
                                       & eMonsterInfo.miDodge .~ Just gunnerDodge
                                       & eMonsterInfo.miAttack .~ Just gunnerAttack
                                       & eMonsterInfo.miMelee .~ Nothing
                                       & eMonsterInfo.miSight .~ Just gunnerSight
                                       & eMonsterInfo.miSearch .~ Just gunnerSearch)
            linkEntity selfRef
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveStand
                                       & eMonsterInfo.miScale .~ modelScale)
            void (entThink GameAI.walkMonsterStart selfRef)
