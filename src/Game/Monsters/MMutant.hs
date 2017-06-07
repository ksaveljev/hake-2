module Game.Monsters.MMutant
    ( spMonsterMutant
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~), (+~))
import           Control.Monad         (replicateM_, when, void, unless)
import           Data.Bits             (complement, (.&.), (.|.))
import           Data.Int              (Int16)
import           Data.Maybe            (isJust)
import qualified Data.Vector           as V
import           Linear                (V3(..), _x, _y, _z, norm, normalize)

import qualified Client.M              as M
import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameCombat       as GameCombat
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameUtil         as GameUtil
import qualified Game.GameWeapon       as GameWeapon
import           Game.LevelLocalsT
import           Game.MonsterInfoT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

modelScale :: Float
modelScale = 1.0

frameAttack01 :: Int
frameAttack01 = 0

frameAttack02 :: Int
frameAttack02 = 1

frameAttack05 :: Int
frameAttack05 = 4

frameAttack08 :: Int
frameAttack08 = 7

frameAttack09 :: Int
frameAttack09 = 8

frameAttack15 :: Int
frameAttack15 = 14

frameDeath101 :: Int
frameDeath101 = 15

frameDeath109 :: Int
frameDeath109 = 23

frameDeath201 :: Int
frameDeath201 = 24

frameDeath210 :: Int
frameDeath210 = 33

framePain101 :: Int
framePain101 = 34

framePain105 :: Int
framePain105 = 38

framePain201 :: Int
framePain201 = 39

framePain206 :: Int
framePain206 = 44

framePain301 :: Int
framePain301 = 45

framePain311 :: Int
framePain311 = 55

frameRun03 :: Int
frameRun03 = 56

frameRun08 :: Int
frameRun08 = 61

frameStand101 :: Int
frameStand101 = 62

frameStand151 :: Int
frameStand151 = 112

frameStand152 :: Int
frameStand152 = 113

frameStand155 :: Int
frameStand155 = 116

frameStand164 :: Int
frameStand164 = 125

frameWalk01 :: Int
frameWalk01 = 126

frameWalk04 :: Int
frameWalk04 = 129

frameWalk05 :: Int
frameWalk05 = 130

frameWalk16 :: Int
frameWalk16 = 141

mutantStep :: EntThink
mutantStep = EntThink "mutant_step" $ \selfRef -> do
    r <- Lib.rand
    soundStep <- getSoundStep ((r + 1) `mod` 3)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice soundStep 1 Constants.attnNorm 0
    return True
  where
    getSoundStep :: Int16 -> Quake Int
    getSoundStep n
        | n == 0    = use (mMutantGlobals.mMutantSoundStep1)
        | n == 1    = use (mMutantGlobals.mMutantSoundStep2)
        | otherwise = use (mMutantGlobals.mMutantSoundStep3)

mutantSight :: EntInteract
mutantSight = EntInteract "mutant_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mMutantGlobals.mMutantSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

mutantSearch :: EntThink
mutantSearch = EntThink "mutant_search" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSearch <- use (mMutantGlobals.mMutantSoundSearch)
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

mutantSwing :: EntThink
mutantSwing = EntThink "mutant_swing" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSwing <- use (mMutantGlobals.mMutantSoundSwing)
    sound (Just selfRef) Constants.chanVoice soundSwing 1 Constants.attnNorm 0
    return True

mutantFramesStand :: V.Vector MFrameT
mutantFramesStand = V.replicate 51 (MFrameT (Just GameAI.aiStand) 0 Nothing)

mutantMoveStand :: MMoveT
mutantMoveStand = MMoveT "mutantMoveStand" frameStand101 frameStand151 mutantFramesStand Nothing

mutantStand :: EntThink
mutantStand = EntThink "mutant_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveStand)
    return True

mutantIdleLoop :: EntThink
mutantIdleLoop = EntThink "mutant_idle_loop" $ \selfRef -> do
    r <- Lib.randomF
    when (r < 0.75) $
        modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameStand155)
    return True

mutantFramesIdle :: V.Vector MFrameT
mutantFramesIdle =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing -- IMPROVE
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
                 -- scratch loop start
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just mutantIdleLoop)
                 -- scratch loop end
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

mutantMoveIdle :: MMoveT
mutantMoveIdle = MMoveT "mutantMoveIdle" frameStand152 frameStand164 mutantFramesIdle (Just mutantStand)

mutantIdle :: EntThink
mutantIdle = EntThink "mutant_idle" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveIdle)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle <- use (mMutantGlobals.mMutantSoundIdle)
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

mutantFramesWalk :: V.Vector MFrameT
mutantFramesWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dists)
  where
    dists = [3, 1, 5, 10, 13, 10, 0, 5, 6, 16, 15, 6]

mutantMoveWalk :: MMoveT
mutantMoveWalk = MMoveT "mutantMoveWalk" frameWalk05 frameWalk16 mutantFramesWalk Nothing

mutantWalkLoop :: EntThink
mutantWalkLoop = EntThink "mutant_walk_loop" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveWalk)
    return True

mutantFramesStartWalk :: V.Vector MFrameT
mutantFramesStartWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dists)
  where
    dists = [5, 5, -2, 1]

mutantMoveStartWalk :: MMoveT
mutantMoveStartWalk = MMoveT "mutantMoveStartWalk" frameWalk01 frameWalk04 mutantFramesStartWalk (Just mutantWalkLoop)

mutantWalk :: EntThink
mutantWalk = EntThink "mutant_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveStartWalk)
    return True

mutantFramesRun :: V.Vector MFrameT
mutantFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 40 Nothing
               , MFrameT (Just GameAI.aiRun) 40 (Just mutantStep)
               , MFrameT (Just GameAI.aiRun) 24 Nothing
               , MFrameT (Just GameAI.aiRun)  5 (Just mutantStep)
               , MFrameT (Just GameAI.aiRun) 17 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               ]

mutantMoveRun :: MMoveT
mutantMoveRun = MMoveT "mutantMoveRun" frameRun03 frameRun08 mutantFramesRun Nothing

mutantRun :: EntThink
mutantRun = EntThink "mutantRun" $ \selfRef -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove self))
    return True
  where
    currentMove self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 = mutantMoveStand
        | otherwise                                                       = mutantMoveRun

mutantHitLeft :: EntThink
mutantHitLeft = EntThink "mutant_hit_left" $ \selfRef -> do
    self <- readRef selfRef
    r <- Lib.rand
    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) 8
    hit <- GameWeapon.fireHit selfRef aim (10 + fromIntegral (r `mod` 5)) 100
    soundIdx <- getSound hit
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
    return True
  where
    getSound :: Bool -> Quake Int
    getSound hit
        | hit       = use (mMutantGlobals.mMutantSoundHit)
        | otherwise = use (mMutantGlobals.mMutantSoundSwing)

mutantHitRight :: EntThink
mutantHitRight = EntThink "mutant_hit_right" $ \selfRef -> do
    self <- readRef selfRef
    r <- Lib.rand
    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMaxs._x) 8
    hit <- GameWeapon.fireHit selfRef aim (10 + fromIntegral (r `mod` 5)) 100
    soundIdx <- getSound hit
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
    return True
  where
    getSound :: Bool -> Quake Int
    getSound hit
        | hit       = use (mMutantGlobals.mMutantSoundHit2)
        | otherwise = use (mMutantGlobals.mMutantSoundSwing)

mutantCheckReFire :: EntThink
mutantCheckReFire = EntThink "mutant_check_refire" $ \selfRef -> do
    self <- readRef selfRef
    done <- checkIfDone (self^.eEnemy)
    unless done $ do
        skill <- fmap (^.cvValue) skillCVar
        r <- Lib.randomF
        enemyRef <- getEnemyRef (self^.eEnemy)
        enemy <- readRef enemyRef
        when (skill == 3 && r < 0.5 || GameUtil.range self enemy == Constants.rangeMelee) $
            modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameAttack09)
    return True
  where
    checkIfDone Nothing = return True
    checkIfDone (Just enemyRef) = do
        enemy <- readRef enemyRef
        return (not (enemy^.eInUse) || (enemy^.eHealth) <= 0)
    getEnemyRef Nothing = do
        Com.fatalError "MMutant.mutantCheckReFire self^.eEnemy is Nothing"
        return (Ref (-1))
    getEnemyRef (Just enemyRef) = return enemyRef

mutantFramesAttack :: V.Vector MFrameT
mutantFramesAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just mutantHitLeft)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just mutantHitRight)
               , MFrameT (Just GameAI.aiCharge) 0 (Just mutantCheckReFire)
               ]

mutantMoveAttack :: MMoveT
mutantMoveAttack = MMoveT "mutantMoveAttack" frameAttack09 frameAttack15 mutantFramesAttack (Just mutantRun)

mutantMelee :: EntThink
mutantMelee = EntThink "mutant_melee" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveAttack)
    return True

mutantJumpTouch :: EntTouch
mutantJumpTouch = EntTouch "mutant_jump_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    doMutantJumpTouch selfRef self otherRef
  where
    doMutantJumpTouch selfRef self otherRef
        | (self^.eHealth) <= 0 =
            modifyRef selfRef (\v -> v & eTouch .~ Nothing)
        | otherwise = do
            other <- readRef otherRef
            when ((other^.eTakeDamage) /= 0 && norm (self^.eVelocity) > 400) $ do
              r <- Lib.randomF
              let normal = normalize (self^.eVelocity)
                  point = (self^.eEntityState.esOrigin) + fmap (* (self^.eMaxs._x)) normal
                  damage = truncate (40 + 10 * r)
              GameCombat.damage otherRef selfRef selfRef (self^.eVelocity) point normal damage damage 0 Constants.modUnknown
            bottom <- M.checkBottom selfRef
            if not bottom
                then do
                    updatedSelf <- readRef selfRef
                    when (isJust (updatedSelf^.eGroundEntity)) $ do
                        modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameAttack02
                                                   & eTouch .~ Nothing)
                else
                    modifyRef selfRef (\v -> v & eTouch .~ Nothing)

mutantJumpTakeOff :: EntThink
mutantJumpTakeOff = EntThink "mutant_jump_takeoff" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    soundSight <- use (mMutantGlobals.mMutantSoundSight)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    let (forward, _, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True False False
        V3 a b _ = fmap (* 600) forward
    modifyRef selfRef (\v -> v & eEntityState.esOrigin._z +~ 1
                               & eVelocity .~ V3 a b 250
                               & eGroundEntity .~ Nothing
                               & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked)
                               & eMonsterInfo.miAttackFinished .~ levelTime + 3
                               & eTouch .~ Just mutantJumpTouch)
    return True

mutantCheckLanding :: EntThink
mutantCheckLanding = EntThink "mutant_check_landing" $ \selfRef -> do
    self <- readRef selfRef
    maybe (noGroundEntity selfRef self) (hasGroundEntity selfRef) (self^.eGroundEntity)
    return True
  where
    noGroundEntity selfRef self = do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ nextFrame self levelTime)
    nextFrame self levelTime
        | levelTime > (self^.eMonsterInfo.miAttackFinished) = frameAttack02
        | otherwise                                         = frameAttack05
    hasGroundEntity selfRef _ = do
        soundThud <- use (mMutantGlobals.mMutantSoundThud)
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        sound (Just selfRef) Constants.chanWeapon soundThud 1 Constants.attnNorm 0
        modifyRef selfRef (\v -> v & eMonsterInfo.miAttackFinished .~ 0
                                   & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiDucked)))

mutantFramesJump :: V.Vector MFrameT
mutantFramesJump =
    V.fromList [ MFrameT (Just GameAI.aiCharge)  0 Nothing
               , MFrameT (Just GameAI.aiCharge) 17 Nothing
               , MFrameT (Just GameAI.aiCharge) 15 (Just mutantJumpTakeOff)
               , MFrameT (Just GameAI.aiCharge) 15 Nothing
               , MFrameT (Just GameAI.aiCharge) 15 (Just mutantCheckLanding)
               , MFrameT (Just GameAI.aiCharge)  0 Nothing
               , MFrameT (Just GameAI.aiCharge)  3 Nothing
               , MFrameT (Just GameAI.aiCharge)  0 Nothing
               ]

mutantMoveJump :: MMoveT
mutantMoveJump = MMoveT "mutantMoveJump" frameAttack01 frameAttack08 mutantFramesJump (Just mutantRun)

mutantJump :: EntThink
mutantJump = EntThink "mutant_jump" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveJump)
    return True

mutantCheckMelee :: EntThink
mutantCheckMelee = EntThink "mutant_check_melee" $ \selfRef -> do
    self <- readRef selfRef
    enemyRef <- getEnemyRef (self^.eEnemy)
    enemy <- readRef enemyRef
    return (GameUtil.range self enemy == Constants.rangeMelee)
  where
    getEnemyRef Nothing = do
        Com.fatalError "MMutant.mutantCheckMelee self^.eEnemy is Nothing"
        return (Ref (-1))
    getEnemyRef (Just enemyRef) = return enemyRef

mutantCheckJump :: EntThink
mutantCheckJump = EntThink "mutant_check_jump" $ \selfRef -> do
    self <- readRef selfRef
    enemyRef <- getEnemyRef (self^.eEnemy)
    enemy <- readRef enemyRef
    doMutantCheckJump self enemy
  where
    getEnemyRef Nothing = do
        Com.fatalError "MMutant.mutantCheckJump self^.eEnemy is Nothing"
        return (Ref (-1))
    getEnemyRef (Just enemyRef) = return enemyRef
    doMutantCheckJump self enemy
        | (self^.eAbsMin._z) > ((enemy^.eAbsMin._z) + 0.75 * (enemy^.eSize._z)) =
            return False
        | (self^.eAbsMax._z) < ((enemy^.eAbsMin._z) + 0.25 * (enemy^.eSize._z)) =
            return False
        | otherwise = do
            let v0 = (self^.eEntityState.esOrigin._x) - (enemy^.eEntityState.esOrigin._x)
                v1 = (self^.eEntityState.esOrigin._y) - (enemy^.eEntityState.esOrigin._y)
                v2 = 0
                distance = norm (V3 v0 v1 v2)
            r <- Lib.randomF
            return (checkDistance distance r)
    checkDistance distance r
        | distance < 100            = False
        | distance > 100 && r < 0.9 = False
        | otherwise                 = True

mutantCheckAttack :: EntThink
mutantCheckAttack = EntThink "mutant_checkattack" $ \selfRef -> do
    self <- readRef selfRef
    maybe (return False) (doMutantCheckAttack selfRef) (self^.eEnemy)
  where
    doMutantCheckAttack selfRef enemyRef = do
        enemy <- readRef enemyRef
        proceedMutantCheckAttack selfRef enemy
    proceedMutantCheckAttack selfRef enemy
        | (enemy^.eHealth) <= 0 = return False
        | otherwise = do
            melee <- entThink mutantCheckMelee selfRef
            checkMelee selfRef melee
    checkMelee selfRef melee
        | melee = do
            modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asMelee)
            return True
        | otherwise = do
            jump <- entThink mutantCheckJump selfRef
            checkJump selfRef jump
    checkJump selfRef jump
        | jump = do
            modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asMissile)
            -- FIXME: play a jump sound here
            return True
        | otherwise = return False

mutantFramesPain1 :: V.Vector MFrameT
mutantFramesPain1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [4, -3, -8, 2, 5]

mutantMovePain1 :: MMoveT
mutantMovePain1 = MMoveT "mutantMovePain1" framePain101 framePain105 mutantFramesPain1 (Just mutantRun)

mutantFramesPain2 :: V.Vector MFrameT
mutantFramesPain2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-24, 11, 5, -2, 6, 4]

mutantMovePain2 :: MMoveT
mutantMovePain2 = MMoveT "mutantMovePain2" framePain201 framePain206 mutantFramesPain2 (Just mutantRun)

mutantFramesPain3 :: V.Vector MFrameT
mutantFramesPain3 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-22, 3, 3, 2, 1, 1, 6, 3, 2, 0, 1]

mutantMovePain3 :: MMoveT
mutantMovePain3 = MMoveT "mutantMovePain3" framePain301 framePain311 mutantFramesPain3 (Just mutantRun)

mutantPain :: EntPain
mutantPain = EntPain "mutant_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        skill <- fmap (^.cvValue) skillCVar
        unless (skill == 3) $ do -- no pain anims in nightmare
            r <- Lib.randomF
            (soundPain, currentMove) <- getSoundAndMove r
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
  where
    getSoundAndMove :: Float -> Quake (Int, MMoveT)
    getSoundAndMove r
        | r < 0.33 = do
            soundPain <- use (mMutantGlobals.mMutantSoundPain1)
            return (soundPain, mutantMovePain1)
        | r < 0.66 = do
            soundPain <- use (mMutantGlobals.mMutantSoundPain2)
            return (soundPain, mutantMovePain2)
        | otherwise = do
            soundPain <- use (mMutantGlobals.mMutantSoundPain1)
            return (soundPain, mutantMovePain3)

mutantDead :: EntThink
mutantDead = EntThink "mutant_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster))
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    void (entThink M.flyCheck selfRef)
    return True

mutantFramesDeath1 :: V.Vector MFrameT
mutantFramesDeath1 = V.replicate 9 (MFrameT (Just GameAI.aiMove) 0 Nothing)

mutantMoveDeath1 :: MMoveT
mutantMoveDeath1 = MMoveT "mutantMoveDeath1" frameDeath101 frameDeath109 mutantFramesDeath1 (Just mutantDead)

mutantFramesDeath2 :: V.Vector MFrameT
mutantFramesDeath2 = V.replicate 10 (MFrameT (Just GameAI.aiMove) 0 Nothing)

mutantMoveDeath2 :: MMoveT
mutantMoveDeath2 = MMoveT "mutantMoveDeath2" frameDeath201 frameDeath210 mutantFramesDeath2 (Just mutantDead)

mutantDie :: EntDie
mutantDie = EntDie "mutant_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    doMutantDie selfRef self damage
  where
    doMutantDie selfRef self damage
        | (self^.eHealth) <= (self^.eGibHealth) = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "misc/udeath.wav")
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            replicateM_ 2 (GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic)
            replicateM_ 4 (GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic)
            GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        | (self^.eDeadFlag) == Constants.deadDead =
            return ()
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundDeath <- use (mMutantGlobals.mMutantSoundDeath)
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0
            r <- Lib.randomF
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                       & eTakeDamage .~ Constants.damageYes
                                       & eEntityState.esSkinNum .~ 1
                                       & eMonsterInfo.miCurrentMove .~ Just (if r < 0.5 then mutantMoveDeath1 else mutantMoveDeath2))

spMonsterMutant :: EntThink
spMonsterMutant = EntThink "SP_monster_mutant" $ \selfRef -> do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterMutant selfRef gameImport deathmatch

proceedSpawnMonsterMutant :: Ref EdictT -> GameImportT -> Float -> Quake Bool
proceedSpawnMonsterMutant selfRef gameImport deathmatch
    | deathmatch /= 0 = do
        GameUtil.freeEdict selfRef
        return False
    | otherwise = do
        soundIndex (Just "mutant/mutatck1.wav") >>= (mMutantGlobals.mMutantSoundSwing .=)
        soundIndex (Just "mutant/mutatck2.wav") >>= (mMutantGlobals.mMutantSoundHit .=)
        soundIndex (Just "mutant/mutatck3.wav") >>= (mMutantGlobals.mMutantSoundHit2 .=)
        soundIndex (Just "mutant/mutdeth1.wav") >>= (mMutantGlobals.mMutantSoundDeath .=)
        soundIndex (Just "mutant/mutidle1.wav") >>= (mMutantGlobals.mMutantSoundIdle .=)
        soundIndex (Just "mutant/mutpain1.wav") >>= (mMutantGlobals.mMutantSoundPain1 .=)
        soundIndex (Just "mutant/mutpain2.wav") >>= (mMutantGlobals.mMutantSoundPain2 .=)
        soundIndex (Just "mutant/mutsght1.wav") >>= (mMutantGlobals.mMutantSoundSight .=)
        soundIndex (Just "mutant/mutsrch1.wav") >>= (mMutantGlobals.mMutantSoundSearch .=)
        soundIndex (Just "mutant/step1.wav") >>= (mMutantGlobals.mMutantSoundStep1 .=)
        soundIndex (Just "mutant/step2.wav") >>= (mMutantGlobals.mMutantSoundStep2 .=)
        soundIndex (Just "mutant/step3.wav") >>= (mMutantGlobals.mMutantSoundStep3 .=)
        soundIndex (Just "mutant/thud1.wav") >>= (mMutantGlobals.mMutantSoundThud .=)
        modelIdx <- modelIndex (Just "models/monsters/mutant/tris.md2")
        modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                   & eSolid .~ Constants.solidBbox
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eMins .~ V3 (-32) (-32) (-24)
                                   & eMaxs .~ V3 32 32 48
                                   & eHealth .~ 300
                                   & eGibHealth .~ (-120)
                                   & eMass .~ 300
                                   & ePain .~ Just mutantPain
                                   & eDie .~ Just mutantDie
                                   & eMonsterInfo.miStand .~ Just mutantStand
                                   & eMonsterInfo.miWalk .~ Just mutantWalk
                                   & eMonsterInfo.miRun .~ Just mutantRun
                                   & eMonsterInfo.miDodge .~ Nothing
                                   & eMonsterInfo.miAttack .~ Just mutantJump
                                   & eMonsterInfo.miMelee .~ Just mutantMelee
                                   & eMonsterInfo.miSight .~ Just mutantSight
                                   & eMonsterInfo.miSearch .~ Just mutantSearch
                                   & eMonsterInfo.miIdle .~ Just mutantIdle
                                   & eMonsterInfo.miCheckAttack .~ Just mutantCheckAttack)
        linkEntity selfRef
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveStand
                                   & eMonsterInfo.miScale .~ modelScale)
        void (entThink GameAI.walkMonsterStart selfRef)
        return True
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity

