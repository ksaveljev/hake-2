{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MMutant where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=), (+=), (&), (.~), (%~), (+~))
import Control.Monad (when, void, unless, liftM)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isJust)
import Linear (V3(..), _x, _y, _z, norm, normalize)
import qualified Data.Vector as V

import Game.EntityStateT
import Game.EdictT
import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import qualified Game.GameAI as GameAI
import qualified Game.GameCombat as GameCombat
import qualified Game.GameMisc as GameMisc
import qualified Game.GameUtil as GameUtil
import qualified Game.GameWeapon as GameWeapon
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

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
mutantStep =
  GenericEntThink "mutant_step" $ \selfRef -> do
    r <- Lib.rand
    let n = (r + 1) `mod` 3

    soundStep <- if | n == 0 -> use $ mMutantGlobals.mMutantSoundStep1
                    | n == 1 -> use $ mMutantGlobals.mMutantSoundStep2
                    | otherwise -> use $ mMutantGlobals.mMutantSoundStep3

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundStep 1 Constants.attnNorm 0
    return True

mutantSight :: EntInteract
mutantSight =
  GenericEntInteract "mutant_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mMutantGlobals.mMutantSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

mutantSearch :: EntThink
mutantSearch =
  GenericEntThink "mutant_search" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSearch <- use $ mMutantGlobals.mMutantSoundSearch
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

mutantSwing :: EntThink
mutantSwing =
  GenericEntThink "mutant_swing" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSwing <- use $ mMutantGlobals.mMutantSoundSwing
    sound (Just selfRef) Constants.chanVoice soundSwing 1 Constants.attnNorm 0
    return True

mutantFramesStand :: V.Vector MFrameT
mutantFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing -- 10
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing -- 20
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing -- 30
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing -- 40
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing -- 50
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

mutantMoveStand :: MMoveT
mutantMoveStand = MMoveT "mutantMoveStand" frameStand101 frameStand151 mutantFramesStand Nothing

mutantStand :: EntThink
mutantStand =
  GenericEntThink "mutant_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveStand)
    return True

mutantIdleLoop :: EntThink
mutantIdleLoop =
  GenericEntThink "mutant_idle_loop" $ \selfRef -> do
    r <- Lib.randomF

    when (r < 0.75) $
      modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameStand155)

    return True

mutantFramesIdle :: V.Vector MFrameT
mutantFramesIdle =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
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
mutantIdle =
  GenericEntThink "mutant_idle" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveIdle)

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mMutantGlobals.mMutantSoundIdle
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

mutantFramesWalk :: V.Vector MFrameT
mutantFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  3 Nothing
               , MFrameT (Just GameAI.aiWalk)  1 Nothing
               , MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk) 13 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk) 16 Nothing
               , MFrameT (Just GameAI.aiWalk) 15 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               ]

mutantMoveWalk :: MMoveT
mutantMoveWalk = MMoveT "mutantMoveWalk" frameWalk05 frameWalk16 mutantFramesWalk Nothing

mutantWalkLoop :: EntThink
mutantWalkLoop =
  GenericEntThink "mutant_walk_loop" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveWalk)
    return True

mutantFramesStartWalk :: V.Vector MFrameT
mutantFramesStartWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)   5  Nothing
               , MFrameT (Just GameAI.aiWalk)   5  Nothing
               , MFrameT (Just GameAI.aiWalk) (-2) Nothing
               , MFrameT (Just GameAI.aiWalk)   1  Nothing
               ]

mutantMoveStartWalk :: MMoveT
mutantMoveStartWalk = MMoveT "mutantMoveStartWalk" frameWalk01 frameWalk04 mutantFramesStartWalk (Just mutantWalkLoop)

mutantWalk :: EntThink
mutantWalk =
  GenericEntThink "mutant_walk" $ \selfRef -> do
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
mutantRun =
  GenericEntThink "mutantRun" $ \selfRef -> do
    self <- readRef selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then mutantMoveStand
                   else mutantMoveRun

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

mutantHitLeft :: EntThink
mutantHitLeft =
  GenericEntThink "mutant_hit_left" $ \selfRef -> do
    self <- readRef selfRef

    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) 8

    r <- Lib.rand
    hit <- GameWeapon.fireHit selfRef aim (10 + fromIntegral (r `mod` 5)) 100

    soundIdx <- if hit
                  then use $ mMutantGlobals.mMutantSoundHit
                  else use $ mMutantGlobals.mMutantSoundSwing

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

    return True

mutantHitRight :: EntThink
mutantHitRight =
  GenericEntThink "mutant_hit_right" $ \selfRef -> do
    self <- readRef selfRef

    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMaxs._x) 8

    r <- Lib.rand
    hit <- GameWeapon.fireHit selfRef aim (10 + fromIntegral (r `mod` 5)) 100

    soundIdx <- if hit
                  then use $ mMutantGlobals.mMutantSoundHit2
                  else use $ mMutantGlobals.mMutantSoundSwing

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

    return True

mutantCheckReFire :: EntThink
mutantCheckReFire =
  GenericEntThink "mutant_check_refire" $ \selfRef -> do
    self <- readRef selfRef

    done <- case self^.eEnemy of
              Nothing ->
                return True

              Just enemyRef -> do
                enemy <- readRef enemyRef

                return $ if not (enemy^.eInUse) || (enemy^.eHealth) <= 0
                           then True
                           else False

    unless done $ do
      skillValue <- liftM (^.cvValue) skillCVar
      r <- Lib.randomF

      let Just enemyRef = self^.eEnemy
      enemy <- readRef enemyRef

      when (skillValue == 3 && r < 0.5 || GameUtil.range self enemy == Constants.rangeMelee) $
        modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameAttack09)

    return True

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
mutantMelee =
  GenericEntThink "mutant_melee" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveAttack)
    return True

mutantJumpTouch :: EntTouch
mutantJumpTouch =
  GenericEntTouch "mutant_jump_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef

    if (self^.eHealth) <= 0
      then
        modifyRef selfRef (\v -> v & eTouch .~ Nothing)

      else do
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
            self' <- readRef selfRef

            when (isJust (self'^.eGroundEntity)) $ do
              modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameAttack02
                                            & eTouch .~ Nothing)

          else
            modifyRef selfRef (\v -> v & eTouch .~ Nothing)

mutantJumpTakeOff :: EntThink
mutantJumpTakeOff =
  GenericEntThink "mutant_jump_takeoff" $ \selfRef -> do
    self <- readRef selfRef

    soundSight <- use $ mMutantGlobals.mMutantSoundSight
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0

    let (Just forward, _, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True False False
        V3 a b _ = fmap (* 600) forward

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyRef selfRef (\v -> v & eEntityState.esOrigin._z +~ 1
                                  & eVelocity .~ V3 a b 250
                                  & eGroundEntity .~ Nothing
                                  & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked)
                                  & eMonsterInfo.miAttackFinished .~ levelTime + 3
                                  & eTouch .~ Just mutantJumpTouch)

    return True

mutantCheckLanding :: EntThink
mutantCheckLanding =
  GenericEntThink "mutant_check_landing" $ \selfRef -> do
    self <- readRef selfRef

    case self^.eGroundEntity of
      Just _ -> do
        soundThud <- use $ mMutantGlobals.mMutantSoundThud
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) Constants.chanWeapon soundThud 1 Constants.attnNorm 0

        modifyRef selfRef (\v -> v & eMonsterInfo.miAttackFinished .~ 0
                                      & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiDucked)))

      Nothing -> do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        let nextFrame = if levelTime > (self^.eMonsterInfo.miAttackFinished)
                          then frameAttack02
                          else frameAttack05

        modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ nextFrame)

    return True

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
mutantJump =
  GenericEntThink "mutant_jump" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just mutantMoveJump)
    return True

mutantCheckMelee :: EntThink
mutantCheckMelee =
  GenericEntThink "mutant_check_melee" $ \selfRef -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    return $ if GameUtil.range self enemy == Constants.rangeMelee
               then True
               else False

mutantCheckJump :: EntThink
mutantCheckJump =
  GenericEntThink "mutant_check_jump" $ \selfRef -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    if | (self^.eAbsMin._z) > ((enemy^.eAbsMin._z) + 0.75 * (enemy^.eSize._z)) ->
           return False

       | (self^.eAbsMax._z) < ((enemy^.eAbsMin._z) + 0.25 * (enemy^.eSize._z)) ->
           return False

       | otherwise -> do
           let v0 = (self^.eEntityState.esOrigin._x) - (enemy^.eEntityState.esOrigin._x)
               v1 = (self^.eEntityState.esOrigin._y) - (enemy^.eEntityState.esOrigin._y)
               v2 = 0
               distance = norm (V3 v0 v1 v2)

           r <- Lib.randomF

           return $ if | distance < 100 -> False
                       | distance > 100 && r < 0.9 -> False
                       | otherwise -> True

mutantCheckAttack :: EntThink
mutantCheckAttack =
  GenericEntThink "mutant_checkattack" $ \selfRef -> do
    self <- readRef selfRef

    case self^.eEnemy of
      Nothing ->
        return False

      Just enemyRef -> do
        enemy <- readRef enemyRef

        if (enemy^.eHealth) <= 0
          then
            return False

          else do
            melee <- think mutantCheckMelee selfRef

            if melee
              then do
                modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asMelee)
                return True

              else do
                jump <- think mutantCheckJump selfRef

                if jump
                  then do
                    modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asMissile)
                    -- FIXME: play a jump sound here
                    return True

                  else
                    return False

mutantFramesPain1 :: V.Vector MFrameT
mutantFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove) (-8) Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               ]

mutantMovePain1 :: MMoveT
mutantMovePain1 = MMoveT "mutantMovePain1" framePain101 framePain105 mutantFramesPain1 (Just mutantRun)

mutantFramesPain2 :: V.Vector MFrameT
mutantFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-24) Nothing
               , MFrameT (Just GameAI.aiMove)   11  Nothing
               , MFrameT (Just GameAI.aiMove)    5  Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)    6  Nothing
               , MFrameT (Just GameAI.aiMove)    4  Nothing
               ]

mutantMovePain2 :: MMoveT
mutantMovePain2 = MMoveT "mutantMovePain2" framePain201 framePain206 mutantFramesPain2 (Just mutantRun)

mutantFramesPain3 :: V.Vector MFrameT
mutantFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-22) Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    6  Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               ]

mutantMovePain3 :: MMoveT
mutantMovePain3 = MMoveT "mutantMovePain3" framePain301 framePain311 mutantFramesPain3 (Just mutantRun)

mutantPain :: EntPain
mutantPain =
  GenericEntPain "mutant_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        r <- Lib.randomF

        (soundPain, currentMove) <- if | r < 0.33 -> do
                                           soundPain <- use $ mMutantGlobals.mMutantSoundPain1
                                           return (soundPain, mutantMovePain1)

                                       | r < 0.66 -> do
                                           soundPain <- use $ mMutantGlobals.mMutantSoundPain2
                                           return (soundPain, mutantMovePain2)

                                       | otherwise -> do
                                           soundPain <- use $ mMutantGlobals.mMutantSoundPain1
                                           return (soundPain, mutantMovePain3)

        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0

mutantDead :: EntThink
mutantDead =
  GenericEntThink "mutant_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster))

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    void $ think M.flyCheck selfRef

    return True

mutantFramesDeath1 :: V.Vector MFrameT
mutantFramesDeath1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

mutantMoveDeath1 :: MMoveT
mutantMoveDeath1 = MMoveT "mutantMoveDeath1" frameDeath101 frameDeath109 mutantFramesDeath1 (Just mutantDead)

mutantFramesDeath2 :: V.Vector MFrameT
mutantFramesDeath2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

mutantMoveDeath2 :: MMoveT
mutantMoveDeath2 = MMoveT "mutantMoveDeath2" frameDeath201 frameDeath210 mutantFramesDeath2 (Just mutantDead)

mutantDie :: EntDie
mutantDie =
  GenericEntDie "mutant_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    if | (self^.eHealth) <= (self^.eGibHealth) -> do
           soundIdx <- soundIndex (Just "misc/udeath.wav")
           sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic

           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

           GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)

       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()

       | otherwise -> do
           soundDeath <- use $ mMutantGlobals.mMutantSoundDeath
           sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0

           r <- Lib.randomF

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         & eEntityState.esSkinNum .~ 1
                                         & eMonsterInfo.miCurrentMove .~ Just (if r < 0.5 then mutantMoveDeath1 else mutantMoveDeath2))

{-
- QUAKED monster_mutant (1 .5 0) (-32 -32 -24) (32 32 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterMutant :: EntThink
spMonsterMutant =
  GenericEntThink "SP_monster_mutant" $ \selfRef -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then do
        GameUtil.freeEdict selfRef
        return False

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

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

        void $ think GameAI.walkMonsterStart selfRef

        return True
