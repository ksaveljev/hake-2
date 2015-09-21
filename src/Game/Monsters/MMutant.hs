{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MMutant where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=))
import Control.Monad (when, void, unless, liftM)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (isJust)
import Linear (V3(..), _x, norm, normalize)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import qualified Game.GameAI as GameAI
import qualified Game.GameCombat as GameCombat
import qualified Game.GameUtil as GameUtil
import qualified Game.GameWeapon as GameWeapon
import qualified Util.Lib as Lib

frameAttack01 :: Int
frameAttack01 = 0

frameAttack02 :: Int
frameAttack02 = 1

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
  GenericEntThink "mutant_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just mutantMoveStand
    return True

mutantIdleLoop :: EntThink
mutantIdleLoop =
  GenericEntThink "mutant_idle_loop" $ \(EdictReference selfIdx) -> do
    r <- Lib.randomF

    when (r < 0.75) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= frameStand155

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
  GenericEntThink "mutant_idle" $ \selfRef@(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just mutantMoveIdle

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
  GenericEntThink "mutant_walk_loop" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just mutantMoveWalk
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
  GenericEntThink "mutant_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just mutantMoveStartWalk
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
  GenericEntThink "mutantRun" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then mutantMoveStand
                   else mutantMoveRun

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

mutantHitLeft :: EntThink
mutantHitLeft =
  GenericEntThink "mutant_hit_left" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

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
  GenericEntThink "mutant_hit_right" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

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
  GenericEntThink "mutant_check_refire" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    done <- case self^.eEnemy of
              Nothing ->
                return True

              Just (EdictReference enemyIdx) -> do
                Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

                return $ if not (enemy^.eInUse) || (enemy^.eHealth) <= 0
                           then True
                           else False

    unless done $ do
      skillValue <- liftM (^.cvValue) skillCVar
      r <- Lib.randomF

      let Just (EdictReference enemyIdx) = self^.eEnemy
      Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

      when (skillValue == 3 && r < 0.5 || GameUtil.range self enemy == Constants.rangeMelee) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= frameAttack09

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
  GenericEntThink "mutant_melee" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just mutantMoveAttack
    return True

mutantJumpTouch :: EntTouch
mutantJumpTouch =
  GenericEntTouch "mutant_jump_touch" $ \selfRef@(EdictReference selfIdx) otherRef@(EdictReference otherIdx) _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eHealth) <= 0
      then
        gameBaseGlobals.gbGEdicts.ix selfIdx.eTouch .= Nothing

      else do
        Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx

        when ((other^.eTakeDamage) /= 0 && norm (self^.eVelocity) > 400) $ do
          r <- Lib.randomF

          let normal = normalize (self^.eVelocity)
              point = (self^.eEntityState.esOrigin) + fmap (* (self^.eMaxs._x)) normal
              damage = truncate (40 + 10 * r)

          GameCombat.damage otherRef selfRef selfRef (self^.eVelocity) point normal damage damage 0 Constants.modUnknown

        bottom <- M.checkBottom selfRef

        if not bottom
          then do
            Just self' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

            when (isJust (self'^.eGroundEntity)) $ do
              zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
                eMonsterInfo.miNextFrame .= frameAttack02
                eTouch .= Nothing

          else
            gameBaseGlobals.gbGEdicts.ix selfIdx.eTouch .= Nothing

mutantJumpTakeOff :: EntThink
mutantJumpTakeOff =
  GenericEntThink "mutant_jump_takeoff" $ \_ -> do
    io (putStrLn "MMutant.mutantJumpTakeOff") >> undefined -- TODO

mutantCheckLanding :: EntThink
mutantCheckLanding =
  GenericEntThink "mutant_check_landing" $ \_ -> do
    io (putStrLn "MMutant.mutantCheckLanding") >> undefined -- TODO

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
  GenericEntThink "mutant_jump" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just mutantMoveJump
    return True

mutantCheckMelee :: EntThink
mutantCheckMelee =
  GenericEntThink "mutant_check_melee" $ \_ -> do
    io (putStrLn "MMutant.mutantCheckMelee") >> undefined -- TODO

mutantCheckJump :: EntThink
mutantCheckJump =
  GenericEntThink "mutant_check_jump" $ \_ -> do
    io (putStrLn "MMutant.mutantCheckJump") >> undefined -- TODO

mutantCheckAttack :: EntThink
mutantCheckAttack =
  GenericEntThink "mutant_checkattack" $ \_ -> do
    io (putStrLn "MMutant.mutantCheckAttack") >> undefined -- TODO

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
  GenericEntPain "mutant_pain" $ \_ _ _ _ -> do
    io (putStrLn "MMutant.mutantPain") >> undefined -- TODO

mutantDead :: EntThink
mutantDead =
  GenericEntThink "mutant_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMins .= V3 (-16) (-16) (-24)
      eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)

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
  GenericEntDie "mutant_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MMutant.mutantDie") >> undefined -- TODO

{-
- QUAKED monster_mutant (1 .5 0) (-32 -32 -24) (32 32 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterMutant :: EntThink
spMonsterMutant =
  GenericEntThink "SP_monster_mutant" $ \_ -> do
    io (putStrLn "MMutant.spMonsterMutant") >> undefined -- TODO
