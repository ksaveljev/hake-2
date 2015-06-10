{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MBoss2 where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=))
import Control.Monad (void)
import Data.Bits ((.&.), (.|.))
import Linear (V3(..), norm)
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil
import qualified Game.Monsters.MSuperTank as MSuperTank
import qualified Util.Lib as Lib

frameStand30 :: Int
frameStand30 = 0

frameStand50 :: Int
frameStand50 = 20

frameStand1 :: Int
frameStand1 = 21

frameWalk1 :: Int
frameWalk1 = 50

frameWalk20 :: Int
frameWalk20 = 69

frameAttack1 :: Int
frameAttack1 = 70

frameAttack9 :: Int
frameAttack9 = 78

frameAttack10 :: Int
frameAttack10 = 79

frameAttack15 :: Int
frameAttack15 = 84

frameAttack16 :: Int
frameAttack16 = 85

frameAttack19 :: Int
frameAttack19 = 88

frameAttack20 :: Int
frameAttack20 = 89

frameAttack40 :: Int
frameAttack40 = 109

framePain2 :: Int
framePain2 = 110

framePain19 :: Int
framePain19 = 127

framePain20 :: Int
framePain20 = 128

framePain23 :: Int
framePain23 = 131

frameDeath2 :: Int
frameDeath2 = 132

frameDeath50 :: Int
frameDeath50 = 180

boss2Stand :: EntThink
boss2Stand =
  GenericEntThink "boss2_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just boss2MoveStand
    return True

boss2Run :: EntThink
boss2Run =
  GenericEntThink "boss2_run" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then boss2MoveStand
                   else boss2MoveRun

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

boss2Walk :: EntThink
boss2Walk =
  GenericEntThink "boss2_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just boss2MoveWalk
    return True

boss2Attack :: EntThink
boss2Attack =
  GenericEntThink "boss2_attack" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self^.eEdictOther.eoEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    let vec = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        range = norm vec

    action <- if range <= 125
                then return boss2MoveAttackPreMg
                else do
                  r <- Lib.randomF
                  return $ if r <= 0.6
                             then boss2MoveAttackPreMg
                             else boss2MoveAttackRocket

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

boss2AttackMg :: EntThink
boss2AttackMg =
  GenericEntThink "boss2_attack_mg" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just boss2MoveAttackMg
    return True

boss2ReAttackMg :: EntThink
boss2ReAttackMg =
  GenericEntThink "boss2_reattack_mg" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self^.eEdictOther.eoEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    action <- if GameUtil.inFront self enemy
                then do
                  r <- Lib.randomF
                  return $ if r <= 0.7
                             then boss2MoveAttackMg
                             else boss2MoveAttackPostMg
                else
                  return boss2MoveAttackPostMg

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

boss2Pain :: EntPain
boss2Pain =
  GenericEntPain "boss2_pain" $ \_ _ _ _ -> do
    io (putStrLn "MBoss2.boss2Pain") >> undefined -- TODO

boss2Dead :: EntThink
boss2Dead =
  GenericEntThink "boss2_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEdictMinMax.eMins .= V3 (-56) (-56) 0
      eEdictMinMax.eMaxs .= V3 56 56 80
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eEdictAction.eaNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

boss2Die :: EntDie
boss2Die =
  GenericEntDie "boss2_die" $ \selfRef@(EdictReference selfIdx) _ _ _ _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundDeath <- use $ mBoss2Globals.mb2SoundDeath

    sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNone 0

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEdictStatus.eDeadFlag .= Constants.deadDead
      eEdictStatus.eTakeDamage .= Constants.damageNo
      eCount .= 0
      eMonsterInfo.miCurrentMove .= Just boss2MoveDeath

boss2CheckAttack :: EntThink
boss2CheckAttack =
  GenericEntThink "Boss2_CheckAttack" $ \_ -> do
    io (putStrLn "MBoss2.boss2CheckAttack") >> undefined -- TODO

boss2Search :: EntThink
boss2Search =
  GenericEntThink "boss2_search" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSearch1 <- use $ mBoss2Globals.mb2SoundSearch1

    sound (Just selfRef) Constants.chanVoice soundSearch1 1 Constants.attnNone 0

    return True

boss2Rocket :: EntThink
boss2Rocket =
  GenericEntThink "Boss2Rocket" $ \_ -> do
    io (putStrLn "MBoss2.boss2Rocket") >> undefined -- TODO

boss2FireBulletRight :: EntThink
boss2FireBulletRight =
  GenericEntThink "boss2_firebullet_right" $ \_ -> do
    io (putStrLn "MBoss2.boss2FireBulletRight") >> undefined -- TODO

boss2FireBulletLeft :: EntThink
boss2FireBulletLeft =
  GenericEntThink "boss2_firebullet_left" $ \_ -> do
    io (putStrLn "MBoss2.boss2FireBulletLeft") >> undefined -- TODO

boss2MachineGun :: EntThink
boss2MachineGun =
  GenericEntThink "Boss2MachineGun" $ \selfRef -> do
    void $ think boss2FireBulletLeft selfRef
    void $ think boss2FireBulletRight selfRef
    return True

boss2FramesStand :: V.Vector MFrameT
boss2FramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
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

boss2MoveStand :: MMoveT
boss2MoveStand = MMoveT "boss2MoveStand" frameStand30 frameStand50 boss2FramesStand Nothing

boss2FramesFidget :: V.Vector MFrameT
boss2FramesFidget =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
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

boss2MoveFidget :: MMoveT
boss2MoveFidget = MMoveT "boss2MoveFidget" frameStand1 frameStand30 boss2FramesFidget Nothing

boss2FramesWalk :: V.Vector MFrameT
boss2FramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               ]

boss2MoveWalk :: MMoveT
boss2MoveWalk = MMoveT "boss2MoveWalk" frameWalk1 frameWalk20 boss2FramesWalk Nothing

boss2FramesRun :: V.Vector MFrameT
boss2FramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               ]

boss2MoveRun :: MMoveT
boss2MoveRun = MMoveT "boss2MoveRun" frameWalk1 frameWalk20 boss2FramesRun Nothing

boss2FramesAttackPreMg :: V.Vector MFrameT
boss2FramesAttackPreMg =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 (Just boss2AttackMg)
               ]

boss2MoveAttackPreMg :: MMoveT
boss2MoveAttackPreMg = MMoveT "boss2MoveAttackPreMg" frameAttack1 frameAttack9 boss2FramesAttackPreMg Nothing

-- Loop this
boss2FramesAttackMg :: V.Vector MFrameT
boss2FramesAttackMg =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
               , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
               , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
               , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
               , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
               , MFrameT (Just GameAI.aiCharge) 1 (Just boss2ReAttackMg)
               ]

boss2MoveAttackMg :: MMoveT
boss2MoveAttackMg = MMoveT "boss2MoveAttackMg" frameAttack10 frameAttack15 boss2FramesAttackMg Nothing

boss2FramesAttackPostMg :: V.Vector MFrameT
boss2FramesAttackPostMg =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               ]

boss2MoveAttackPostMg :: MMoveT
boss2MoveAttackPostMg = MMoveT "boss2MoveAttackPostMg" frameAttack16 frameAttack19 boss2FramesAttackPostMg (Just boss2Run)

boss2FramesAttackRocket :: V.Vector MFrameT
boss2FramesAttackRocket =
    V.fromList [ MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiMove)   (-20) (Just boss2Rocket)
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               ]

boss2MoveAttackRocket :: MMoveT
boss2MoveAttackRocket = MMoveT "boss2MoveAttackRocket" frameAttack20 frameAttack40 boss2FramesAttackRocket (Just boss2Run)

boss2FramesPainHeavy :: V.Vector MFrameT
boss2FramesPainHeavy =
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
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

boss2MovePainHeavy :: MMoveT
boss2MovePainHeavy = MMoveT "boss2MovePainHeavy" framePain2 framePain19 boss2FramesPainHeavy (Just boss2Run)

boss2FramesPainLight :: V.Vector MFrameT
boss2FramesPainLight =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

boss2MovePainLight :: MMoveT
boss2MovePainLight = MMoveT "boss2MovePainLight" framePain20 framePain23 boss2FramesPainLight (Just boss2Run)

boss2FramesDeath :: V.Vector MFrameT
boss2FramesDeath =
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
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 (Just MSuperTank.bossExplode)
               ]

boss2MoveDeath :: MMoveT
boss2MoveDeath = MMoveT "boss2MoveDeath" frameDeath2 frameDeath50 boss2FramesDeath (Just boss2Dead)

spMonsterBoss2 :: EdictReference -> Quake ()
spMonsterBoss2 _ = io (putStrLn "MBoss2.spMonsterBoss2") >> undefined -- TODO
