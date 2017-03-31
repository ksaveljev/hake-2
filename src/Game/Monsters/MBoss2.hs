{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MBoss2 where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (&), (.~), (%~))
import Control.Monad (void, when, unless, liftM)
import Data.Bits ((.&.), (.|.))
import Linear (V3(..), norm)
import qualified Data.Vector as V

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
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil
import qualified Game.Monsters.MSuperTank as MSuperTank
import qualified Util.Lib as Lib

modelScale :: Float
modelScale = 1.0

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
  GenericEntThink "boss2_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveStand)
    return True

boss2Run :: EntThink
boss2Run =
  GenericEntThink "boss2_run" $ \selfRef -> do
    self <- readRef selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then boss2MoveStand
                   else boss2MoveRun

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

boss2Walk :: EntThink
boss2Walk =
  GenericEntThink "boss2_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveWalk)
    return True

boss2Attack :: EntThink
boss2Attack =
  GenericEntThink "boss2_attack" $ \selfRef -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    let vec = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        range = norm vec

    action <- if range <= 125
                then
                  return boss2MoveAttackPreMg
                else do
                  r <- Lib.randomF
                  return $ if r <= 0.6
                             then boss2MoveAttackPreMg
                             else boss2MoveAttackRocket

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

boss2AttackMg :: EntThink
boss2AttackMg =
  GenericEntThink "boss2_attack_mg" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveAttackMg)
    return True

boss2ReAttackMg :: EntThink
boss2ReAttackMg =
  GenericEntThink "boss2_reattack_mg" $ \selfRef -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    action <- if GameUtil.inFront self enemy
                then do
                  r <- Lib.randomF
                  return $ if r <= 0.7
                             then boss2MoveAttackMg
                             else boss2MoveAttackPostMg
                else
                  return boss2MoveAttackPostMg

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

boss2Pain :: EntPain
boss2Pain =
  GenericEntPain "boss2_pain" $ \selfRef _ _ damage -> do
    self <- readRef selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

      (soundPain, currentMove) <- if | damage < 10 -> do
                                         soundPain <- use $ mBoss2Globals.mb2SoundPain3
                                         return (soundPain, boss2MovePainLight)

                                     | damage < 30 -> do
                                         soundPain <- use $ mBoss2Globals.mb2SoundPain1
                                         return (soundPain, boss2MovePainLight)

                                     | otherwise -> do
                                         soundPain <- use $ mBoss2Globals.mb2SoundPain2
                                         return (soundPain, boss2MovePainHeavy)

      sound <- use $ gameBaseGlobals.gbGameImport.giSound
      sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNone 0
      modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

boss2Dead :: EntThink
boss2Dead =
  GenericEntThink "boss2_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-56) (-56) 0
                                  & eMaxs .~ V3 56 56 80
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

boss2Die :: EntDie
boss2Die =
  GenericEntDie "boss2_die" $ \selfRef _ _ _ _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundDeath <- use $ mBoss2Globals.mb2SoundDeath

    sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNone 0

    modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                  & eTakeDamage .~ Constants.damageNo
                                  & eCount .~ 0
                                  & eMonsterInfo.miCurrentMove .~ Just boss2MoveDeath)

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

{-
- QUAKED monster_boss2 (1 .5 0) (-56 -56 0) (56 56 80) Ambush Trigger_Spawn
- Sight
-}
spMonsterBoss2 :: Ref EdictT -> Quake ()
spMonsterBoss2 selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex (Just "bosshovr/bhvpain1.wav") >>= (mBoss2Globals.mb2SoundPain1 .=)
        soundIndex (Just "bosshovr/bhvpain2.wav") >>= (mBoss2Globals.mb2SoundPain2 .=)
        soundIndex (Just "bosshovr/bhvpain3.wav") >>= (mBoss2Globals.mb2SoundPain3 .=)
        soundIndex (Just "bosshovr/bhvdeth1.wav") >>= (mBoss2Globals.mb2SoundDeath .=)
        soundIndex (Just "bosshovr/bhvunqv1.wav") >>= (mBoss2Globals.mb2SoundSearch1 .=)

        soundIdx <- soundIndex (Just "bosshovr/bhvengn1.wav")
        modelIdx <- modelIndex (Just "models/monsters/boss2/tris.md2")

        modifyRef selfRef (\v -> v & eEntityState.esSound .~ soundIdx
                                      & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-56) (-56) 0
                                      & eMaxs .~ V3 56 56 80
                                      & eHealth .~ 2000
                                      & eGibHealth .~ (-200)
                                      & eMass .~ 1000
                                      & eFlags %~ (.|. Constants.flImmuneLaser)
                                      & ePain .~ Just boss2Pain
                                      & eDie .~ Just boss2Die
                                      & eMonsterInfo.miStand .~ Just boss2Stand
                                      & eMonsterInfo.miWalk .~ Just boss2Walk
                                      & eMonsterInfo.miRun .~ Just boss2Run
                                      & eMonsterInfo.miAttack .~ Just boss2Attack
                                      & eMonsterInfo.miSearch .~ Just boss2Search
                                      & eMonsterInfo.miCheckAttack .~ Just boss2CheckAttack)

        linkEntity selfRef

        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveStand
                                      & eMonsterInfo.miScale .~ modelScale)

        void $ think GameAI.flyMonsterStart selfRef
