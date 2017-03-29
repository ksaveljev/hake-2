{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MBoss31 where

import Control.Lens (use, ix, (.=), zoom, preuse, (^.), (&), (.~), (%~), (+~))
import Control.Monad (void, when, unless, liftM)
import Data.Bits ((.&.))
import Linear (norm, normalize, _z)
import qualified Data.Vector as V

import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil
import qualified Game.Monster as Monster
import qualified Game.Monsters.MBoss32 as MBoss32
import qualified Game.Monsters.MFlash as MFlash
import qualified Game.Monsters.MSuperTank as MSuperTank
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

frameAttack101 :: Int
frameAttack101 = 0

frameAttack108 :: Int
frameAttack108 = 7

frameAttack109 :: Int
frameAttack109 = 8

frameAttack114 :: Int
frameAttack114 = 13

frameAttack115 :: Int
frameAttack115 = 14

frameAttack118 :: Int
frameAttack118 = 17

frameAttack201 :: Int
frameAttack201 = 18

frameAttack208 :: Int
frameAttack208 = 25

frameAttack213 :: Int
frameAttack213 = 30

frameDeath01 :: Int
frameDeath01 = 31

frameDeath50 :: Int
frameDeath50 = 80

framePain101 :: Int
framePain101 = 81

framePain103 :: Int
framePain103 = 83

framePain201 :: Int
framePain201 = 84

framePain203 :: Int
framePain203 = 86

framePain301 :: Int
framePain301 = 87

framePain325 :: Int
framePain325 = 111

frameStand01 :: Int
frameStand01 = 112

frameStand51 :: Int
frameStand51 = 162

frameWalk01 :: Int
frameWalk01 = 163

frameWalk05 :: Int
frameWalk05 = 167

frameWalk06 :: Int
frameWalk06 = 168

frameWalk19 :: Int
frameWalk19 = 181

frameWalk20 :: Int
frameWalk20 = 182

frameWalk25 :: Int
frameWalk25 = 187

jorgSearch :: EntThink
jorgSearch =
  GenericEntThink "jorg_search" $ \selfRef -> do
    r <- Lib.randomF

    soundSearch <- if | r <= 0.3 -> use (mBoss31Globals.mb31SoundSearch1)
                      | r <= 0.6 -> use (mBoss31Globals.mb31SoundSearch2)
                      | otherwise -> use (mBoss31Globals.mb31SoundSearch3)

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0

    return True

jorgIdle :: EntThink
jorgIdle =
  GenericEntThink "jorg_idle" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mBoss31Globals.mb31SoundIdle
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnNorm 0

    return True

jorgDeathHit :: EntThink
jorgDeathHit =
  GenericEntThink "jorg_death_hit" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundDeathHit <- use $ mBoss31Globals.mb31SoundDeathHit
    sound (Just selfRef) Constants.chanBody soundDeathHit 1 Constants.attnNorm 0

    return True

jorgStepLeft :: EntThink
jorgStepLeft =
  GenericEntThink "jorg_step_left" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundStepLeft <- use $ mBoss31Globals.mb31SoundStepLeft
    sound (Just selfRef) Constants.chanBody soundStepLeft 1 Constants.attnNorm 0

    return True

jorgStepRight :: EntThink
jorgStepRight =
  GenericEntThink "jorg_step_right" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundStepRight <- use $ mBoss31Globals.mb31SoundStepRight
    sound (Just selfRef) Constants.chanBody soundStepRight 1 Constants.attnNorm 0

    return True

jorgStand :: EntThink
jorgStand =
  GenericEntThink "jorg_stand" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just jorgMoveStand)
    return True

jorgReAttack1 :: EntThink
jorgReAttack1 =
  GenericEntThink "jorg_reattack1" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    -- Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx
    vis <- GameUtil.visible selfRef enemyRef

    if vis
      then do
        r <- Lib.randomF

        if r < 0.9
          then
            modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just jorgMoveAttack1)

          else do
            modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ 0
                                          & eMonsterInfo.miCurrentMove .~ Just jorgMoveEndAttack1)
      else do
        modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ 0
                                      & eMonsterInfo.miCurrentMove .~ Just jorgMoveEndAttack1)

    return True

jorgAttack1 :: EntThink
jorgAttack1 =
  GenericEntThink "jorg_attack1" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ (Just jorgMoveAttack1))
    return True

jorgPain :: EntPain
jorgPain =
  GenericEntPain "jorg_pain" $ \selfRef _ _ damage -> do
    self <- readEdictT selfRef
    
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyEdictT selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
      
    modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ 0)
    
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    -- Lessen the chance of him going into his pain frames if he takes
    -- little damage
    skipPainFrames <- if damage <= 40
                        then do
                          r <- Lib.randomF
                          return (r <= 0.6)
                        else
                          return False
                          
    unless (levelTime < (self^.ePainDebounceTime) || skipPainFrames) $ do
      -- If he's entering his attack1 or using attack1, lessen the chance
      -- of him going into pain
      done <- checkAttackFrames self
      
      unless done $ do
        modifyEdictT selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        
        skillValue <- liftM (^.cvValue) skillCVar
        
        -- no pain anims in nightmare
        unless (skillValue == 3) $ do
          soundMove <- if | damage <= 50 -> do
                              soundPain <- use $ mBoss31Globals.mb31SoundPain1
                              return $ Just (soundPain, jorgMovePain1)
                          | damage <= 100 -> do
                              soundPain <- use $ mBoss31Globals.mb31SoundPain2
                              return $ Just (soundPain, jorgMovePain2)
                          | otherwise -> do
                              r <- Lib.randomF
                              if r <= 0.3
                                then do
                                  soundPain <- use $ mBoss31Globals.mb31SoundPain3
                                  return $ Just (soundPain, jorgMovePain3)
                                else
                                  return Nothing
          
          case soundMove of
            Nothing ->
              return ()
            
            Just (soundPain, currentMove) -> do
              sound <- use $ gameBaseGlobals.gbGameImport.giSound
              sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
              modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
  
  where checkAttackFrames :: EdictT -> Quake Bool
        checkAttackFrames self = do
          if | (self^.eEntityState.esFrame) >= frameAttack101 && (self^.eEntityState.esFrame) <= frameAttack108 -> do
                 r <- Lib.randomF
                 return (r <= 0.005)
                 
             | (self^.eEntityState.esFrame) >= frameAttack109 && (self^.eEntityState.esFrame) <= frameAttack114 -> do
                 r <- Lib.randomF
                 return (r <= 0.00005)
                 
             | (self^.eEntityState.esFrame) >= frameAttack201 && (self^.eEntityState.esFrame) <= frameAttack208 -> do
                 r <- Lib.randomF
                 return (r <= 0.005)
                 
             | otherwise ->
                 return False

jorgBFG :: EntThink
jorgBFG =
  GenericEntThink "jorgBFG" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef
    
    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2JorgBfg1) forward right
        vec = (enemy^.eEntityState.esOrigin) & _z +~ fromIntegral (enemy^.eViewHeight)
        dir = normalize (vec - start)
    
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundAttack <- use $ mBoss31Globals.mb31SoundAttack2
    
    sound (Just selfRef) Constants.chanVoice soundAttack 1 Constants.attnNorm 0
    Monster.monsterFireBFG selfRef start dir 50 300 100 200 Constants.mz2JorgBfg1
    return True

jorgFireBulletRight :: EntThink
jorgFireBulletRight =
  GenericEntThink "jorg_firebullet_right" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef
    
    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2JorgMachinegunR1) forward right
        target = ((enemy^.eEntityState.esOrigin) + fmap (* (-0.2)) (enemy^.eVelocity)) & _z +~ fromIntegral (enemy^.eViewHeight)
        forward' = normalize (target - start)
        
    Monster.monsterFireBullet selfRef start forward' 6 4 Constants.defaultBulletHspread Constants.defaultBulletVspread Constants.mz2JorgMachinegunR1
    return True

jorgFireBulletLeft :: EntThink
jorgFireBulletLeft =
  GenericEntThink "jorg_firebullet_left" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef
    
    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2JorgMachinegunL1) forward right
        target = ((enemy^.eEntityState.esOrigin) + fmap (* (-0.2)) (enemy^.eVelocity)) & _z +~ fromIntegral (enemy^.eViewHeight)
        forward' = normalize (target - start)
    
    Monster.monsterFireBullet selfRef start forward' 6 4 Constants.defaultBulletHspread Constants.defaultBulletVspread Constants.mz2JorgMachinegunL1
    return True

jorgFireBullet :: EntThink
jorgFireBullet =
  GenericEntThink "jorg_firebullet" $ \selfRef -> do
    void $ think jorgFireBulletLeft selfRef
    void $ think jorgFireBulletRight selfRef
    return True

jorgAttack :: EntThink
jorgAttack =
  GenericEntThink "jorg_attack" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef
    
    let vec = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        range = norm vec
        
    r <- Lib.randomF
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex
    
    if r <= 0.75
      then do
        soundAttack <- use $ mBoss31Globals.mb31SoundAttack1
        sound (Just selfRef) Constants.chanVoice soundAttack 1 Constants.attnNorm 0
        soundIdx <- soundIndex (Just "boss3/w_loop.wav")
        modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ soundIdx
                                      & eMonsterInfo.miCurrentMove .~ Just jorgMoveStartAttack1
                                      )
      else do
        soundAttack <- use $ mBoss31Globals.mb31SoundAttack2
        sound (Just selfRef) Constants.chanVoice soundAttack 1 Constants.attnNorm 0
        modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just jorgMoveAttack2)
    
    return True

jorgDead :: EntThink
jorgDead =
  GenericEntThink "jorg_dead" $ \_ -> do
    io (putStrLn "MBoss31.jorgDead") >> undefined -- TODO

jorgDie :: EntDie
jorgDie =
  GenericEntDie "jorg_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MBoss31.jorgDie") >> undefined -- TODO

jorgCheckAttack :: EntThink
jorgCheckAttack =
  GenericEntThink "Jorg_CheckAttack" $ \_ -> do
    io (putStrLn "MBoss31.jorgCheckAttack") >> undefined -- TODO

jorgFramesStand :: V.Vector MFrameT
jorgFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 (Just jorgIdle)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
                 -- 10
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
                 -- 20
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
                 -- 30
               , MFrameT (Just GameAI.aiStand)  0 Nothing
               , MFrameT (Just GameAI.aiStand)  0 Nothing
               , MFrameT (Just GameAI.aiStand)  0 Nothing
               , MFrameT (Just GameAI.aiStand) 19 Nothing
               , MFrameT (Just GameAI.aiStand) 11 (Just jorgStepLeft)
               , MFrameT (Just GameAI.aiStand)  0 Nothing
               , MFrameT (Just GameAI.aiStand)  0 Nothing
               , MFrameT (Just GameAI.aiStand)  6 Nothing
               , MFrameT (Just GameAI.aiStand)  9 (Just jorgStepRight)
               , MFrameT (Just GameAI.aiStand)  0 Nothing
                 -- 40
               , MFrameT (Just GameAI.aiStand)    0  Nothing
               , MFrameT (Just GameAI.aiStand)    0  Nothing
               , MFrameT (Just GameAI.aiStand)    0  Nothing
               , MFrameT (Just GameAI.aiStand)    0  Nothing
               , MFrameT (Just GameAI.aiStand)    0  Nothing
               , MFrameT (Just GameAI.aiStand)    0  Nothing
               , MFrameT (Just GameAI.aiStand)  (-2) Nothing
               , MFrameT (Just GameAI.aiStand) (-17) (Just jorgStepLeft)
               , MFrameT (Just GameAI.aiStand)    0  Nothing
               , MFrameT (Just GameAI.aiStand) (-12) Nothing
                 -- 50
               , MFrameT (Just GameAI.aiStand) (-14) (Just jorgStepRight) -- 51
               ]

jorgMoveStand :: MMoveT
jorgMoveStand = MMoveT "jorgMoveStand" frameStand01 frameStand51 jorgFramesStand Nothing

jorgFramesRun :: V.Vector MFrameT
jorgFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 17 (Just jorgStepLeft)
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun)  8 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 33 (Just jorgStepRight)
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               ]

jorgMoveRun :: MMoveT
jorgMoveRun = MMoveT "jorgMoveRun" frameWalk06 frameWalk19 jorgFramesRun Nothing

jorgFramesStartWalk :: V.Vector MFrameT
jorgFramesStartWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk)  7 Nothing
               , MFrameT (Just GameAI.aiWalk)  9 Nothing
               , MFrameT (Just GameAI.aiWalk) 15 Nothing
               ]

jorgMoveStartWalk :: MMoveT
jorgMoveStartWalk = MMoveT "jorgMoveStartWalk" frameWalk01 frameWalk05 jorgFramesStartWalk Nothing

jorgFramesWalk :: V.Vector MFrameT
jorgFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 17 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk) 12 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk) 33 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  9 Nothing
               , MFrameT (Just GameAI.aiWalk)  9 Nothing
               , MFrameT (Just GameAI.aiWalk)  9 Nothing
               ]

jorgMoveWalk :: MMoveT
jorgMoveWalk = MMoveT "jorgMoveWalk" frameWalk06 frameWalk19 jorgFramesWalk Nothing

jorgFramesEndWalk :: V.Vector MFrameT
jorgFramesEndWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  11  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   8  Nothing
               , MFrameT (Just GameAI.aiWalk) (-8) Nothing
               ]

jorgMoveEndWalk :: MMoveT
jorgMoveEndWalk = MMoveT "jorgMoveEndWalk" frameWalk20 frameWalk25 jorgFramesEndWalk Nothing

jorgWalk :: EntThink
jorgWalk =
  GenericEntThink "jorg_walk" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just jorgMoveWalk)
    return True

jorgRun :: EntThink
jorgRun =
  GenericEntThink "jorg_run" $ \selfRef -> do
    self <- readEdictT selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then jorgMoveStand
                   else jorgMoveRun

    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

jorgFramesPain3 :: V.Vector MFrameT
jorgFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-28) Nothing
               , MFrameT (Just GameAI.aiMove)  (-6) Nothing
               , MFrameT (Just GameAI.aiMove)  (-3) (Just jorgStepLeft)
               , MFrameT (Just GameAI.aiMove)  (-9) Nothing
               , MFrameT (Just GameAI.aiMove)    0  (Just jorgStepRight)
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-7) Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove) (-11) Nothing
               , MFrameT (Just GameAI.aiMove)  (-4) Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)   10  Nothing
               , MFrameT (Just GameAI.aiMove)   11  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)   10  Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)   10  Nothing
               , MFrameT (Just GameAI.aiMove)    7  (Just jorgStepLeft)
               , MFrameT (Just GameAI.aiMove)   17  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  (Just jorgStepRight)
               ]

jorgMovePain3 :: MMoveT
jorgMovePain3 = MMoveT "jorgMovePain3" framePain301 framePain325 jorgFramesPain3 (Just jorgRun)

jorgFramesPain2 :: V.Vector MFrameT
jorgFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

jorgMovePain2 :: MMoveT
jorgMovePain2 = MMoveT "jorgMovePain2" framePain201 framePain203 jorgFramesPain2 (Just jorgRun)

jorgFramesPain1 :: V.Vector MFrameT
jorgFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

jorgMovePain1 :: MMoveT
jorgMovePain1 = MMoveT "jorgMovePain1" framePain101 framePain103 jorgFramesPain1 (Just jorgRun)

jorgFramesDeath1 :: V.Vector MFrameT
jorgFramesDeath1 =
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
                 -- 10
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
                 -- 20
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
                 -- 30
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
                 -- 40
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 (Just MBoss32.makronToss)
               , MFrameT (Just GameAI.aiMove) 0 (Just MSuperTank.bossExplode) -- 50
               ]

jorgMoveDeath :: MMoveT
jorgMoveDeath = MMoveT "jorgMoveDeath" frameDeath01 frameDeath50 jorgFramesDeath1 (Just jorgDead)

jorgFramesAttack2 :: V.Vector MFrameT
jorgFramesAttack2 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just jorgBFG)
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               ]

jorgMoveAttack2 :: MMoveT
jorgMoveAttack2 = MMoveT "jorgMoveAttack2" frameAttack201 frameAttack213 jorgFramesAttack2 (Just jorgRun)

jorgFramesStartAttack1 :: V.Vector MFrameT
jorgFramesStartAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

jorgMoveStartAttack1 :: MMoveT
jorgMoveStartAttack1 = MMoveT "jorgMoveStartAttack1" frameAttack101 frameAttack108 jorgFramesStartAttack1 (Just jorgAttack1)

jorgFramesAttack1 :: V.Vector MFrameT
jorgFramesAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just jorgFireBullet)
               , MFrameT (Just GameAI.aiCharge) 0 (Just jorgFireBullet)
               , MFrameT (Just GameAI.aiCharge) 0 (Just jorgFireBullet)
               , MFrameT (Just GameAI.aiCharge) 0 (Just jorgFireBullet)
               , MFrameT (Just GameAI.aiCharge) 0 (Just jorgFireBullet)
               , MFrameT (Just GameAI.aiCharge) 0 (Just jorgFireBullet)
               ]

jorgMoveAttack1 :: MMoveT
jorgMoveAttack1 = MMoveT "jorgMoveAttack1" frameAttack109 frameAttack114 jorgFramesAttack1 (Just jorgReAttack1)

jorgFramesEndAttack1 :: V.Vector MFrameT
jorgFramesEndAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

jorgMoveEndAttack1 :: MMoveT
jorgMoveEndAttack1 = MMoveT "jorgMoveEndAttack1" frameAttack115 frameAttack118 jorgFramesEndAttack1 (Just jorgRun)

{-
- QUAKED monster_jorg (1 .5 0) (-80 -80 0) (90 90 140) Ambush Trigger_Spawn
- Sight
-}
spMonsterJorg :: EdictReference -> Quake ()
spMonsterJorg _ = io (putStrLn "MBoss31.spMonsterJorg") >> undefined -- TODO
