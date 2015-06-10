{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MActor where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (%=))
import Control.Monad (when, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

frameAttack01 :: Int
frameAttack01 = 0

frameAttack04 :: Int
frameAttack04 = 3

frameDeath101 :: Int
frameDeath101 = 4

frameDeath107 :: Int
frameDeath107 = 10

frameDeath201 :: Int
frameDeath201 = 11

frameDeath213 :: Int
frameDeath213 = 23

frameFlip01 :: Int
frameFlip01 = 39

frameFlip14 :: Int
frameFlip14 = 52

framePain101 :: Int
framePain101 = 74

framePain103 :: Int
framePain103 = 76

framePain201 :: Int
framePain201 = 77

framePain203 :: Int
framePain203 = 79

framePain301 :: Int
framePain301 = 80

framePain303 :: Int
framePain303 = 82

frameRun02 :: Int
frameRun02 = 93

frameRun07 :: Int
frameRun07 = 98

frameStand101 :: Int
frameStand101 = 128

frameStand140 :: Int
frameStand140 = 167

frameTaunt01 :: Int
frameTaunt01 = 234

frameTaunt17 :: Int
frameTaunt17 = 250

frameWalk01 :: Int
frameWalk01 = 251

frameWalk08 :: Int
frameWalk08 = 258

actorNames :: V.Vector B.ByteString
actorNames = V.fromList [ "Hellrot", "Tokay", "Killme", "Disruptor"
                        , "Adrianator", "Rambear", "Titus", "Bitterman"
                        ]

actorStand :: EntThink
actorStand =
  GenericEntThink "actor_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just actorMoveStand

    -- randomize on startup
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    when (levelTime < 1.0) $ do
      Just (Just currentMove) <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove
      r <- Lib.rand
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esFrame .= (currentMove^.mmFirstFrame) + (fromIntegral r `mod` ((currentMove^.mmLastFrame) - (currentMove^.mmFirstFrame) + 1))

    return True

actorFramesStand :: V.Vector MFrameT
actorFramesStand =
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

actorMoveStand :: MMoveT
actorMoveStand = MMoveT "actorMoveStand" frameStand101 frameStand140 actorFramesStand Nothing

actorFramesWalk :: V.Vector MFrameT
actorFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk)  3 Nothing
               , MFrameT (Just GameAI.aiWalk)  2 Nothing
               , MFrameT (Just GameAI.aiWalk)  7 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk)  1 Nothing
               , MFrameT (Just GameAI.aiWalk)  4 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               ]

actorMoveWalk :: MMoveT
actorMoveWalk = MMoveT "actorMoveWalk" frameWalk01 frameWalk08 actorFramesWalk Nothing

actorWalk :: EntThink
actorWalk =
  GenericEntThink "actor_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just actorMoveWalk
    return True

actorFramesRun :: V.Vector MFrameT
actorFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)   4  Nothing
               , MFrameT (Just GameAI.aiRun)  15  Nothing
               , MFrameT (Just GameAI.aiRun)  15  Nothing
               , MFrameT (Just GameAI.aiRun)   8  Nothing
               , MFrameT (Just GameAI.aiRun)  20  Nothing
               , MFrameT (Just GameAI.aiRun)  15  Nothing
               , MFrameT (Just GameAI.aiRun)   8  Nothing
               , MFrameT (Just GameAI.aiRun)  17  Nothing
               , MFrameT (Just GameAI.aiRun)  12  Nothing
               , MFrameT (Just GameAI.aiRun) (-2) Nothing
               , MFrameT (Just GameAI.aiRun) (-2) Nothing
               , MFrameT (Just GameAI.aiRun) (-1) Nothing
               ]

actorMoveRun :: MMoveT
actorMoveRun = MMoveT "actorMoveRun" frameRun02 frameRun07 actorFramesRun Nothing

actorRun :: EntThink
actorRun =
  GenericEntThink "actor_run" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if levelTime < (self^.eEdictTiming.etPainDebounceTime) && isNothing (self^.eEdictOther.eoEnemy)
      then
        case self^.eMoveTarget of
          Nothing -> void $ think actorStand selfRef
          _ -> void $ think actorWalk selfRef

      else do
        if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
          then void $ think actorStand selfRef
          else gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just actorMoveRun

    return True

actorFramesPain1 :: V.Vector MFrameT
actorFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               ]

actorMovePain1 :: MMoveT
actorMovePain1 = MMoveT "actorMovePain1" framePain101 framePain103 actorFramesPain1 (Just actorRun)

actorFramesPain2 :: V.Vector MFrameT
actorFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

actorMovePain2 :: MMoveT
actorMovePain2 = MMoveT "actorMovePain2" framePain201 framePain203 actorFramesPain2 (Just actorRun)

actorFramesPain3 :: V.Vector MFrameT
actorFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

actorMovePain3 :: MMoveT
actorMovePain3 = MMoveT "actorMovePain3" framePain301 framePain303 actorFramesPain3 (Just actorRun)

actorFramesFlipOff :: V.Vector MFrameT
actorFramesFlipOff =
    V.fromList [ MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               ]

actorFramesMoveFlipOff :: MMoveT
actorFramesMoveFlipOff = MMoveT "actorFramesMoveFlipOff" frameFlip01 frameFlip14 actorFramesFlipOff (Just actorRun)

actorFramesTaunt :: V.Vector MFrameT
actorFramesTaunt =
    V.fromList [ MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               ]

actorMoveTaunt :: MMoveT
actorMoveTaunt = MMoveT "actorMoveTaunt" frameTaunt01 frameTaunt17 actorFramesTaunt (Just actorRun)

messages :: V.Vector B.ByteString
messages = V.fromList [ "Watch it", "#$@*&", "Idiot", "Check your targets" ]

actorPain :: EntPain
actorPain =
  GenericEntPain "actor_pain" $ \_ _ _ _ -> do
    io (putStrLn "MActor.actorPain") >> undefined -- TODO

actorDead :: EntThink
actorDead =
  GenericEntThink "actor_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEdictMinMax.eMins .= V3 (-16) (-16) (-24)
      eEdictMinMax.eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eEdictAction.eaNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

actorFramesDeath1 :: V.Vector MFrameT
actorFramesDeath1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove) (-13) Nothing
               , MFrameT (Just GameAI.aiMove)   14  Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               ]

actorMoveDeath1 :: MMoveT
actorMoveDeath1 = MMoveT "actorMoveDeath1" frameDeath101 frameDeath107 actorFramesDeath1 (Just actorDead)

actorFramesDeath2 :: V.Vector MFrameT
actorFramesDeath2 =
    V.fromList [ MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    7  Nothing
               , MFrameT (Just GameAI.aiMove)  (-6) Nothing
               , MFrameT (Just GameAI.aiMove)  (-5) Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-9) Nothing
               , MFrameT (Just GameAI.aiMove) (-13) Nothing
               , MFrameT (Just GameAI.aiMove) (-13) Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               ]

actorMoveDeath2 :: MMoveT
actorMoveDeath2 = MMoveT "actorMoveDeath2" frameDeath201 frameDeath213 actorFramesDeath2 (Just actorDead)

actorDie :: EntDie
actorDie =
  GenericEntDie "actor_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MActor.actorDie") >> undefined -- TODO

actorFire :: EntThink
actorFire =
  GenericEntThink "actor_fire" $ \selfRef@(EdictReference selfIdx) -> do
    actorMachineGun selfRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if levelTime >= (self^.eMonsterInfo.miPauseTime)
      then gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiHoldFrame))
      else gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiHoldFrame)

    return True

actorFramesAttack :: V.Vector MFrameT
actorFramesAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) (-2) (Just actorFire)
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               , MFrameT (Just GameAI.aiCharge)   3  Nothing
               , MFrameT (Just GameAI.aiCharge)   2  Nothing
               ]

actorMoveAttack :: MMoveT
actorMoveAttack = MMoveT "actorMoveAttack" frameAttack01 frameAttack04 actorFramesAttack (Just actorRun)

actorAttack :: EntThink
actorAttack =
  GenericEntThink "actor_attack" $ \(EdictReference selfIdx) -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    r <- Lib.rand
    let n = (r .&. 15) + 3 + 7

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo) $ do
      miCurrentMove .= Just actorMoveAttack
      miPauseTime .= levelTime + (fromIntegral n) * Constants.frameTime

    return True

actorUse :: EntUse
actorUse =
  GenericEntUse "actor_use" $ \_ _ _ -> do
    io (putStrLn "MActor.actorUse") >> undefined -- TODO

{-
- QUAKED target_actor (.5 .3 0) (-8 -8 -8) (8 8 8) JUMP SHOOT ATTACK x HOLD
- BRUTAL JUMP jump in set direction upon reaching this target SHOOT take a
- single shot at the pathtarget ATTACK attack pathtarget until it or actor
- is dead
- 
- "target" next target_actor "pathtarget" target of any action to be taken
- at this point "wait" amount of time actor should pause at this point
- "message" actor will "say" this to the player
- 
- for JUMP only: "speed" speed thrown forward (default 200) "height" speed
- thrown upwards (default 200)
-}
targetActorTouch :: EntTouch
targetActorTouch =
  GenericEntTouch "target_actor_touch" $ \_ _ _ _ -> do
    io (putStrLn "MActor.targetActorTouch") >> undefined -- TODO

actorMachineGun :: EdictReference -> Quake ()
actorMachineGun _ = do
    io (putStrLn "MActor.actorMachineGun") >> undefined -- TODO

spMiscActor :: EdictReference -> Quake ()
spMiscActor _ = io (putStrLn "MActor.spMiscActor") >> undefined -- TODO

spTargetActor :: EdictReference -> Quake ()
spTargetActor _ = io (putStrLn "MActor.spTargetActor") >> undefined -- TODO
