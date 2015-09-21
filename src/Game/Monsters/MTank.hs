{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MTank where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=))
import Control.Monad (void, when, unless, liftM)
import Data.Bits ((.&.), (.|.), complement)
import Linear (V3(..))
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameMisc as GameMisc
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

frameStand01 :: Int
frameStand01 = 0

frameStand30 :: Int
frameStand30 = 29

frameWalk01 :: Int
frameWalk01 = 30

frameWalk04 :: Int
frameWalk04 = 33

frameWalk05 :: Int
frameWalk05 = 34

frameWalk20 :: Int
frameWalk20 = 49

frameWalk21 :: Int
frameWalk21 = 50

frameWalk25 :: Int
frameWalk25 = 54

frameAttack101 :: Int
frameAttack101 = 55

frameAttack111 :: Int
frameAttack111 = 65

frameAttack116 :: Int
frameAttack116 = 70

frameAttack117 :: Int
frameAttack117 = 71

frameAttack122 :: Int
frameAttack122 = 76

frameAttack201 :: Int
frameAttack201 = 77

frameAttack238 :: Int
frameAttack238 = 114

frameAttack301 :: Int
frameAttack301 = 115

frameAttack321 :: Int
frameAttack321 = 135

frameAttack322 :: Int
frameAttack322 = 136

frameAttack330 :: Int
frameAttack330 = 144

frameAttack331 :: Int
frameAttack331 = 145

frameAttack353 :: Int
frameAttack353 = 167

frameAttack401 :: Int
frameAttack401 = 168

frameAttack429 :: Int
frameAttack429 = 196

framePain101 :: Int
framePain101 = 197

framePain104 :: Int
framePain104 = 200

framePain201 :: Int
framePain201 = 201

framePain205 :: Int
framePain205 = 205

framePain301 :: Int
framePain301 = 206

framePain316 :: Int
framePain316 = 221

frameDeath101 :: Int
frameDeath101 = 222

frameDeath132 :: Int
frameDeath132 = 253

tankSight :: EntInteract
tankSight =
  GenericEntInteract "tank_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mTankGlobals.mTankSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

tankFootStep :: EntThink
tankFootStep =
  GenericEntThink "tank_footstep" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundStep <- use $ mTankGlobals.mTankSoundStep
    sound (Just selfRef) Constants.chanBody soundStep 1 Constants.attnNorm 0
    return True

tankThud :: EntThink
tankThud =
  GenericEntThink "tank_thud" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundThud <- use $ mTankGlobals.mTankSoundThud
    sound (Just selfRef) Constants.chanBody soundThud 1 Constants.attnNorm 0
    return True

tankWindUp :: EntThink
tankWindUp =
  GenericEntThink "tank_windup" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundWindUp <- use $ mTankGlobals.mTankSoundWindUp
    sound (Just selfRef) Constants.chanWeapon soundWindUp 1 Constants.attnNorm 0
    return True

tankIdle :: EntThink
tankIdle =
  GenericEntThink "tank_idle" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mTankGlobals.mTankSoundIdle
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

tankFramesStand :: V.Vector MFrameT
tankFramesStand =
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

tankMoveStand :: MMoveT
tankMoveStand = MMoveT "tankMoveStand" frameStand01 frameStand30 tankFramesStand Nothing

tankStand :: EntThink
tankStand =
  GenericEntThink "tank_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just tankMoveStand
    return True

tankRun :: EntThink
tankRun =
  GenericEntThink "tank_run" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    case self^.eEnemy of
      Nothing ->
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiBrutal))

      Just (EdictReference enemyIdx) -> do
        Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

        case enemy^.eClient of
          Nothing -> gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiBrutal))
          Just _ -> gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiBrutal)

    Just self' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self'^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
      then do
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just tankMoveStand

      else do
        let currentMove = case self'^.eMonsterInfo.miCurrentMove of
                            Nothing -> tankMoveStartRun
                            Just move -> if (move^.mmId) == "tankMoveWalk" || (move^.mmId) == "tankMoveStartRun"
                                           then tankMoveRun
                                           else tankMoveStartRun

        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove

    return True

tankWalk :: EntThink
tankWalk =
  GenericEntThink "tank_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just tankMoveWalk
    return True

tankFramesStartWalk :: V.Vector MFrameT
tankFramesStartWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk) 11 (Just tankFootStep)
               ]

tankMoveStartWalk :: MMoveT
tankMoveStartWalk = MMoveT "tankMoveStartWalk" frameWalk01 frameWalk04 tankFramesStartWalk (Just tankWalk)

tankFramesWalk :: V.Vector MFrameT
tankFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 3 Nothing
               , MFrameT (Just GameAI.aiWalk) 2 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 (Just tankFootStep)
               , MFrameT (Just GameAI.aiWalk) 3 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 7 Nothing
               , MFrameT (Just GameAI.aiWalk) 7 Nothing
               , MFrameT (Just GameAI.aiWalk) 6 Nothing
               , MFrameT (Just GameAI.aiWalk) 6 (Just tankFootStep)
               ]

tankMoveWalk :: MMoveT
tankMoveWalk = MMoveT "tankMoveWalk" frameWalk05 frameWalk20 tankFramesWalk Nothing

tankFramesStopWalk :: V.Vector MFrameT
tankFramesStopWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 3 Nothing
               , MFrameT (Just GameAI.aiWalk) 3 Nothing
               , MFrameT (Just GameAI.aiWalk) 2 Nothing
               , MFrameT (Just GameAI.aiWalk) 2 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 (Just tankFootStep) 
               ]

tankMoveStopWalk :: MMoveT
tankMoveStopWalk = MMoveT "tankMoveStopWalk" frameWalk21 frameWalk25 tankFramesStopWalk (Just tankStand)

tankFramesStartRun :: V.Vector MFrameT
tankFramesStartRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun)  6 Nothing
               , MFrameT (Just GameAI.aiRun)  6 Nothing
               , MFrameT (Just GameAI.aiRun) 11 (Just tankFootStep)
               ]

tankMoveStartRun :: MMoveT
tankMoveStartRun = MMoveT "tankMoveStartRun" frameWalk01 frameWalk04 tankFramesStartRun (Just tankRun)

tankFramesRun :: V.Vector MFrameT
tankFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 4 Nothing
               , MFrameT (Just GameAI.aiRun) 5 Nothing
               , MFrameT (Just GameAI.aiRun) 3 Nothing
               , MFrameT (Just GameAI.aiRun) 2 Nothing
               , MFrameT (Just GameAI.aiRun) 5 Nothing
               , MFrameT (Just GameAI.aiRun) 5 Nothing
               , MFrameT (Just GameAI.aiRun) 4 Nothing
               , MFrameT (Just GameAI.aiRun) 4 (Just tankFootStep)
               , MFrameT (Just GameAI.aiRun) 3 Nothing
               , MFrameT (Just GameAI.aiRun) 5 Nothing
               , MFrameT (Just GameAI.aiRun) 4 Nothing
               , MFrameT (Just GameAI.aiRun) 5 Nothing
               , MFrameT (Just GameAI.aiRun) 7 Nothing
               , MFrameT (Just GameAI.aiRun) 7 Nothing
               , MFrameT (Just GameAI.aiRun) 6 Nothing
               , MFrameT (Just GameAI.aiRun) 6 (Just tankFootStep)
               ]

tankMoveRun :: MMoveT
tankMoveRun = MMoveT "tankMoveRun" frameWalk05 frameWalk20 tankFramesRun Nothing

tankFramesStopRun :: V.Vector MFrameT
tankFramesStopRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 3 Nothing
               , MFrameT (Just GameAI.aiRun) 3 Nothing
               , MFrameT (Just GameAI.aiRun) 2 Nothing
               , MFrameT (Just GameAI.aiRun) 2 Nothing
               , MFrameT (Just GameAI.aiRun) 4 (Just tankFootStep)
               ]

tankMoveStopRun :: MMoveT
tankMoveStopRun = MMoveT "tankMoveStopRun" frameWalk21 frameWalk25 tankFramesStopRun (Just tankWalk)

tankFramesPain1 :: V.Vector MFrameT
tankFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

tankMovePain1 :: MMoveT
tankMovePain1 = MMoveT "tankMovePain1" framePain101 framePain104 tankFramesPain1 (Just tankRun)

tankFramesPain2 :: V.Vector MFrameT
tankFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

tankMovePain2 :: MMoveT
tankMovePain2 = MMoveT "tankMovePain2" framePain201 framePain205 tankFramesPain2 (Just tankRun)

tankFramesPain3 :: V.Vector MFrameT
tankFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-7) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  (Just tankFootStep) 
               ]

tankMovePain3 :: MMoveT
tankMovePain3 = MMoveT "tankMovePain3" framePain301 framePain316 tankFramesPain3 (Just tankRun)

tankPain :: EntPain
tankPain =
  GenericEntPain "tank_pain" $ \_ _ _ _ -> do
    io (putStrLn "MTank.tankPain") >> undefined -- TODO

tankBlaster :: EntThink
tankBlaster =
  GenericEntThink "TankBlaster" $ \_ -> do
    io (putStrLn "MTank.tankBlaster") >> undefined -- TODO

tankStrike :: EntThink
tankStrike =
  GenericEntThink "TankStrike" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundStrike <- use $ mTankGlobals.mTankSoundStrike
    sound (Just selfRef) Constants.chanWeapon soundStrike 1 Constants.attnNorm 0
    return True

tankRocket :: EntThink
tankRocket =
  GenericEntThink "TankRocket" $ \_ -> do
    io (putStrLn "MTank.tankRocket") >> undefined -- TODO

tankMachineGun :: EntThink
tankMachineGun =
  GenericEntThink "TankMachineGun" $ \_ -> do
    io (putStrLn "MTank.tankMachineGun") >> undefined -- TODO

tankReAttackBlaster :: EntThink
tankReAttackBlaster =
  GenericEntThink "tank_reattack_blaster" $ \_ -> do
    io (putStrLn "MTank.tankReAttackBlaster") >> undefined -- TODO

tankFramesAttackBlast :: V.Vector MFrameT
tankFramesAttackBlast =
    V.fromList [ MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  (Just tankBlaster) -- 10
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  (Just tankBlaster)
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  (Just tankBlaster) -- 16
               ]

tankMoveAttackBlast :: MMoveT
tankMoveAttackBlast = MMoveT "tankMoveAttackBlast" frameAttack101 frameAttack116 tankFramesAttackBlast (Just tankReAttackBlaster)

tankFramesReAttackBlast :: V.Vector MFrameT
tankFramesReAttackBlast =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just tankBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just tankBlaster) -- 16
               ]

tankMoveReAttackBlast :: MMoveT
tankMoveReAttackBlast = MMoveT "tankMoveReAttackBlast" frameAttack111 frameAttack116 tankFramesReAttackBlast (Just tankReAttackBlaster)

tankFramesAttackPostBlast :: V.Vector MFrameT
tankFramesAttackPostBlast =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  Nothing -- 17
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove) (-2) (Just tankFootStep) -- 22
               ]

tankMoveAttackPostBlast :: MMoveT
tankMoveAttackPostBlast = MMoveT "tankMoveAttackPostBlast" frameAttack117 frameAttack122 tankFramesAttackPostBlast (Just tankRun)

tankPostStrike :: EntThink
tankPostStrike =
  GenericEntThink "tank_poststrike" $ \selfRef@(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eEnemy .= Nothing
    void $ think tankRun selfRef
    return True

tankDoAttackRocket :: EntThink
tankDoAttackRocket =
  GenericEntThink "tank_doattack_rocket" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just tankMoveAttackFireRocket
    return True

tankReFireRocket :: EntThink
tankReFireRocket =
  GenericEntThink "tank_refire_rocket" $ \_ -> do
    io (putStrLn "MTank.tankReFireRocket") >> undefined -- TODO

tankFramesAttackStrike :: V.Vector MFrameT
tankFramesAttackStrike =
    V.fromList [ MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    6  Nothing
               , MFrameT (Just GameAI.aiMove)    7  Nothing
               , MFrameT (Just GameAI.aiMove)    9  (Just tankFootStep)
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    2  (Just tankFootStep)
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)    0  (Just tankWindUp)
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  (Just tankStrike)
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-3) Nothing
               , MFrameT (Just GameAI.aiMove) (-10) Nothing
               , MFrameT (Just GameAI.aiMove) (-10) Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)  (-3) Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) (Just tankFootStep)
               ]

tankMoveAttackStrike :: MMoveT
tankMoveAttackStrike = MMoveT "tankMoveAttackStrike" frameAttack201 frameAttack238 tankFramesAttackStrike (Just tankPostStrike)

tankFramesAttackPreRocket :: V.Vector MFrameT
tankFramesAttackPreRocket =
    V.fromList [ MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 10
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   1  Nothing
               , MFrameT (Just GameAI.aiCharge)   2  Nothing
               , MFrameT (Just GameAI.aiCharge)   7  Nothing
               , MFrameT (Just GameAI.aiCharge)   7  Nothing
               , MFrameT (Just GameAI.aiCharge)   7  (Just tankFootStep)
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 20)
               , MFrameT (Just GameAI.aiCharge) (-3) Nothing
               ]

tankMoveAttackPreRocket :: MMoveT
tankMoveAttackPreRocket = MMoveT "tankMoveAttackPreRocket" frameAttack301 frameAttack321 tankFramesAttackPreRocket (Just tankDoAttackRocket)

tankFramesAttackFireRocket :: V.Vector MFrameT
tankFramesAttackFireRocket =
    V.fromList [ MFrameT (Just GameAI.aiCharge) (-3) Nothing -- Loop Start 22 
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  (Just tankRocket) -- 24
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  (Just tankRocket)
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) (Just tankRocket) -- 30 Loop End
               ]

tankMoveAttackFireRocket :: MMoveT
tankMoveAttackFireRocket = MMoveT "tankMoveAttackFireRocket" frameAttack322 frameAttack330 tankFramesAttackFireRocket (Just tankReFireRocket)

tankFramesAttackPostRocket :: V.Vector MFrameT
tankFramesAttackPostRocket =
    V.fromList [ MFrameT (Just GameAI.aiCharge)   0  Nothing -- 31
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   2  Nothing
               , MFrameT (Just GameAI.aiCharge)   3  Nothing
               , MFrameT (Just GameAI.aiCharge)   4  Nothing
               , MFrameT (Just GameAI.aiCharge)   2  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 40
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-9) Nothing
               , MFrameT (Just GameAI.aiCharge) (-8) Nothing
               , MFrameT (Just GameAI.aiCharge) (-7) Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) (Just tankFootStep)
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 50
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               ]

tankMoveAttackPostRocket :: MMoveT
tankMoveAttackPostRocket = MMoveT "tankMoveAttackPostRocket" frameAttack331 frameAttack353 tankFramesAttackPostRocket (Just tankRun)

tankFramesAttackChain :: V.Vector MFrameT
tankFramesAttackChain =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT                Nothing 0 (Just tankMachineGun)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

tankMoveAttackChain :: MMoveT
tankMoveAttackChain = MMoveT "tankMoveAttackChain" frameAttack401 frameAttack429 tankFramesAttackChain (Just tankRun)

tankAttack :: EntThink
tankAttack =
  GenericEntThink "tank_attack" $ \_ -> do
    io (putStrLn "MTank.tankAttack") >> undefined -- TODO

tankDead :: EntThink
tankDead =
  GenericEntThink "tank_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMins .= V3 (-16) (-16) (-16)
      eMaxs .= V3 16 16 0
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

tankFramesDeath1 :: V.Vector MFrameT
tankFramesDeath1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)  (-7) Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    6  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-3) Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-4) Nothing
               , MFrameT (Just GameAI.aiMove)  (-6) Nothing
               , MFrameT (Just GameAI.aiMove)  (-4) Nothing
               , MFrameT (Just GameAI.aiMove)  (-5) Nothing
               , MFrameT (Just GameAI.aiMove)  (-7) Nothing
               , MFrameT (Just GameAI.aiMove) (-15) (Just tankThud)
               , MFrameT (Just GameAI.aiMove)  (-5) Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               ]

tankMoveDeath :: MMoveT
tankMoveDeath = MMoveT "tankMoveDeath" frameDeath101 frameDeath132 tankFramesDeath1 (Just tankDead)

tankDie :: EntDie
tankDie =
  GenericEntDie "tank_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MTank.tankDie") >> undefined -- TODO

{-
- QUAKED monster_tank (1 .5 0) (-32 -32 -16) (32 32 72) Ambush
- Trigger_Spawn Sight
-}
{-
- QUAKED monster_tank_commander (1 .5 0) (-32 -32 -16) (32 32 72) Ambush
- Trigger_Spawn Sight
-}
spMonsterTank :: EntThink
spMonsterTank =
  GenericEntThink "SP_monster_tank" $ \_ -> do
    io (putStrLn "MTank.spMonsterTank") >> undefined -- TODO
