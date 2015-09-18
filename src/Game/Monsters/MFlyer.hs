{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MFlyer where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=))
import Data.Bits ((.&.))
import Linear (V3(..), _x)
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameWeapon as GameWeapon
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

actionAttack1 :: Int
actionAttack1 = 1

actionAttack2 :: Int
actionAttack2 = 2

actionRun :: Int
actionRun = 3

frameStart01 :: Int
frameStart01 = 0

frameStart06 :: Int
frameStart06 = 5

frameStop01 :: Int
frameStop01 = 6

frameStop07 :: Int
frameStop07 = 12

frameStand01 :: Int
frameStand01 = 13

frameStand45 :: Int
frameStand45 = 57

frameAttack101 :: Int
frameAttack101 = 58

frameAttack106 :: Int
frameAttack106 = 63

frameAttack107 :: Int
frameAttack107 = 64

frameAttack118 :: Int
frameAttack118 = 75

frameAttack119 :: Int
frameAttack119 = 76

frameAttack121 :: Int
frameAttack121 = 78

frameAttack201 :: Int
frameAttack201 = 79

frameAttack217 :: Int
frameAttack217 = 95

frameBankLeft01 :: Int
frameBankLeft01 = 96

frameBankLeft07 :: Int
frameBankLeft07 = 102

frameBankRight01 :: Int
frameBankRight01 = 103

frameBankRight07 :: Int
frameBankRight07 = 109

frameRollLeft01 :: Int
frameRollLeft01 = 110

frameRollLeft09 :: Int
frameRollLeft09 = 118

farmeRollRight01 :: Int
farmeRollRight01 = 119

frameRollRight09 :: Int
frameRollRight09 = 127

frameDefense01 :: Int
frameDefense01 = 128

frameDefense06 :: Int
frameDefense06 = 133

framePain101 :: Int
framePain101 = 134

framePain109 :: Int
framePain109 = 142

framePain201 :: Int
framePain201 = 143

framePain204 :: Int
framePain204 = 146

framePain301 :: Int
framePain301 = 147

framePain304 :: Int
framePain304 = 150

flyerSight :: EntInteract
flyerSight =
  GenericEntInteract "flyer_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mFlyerGlobals.mFlyerSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

flyerIdle :: EntThink
flyerIdle =
  GenericEntThink "flyer_idle" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mFlyerGlobals.mFlyerSoundIdle
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

flyerPopBlades :: EntThink
flyerPopBlades =
  GenericEntThink "flyer_pop_blades" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSproing <- use $ mFlyerGlobals.mFlyerSoundSproing
    sound (Just selfRef) Constants.chanVoice soundSproing 1 Constants.attnNorm 0
    return True

flyerFramesStand :: V.Vector MFrameT
flyerFramesStand =
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
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

flyerMoveStand :: MMoveT
flyerMoveStand = MMoveT "flyerMoveStand" frameStand01 frameStand45 flyerFramesStand Nothing

flyerFramesWalk :: V.Vector MFrameT
flyerFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               ]

flyerMoveWalk :: MMoveT
flyerMoveWalk = MMoveT "flyerMoveWalk" frameStand01 frameStand45 flyerFramesWalk Nothing

flyerFramesRun :: V.Vector MFrameT
flyerFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               ]

flyerMoveRun :: MMoveT
flyerMoveRun = MMoveT "flyerMoveRun" frameStand01 frameStand45 flyerFramesRun Nothing

flyerRun :: EntThink
flyerRun =
  GenericEntThink "flyer_run" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then flyerMoveStand
                   else flyerMoveRun

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

flyerWalk :: EntThink
flyerWalk =
  GenericEntThink "flyer_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveWalk
    return True

flyerStand :: EntThink
flyerStand =
  GenericEntThink "flyer_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveStand
    return True

flyerNextMove :: EntThink
flyerNextMove =
  GenericEntThink "flyer_nextmove" $ \(EdictReference selfIdx) -> do
    nextMove <- use $ mFlyerGlobals.mFlyerNextMove

    if | nextMove == actionAttack1 ->
           gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveStartMelee
       | nextMove == actionAttack2 ->
           gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveAttack2
       | nextMove == actionRun ->
           gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveRun
       | otherwise ->
           return ()

    return True

flyerFramesStart :: V.Vector MFrameT
flyerFramesStart =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 (Just flyerNextMove)
               ]

flyerMoveStart :: MMoveT
flyerMoveStart = MMoveT "flyerMoveStart" frameStart01 frameStart06 flyerFramesStart Nothing

flyerFramesStop :: V.Vector MFrameT
flyerFramesStop =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 (Just flyerNextMove)
               ]

flyerMoveStop :: MMoveT
flyerMoveStop = MMoveT "flyerMoveStop" frameStop01 frameStop07 flyerFramesStop Nothing

flyerStop :: EntThink
flyerStop =
  GenericEntThink "flyer_stop" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveStop
    return True

flyerStart :: EntThink
flyerStart =
  GenericEntThink "flyer_start" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveStart
    return True

flyerFramesRollRight :: V.Vector MFrameT
flyerFramesRollRight =
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

flyerMoveRollRight :: MMoveT
flyerMoveRollRight = MMoveT "flyerMoveRollRight" farmeRollRight01 frameRollRight09 flyerFramesRollRight Nothing

flyerFramesRollLeft :: V.Vector MFrameT
flyerFramesRollLeft =
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

flyerMoveRollLeft :: MMoveT
flyerMoveRollLeft = MMoveT "flyerMoveRollLeft" frameRollLeft01 frameRollLeft09 flyerFramesRollLeft Nothing

flyerFramesPain3 :: V.Vector MFrameT
flyerFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flyerMovePain3 :: MMoveT
flyerMovePain3 = MMoveT "flyerMovePain3" framePain301 framePain304 flyerFramesPain3 (Just flyerRun)

flyerFramesPain2 :: V.Vector MFrameT
flyerFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flyerMovePain2 :: MMoveT
flyerMovePain2 = MMoveT "flyerMovePain2" framePain201 framePain204 flyerFramesPain2 (Just flyerRun)

flyerFramesPain1 :: V.Vector MFrameT
flyerFramesPain1 =
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

flyerMovePain1 :: MMoveT
flyerMovePain1 = MMoveT "flyerMovePain1" framePain101 framePain109 flyerFramesPain1 (Just flyerRun)

flyerFramesDefense :: V.Vector MFrameT
flyerFramesDefense =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
                 -- Hold this frame
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flyerMoveDefense :: MMoveT
flyerMoveDefense = MMoveT "flyerMoveDefense" frameDefense01 frameDefense06 flyerFramesDefense Nothing

flyerFramesBankRight :: V.Vector MFrameT
flyerFramesBankRight =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flyerMoveBankRight :: MMoveT
flyerMoveBankRight = MMoveT "flyerMoveBankRight" frameBankRight01 frameBankRight07 flyerFramesBankRight Nothing

flyerFramesBankLeft :: V.Vector MFrameT
flyerFramesBankLeft =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flyerMoveBankLeft :: MMoveT
flyerMoveBankLeft = MMoveT "flyerMoveBankLeft" frameBankLeft01 frameBankLeft07 flyerFramesBankLeft Nothing

flyerFireLeft :: EntThink
flyerFireLeft =
  GenericEntThink "flyer_fireleft" $ \selfRef -> do
    flyerFire selfRef Constants.mz2FlyerBlaster1
    return True

flyerFireRight :: EntThink
flyerFireRight =
  GenericEntThink "flyer_fireright" $ \selfRef -> do
    flyerFire selfRef Constants.mz2FlyerBlaster2
    return True

flyerFramesAttack2 :: V.Vector MFrameT
flyerFramesAttack2 =
    V.fromList [ MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireLeft)  -- left gun
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireRight) -- right gun
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireLeft)  -- left gun
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireRight) -- right gun
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireLeft)  -- left gun
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireRight) -- right gun
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireLeft)  -- left gun
               , MFrameT (Just GameAI.aiCharge) (-10) (Just flyerFireRight) -- right gun
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               ]

flyerMoveAttack2 :: MMoveT
flyerMoveAttack2 = MMoveT "flyerMoveAttack2" frameAttack201 frameAttack217 flyerFramesAttack2 (Just flyerRun)

flyerSlashLeft :: EntThink
flyerSlashLeft =
  GenericEntThink "flyer_slash_left" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) 0

    GameWeapon.fireHit selfRef aim 5 0
  
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSlash <- use $ mFlyerGlobals.mFlyerSoundSlash
    sound (Just selfRef) Constants.chanWeapon soundSlash 1 Constants.attnNorm 0
    
    return True

flyerSlashRight :: EntThink
flyerSlashRight =
  GenericEntThink "flyer_slash_right" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMaxs._x) 0

    GameWeapon.fireHit selfRef aim 5 0

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSlash <- use $ mFlyerGlobals.mFlyerSoundSlash
    sound (Just selfRef) Constants.chanWeapon soundSlash 1 Constants.attnNorm 0
    
    return True

flyerLoopMelee :: EntThink
flyerLoopMelee =
  GenericEntThink "flyer_loop_melee" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveLoopMelee
    return True

flyerFramesStartMelee :: V.Vector MFrameT
flyerFramesStartMelee =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just flyerPopBlades)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

flyerMoveStartMelee :: MMoveT
flyerMoveStartMelee = MMoveT "flyerMoveStartMelee" frameAttack101 frameAttack106 flyerFramesStartMelee (Just flyerLoopMelee)

flyerFramesEndMelee :: V.Vector MFrameT
flyerFramesEndMelee =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

flyerMoveEndMelee :: MMoveT
flyerMoveEndMelee = MMoveT "flyerMoveEndMelee" frameAttack119 frameAttack121 flyerFramesEndMelee (Just flyerRun)

flyerFramesLoopMelee :: V.Vector MFrameT
flyerFramesLoopMelee =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing -- Loop Start
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just flyerSlashLeft) -- Left Wing Strike
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just flyerSlashRight) -- Right Wing Strike
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing -- Loop Ends
               ]

flyerCheckMelee :: EntThink
flyerCheckMelee =
  GenericEntThink "flyer_check_melee" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let Just (EdictReference enemyIdx) = self^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    r <- Lib.randomF

    let currentMove = if GameUtil.range self enemy == Constants.rangeMelee
                        then if r <= 0.8
                               then flyerMoveLoopMelee
                               else flyerMoveEndMelee
                        else flyerMoveEndMelee

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove
    return True

flyerMoveLoopMelee :: MMoveT
flyerMoveLoopMelee = MMoveT "flyerMoveLoopMelee" frameAttack107 frameAttack118 flyerFramesLoopMelee (Just flyerCheckMelee)

flyerAttack :: EntThink
flyerAttack =
  GenericEntThink "flyer_attack" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveAttack2
    return True

flyerSetStart :: EntThink
flyerSetStart =
  GenericEntThink "flyer_setstart" $ \(EdictReference selfIdx) -> do
    mFlyerGlobals.mFlyerNextMove .= actionRun
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveStart
    return True

flyerMelee :: EntThink
flyerMelee =
  GenericEntThink "flyer_melee" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flyerMoveStartMelee
    return True

flyerPain :: EntPain
flyerPain =
  GenericEntPain "flyer_pain" $ \_ _ _ _ -> do
    io (putStrLn "MFlyer.flyerPain") >> undefined -- TODO

flyerDie :: EntDie
flyerDie =
  GenericEntDie "flyer_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MFlyer.flyerDie") >> undefined -- TODO

flyerFire :: EdictReference -> Int -> Quake ()
flyerFire _ _ = do
    io (putStrLn "MFlyer.flyerFire") >> undefined -- TODO

{-
- QUAKED monster_flyer (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterFlyer :: EdictReference -> Quake ()
spMonsterFlyer _ = do
    io (putStrLn "MFlyer.spMonsterFlyer") >> undefined -- TODO
