{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MHover where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=))
import Data.Bits ((.&.))
import Linear (V3(..))
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

frameStand01 :: Int
frameStand01 = 0

frameStand30 :: Int
frameStand30 = 29

frameForward01 :: Int
frameForward01 = 30

frameForward35 :: Int
frameForward35 = 64

frameStop101 :: Int
frameStop101 = 65

frameStop109 :: Int
frameStop109 = 73

frameStop201 :: Int
frameStop201 = 74

frameStop208 :: Int
frameStop208 = 81

frameTakeOff01 :: Int
frameTakeOff01 = 82

frameTakeOff30 :: Int
frameTakeOff30 = 111

frameLand01 :: Int
frameLand01 = 112

framePain101 :: Int
framePain101 = 113

framePain128 :: Int
framePain128 = 140

framePain201 :: Int
framePain201 = 141

framePain212 :: Int
framePain212 = 152

framePain301 :: Int
framePain301 = 153

framePain309 :: Int
framePain309 = 161

frameDeath101 :: Int
frameDeath101 = 162

frameDeath111 :: Int
frameDeath111 = 172

frameBackward01 :: Int
frameBackward01 = 173

frameBackward24 :: Int
frameBackward24 = 196

frameAttack101 :: Int
frameAttack101 = 197

frameAttack103 :: Int
frameAttack103 = 199

frameAttack104 :: Int
frameAttack104 = 200

frameAttack106 :: Int
frameAttack106 = 202

frameAttack107 :: Int
frameAttack107 = 203

frameAttack108 :: Int
frameAttack108 = 204

hoverReAttack :: EntThink
hoverReAttack =
  GenericEntThink "hover_reattack" $ \_ -> do
    io (putStrLn "MHover.hoverReAttack") >> undefined -- TODO

hoverFireBlaster :: EntThink
hoverFireBlaster =
  GenericEntThink "hover_fire_blaster" $ \_ -> do
    io (putStrLn "MHover.hoverFireBlaster") >> undefined -- TODO

hoverStand :: EntThink
hoverStand =
  GenericEntThink "hover_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just hoverMoveStand
    return True

hoverRun :: EntThink
hoverRun =
  GenericEntThink "hover_run" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then hoverMoveStand
                   else hoverMoveRun

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

hoverWalk :: EntThink
hoverWalk =
  GenericEntThink "hover_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just hoverMoveWalk
    return True

hoverStartAttack :: EntThink
hoverStartAttack =
  GenericEntThink "hover_start_attack" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just hoverMoveStartAttack
    return True

hoverAttack :: EntThink
hoverAttack =
  GenericEntThink "hover_attack" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just hoverMoveAttack1
    return True

hoverPain :: EntPain
hoverPain =
  GenericEntPain "hover_pain" $ \_ _ _ _ -> do
    io (putStrLn "MHover.hoverPain") >> undefined -- TODO

hoverDeadThink :: EntThink
hoverDeadThink =
  GenericEntThink "hover_deadthink" $ \_ -> do
    io (putStrLn "MHover.hoverDeadThink") >> undefined -- TODO

hoverDead :: EntThink
hoverDead =
  GenericEntThink "hover_dead" $ \selfRef@(EdictReference selfIdx) -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEdictMinMax.eMins .= V3 (-16) (-16) (-24)
      eEdictMinMax.eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eThink .= Just hoverDeadThink
      eNextThink .= levelTime + Constants.frameTime
      eTimeStamp .= levelTime + 15

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

hoverDie :: EntDie
hoverDie =
  GenericEntDie "hover_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MHover.hoverDie") >> undefined -- TODO

hoverSight :: EntInteract
hoverSight =
  GenericEntInteract "hover_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mHoverGlobals.mHoverSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

hoverSearch :: EntThink
hoverSearch =
  GenericEntThink "hover_search" $ \selfRef -> do
    r <- Lib.randomF

    soundSearch <- if r < 0.5
                     then use $ mHoverGlobals.mHoverSoundSearch1
                     else use $ mHoverGlobals.mHoverSoundSearch2

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

hoverFramesStand :: V.Vector MFrameT
hoverFramesStand =
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

hoverMoveStand :: MMoveT
hoverMoveStand = MMoveT "hoverMoveStand" frameStand01 frameStand30 hoverFramesStand Nothing

hoverFramesStop1 :: V.Vector MFrameT
hoverFramesStop1 =
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

hoverMoveStop1 :: MMoveT
hoverMoveStop1 = MMoveT "hoverMoveStop1" frameStop101 frameStop109 hoverFramesStop1 Nothing

hoverFramesStop2 :: V.Vector MFrameT
hoverFramesStop2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

hoverMoveStop2 :: MMoveT
hoverMoveStop2 = MMoveT "hoverMoveStop2" frameStop201 frameStop208 hoverFramesStop2 Nothing

hoverFramesTakeOff :: V.Vector MFrameT
hoverFramesTakeOff =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove) (-6) Nothing
               , MFrameT (Just GameAI.aiMove) (-9) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

hoverMoveTakeOff :: MMoveT
hoverMoveTakeOff = MMoveT "hoverMoveTakeOff" frameTakeOff01 frameTakeOff30 hoverFramesTakeOff Nothing

hoverFramesPain3 :: V.Vector MFrameT
hoverFramesPain3 =
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

hoverMovePain3 :: MMoveT
hoverMovePain3 = MMoveT "hoverMovePain3" framePain301 framePain309 hoverFramesPain3 (Just hoverRun)

hoverFramesPain2 :: V.Vector MFrameT
hoverFramesPain2 =
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
               ]

hoverMovePain2 :: MMoveT
hoverMovePain2 = MMoveT "hoverMovePain2" framePain201 framePain212 hoverFramesPain2 (Just hoverRun)

hoverFramesPain1 :: V.Vector MFrameT
hoverFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove) (-8) Nothing
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove) (-6) Nothing
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   7  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               ]

hoverMovePain1 :: MMoveT
hoverMovePain1 = MMoveT "hoverMovePain1" framePain101 framePain128 hoverFramesPain1 (Just hoverRun)

hoverFramesLand :: V.Vector MFrameT
hoverFramesLand = V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing ]

hoverMoveLand :: MMoveT
hoverMoveLand = MMoveT "hoverMoveLand" frameLand01 frameLand01 hoverFramesLand Nothing

hoverFramesForward :: V.Vector MFrameT
hoverFramesForward =
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
               ]

hoverMoveForward :: MMoveT
hoverMoveForward = MMoveT "hoverMoveForward" frameForward01 frameForward35 hoverFramesForward Nothing

hoverFramesWalk :: V.Vector MFrameT
hoverFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               ]

hoverMoveWalk :: MMoveT
hoverMoveWalk = MMoveT "hoverMoveWalk" frameForward01 frameForward35 hoverFramesWalk Nothing

hoverFramesRun :: V.Vector MFrameT
hoverFramesRun =
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
               ]

hoverMoveRun :: MMoveT
hoverMoveRun = MMoveT "hoverMoveRun" frameForward01 frameForward35 hoverFramesRun Nothing

hoverFramesDeath1 :: V.Vector MFrameT
hoverFramesDeath1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove) (-10) Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    5  Nothing
               , MFrameT (Just GameAI.aiMove)    4  Nothing
               , MFrameT (Just GameAI.aiMove)    7  Nothing
               ]

hoverMoveDeath1 :: MMoveT
hoverMoveDeath1 = MMoveT "hoverMoveDeath1" frameDeath101 frameDeath111 hoverFramesDeath1 (Just hoverDead)

hoverFramesBackward :: V.Vector MFrameT
hoverFramesBackward =
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
               ]

hoverMoveBackward :: MMoveT
hoverMoveBackward = MMoveT "hoverMoveBackward" frameBackward01 frameBackward24 hoverFramesBackward Nothing

hoverFramesStartAttack :: V.Vector MFrameT
hoverFramesStartAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               ]

hoverMoveStartAttack :: MMoveT
hoverMoveStartAttack = MMoveT "hoverMoveStartAttack" frameAttack101 frameAttack103 hoverFramesStartAttack (Just hoverAttack)

hoverFramesAttack1 :: V.Vector MFrameT
hoverFramesAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) (-10) (Just hoverFireBlaster)
               , MFrameT (Just GameAI.aiCharge) (-10) (Just hoverFireBlaster)
               , MFrameT (Just GameAI.aiCharge)    0  (Just hoverReAttack)
               ]

hoverMoveAttack1 :: MMoveT
hoverMoveAttack1 = MMoveT "hoverMoveAttack1" frameAttack104 frameAttack106 hoverFramesAttack1 Nothing

hoverFramesEndAttack :: V.Vector MFrameT
hoverFramesEndAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 1 Nothing
               ]

hoverMoveEndAttack :: MMoveT
hoverMoveEndAttack = MMoveT "hoverMoveEndAttack" frameAttack107 frameAttack108 hoverFramesEndAttack (Just hoverRun)

{-
- QUAKED monster_hover (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterHover :: EdictReference -> Quake ()
spMonsterHover _ = do
    io (putStrLn "MHover.spMonsterHover") >> undefined -- TODO
