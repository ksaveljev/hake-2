{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MFlipper where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=))
import Data.Bits ((.|.))
import Linear (V3(..))
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

frameFlpbit01 :: Int
frameFlpbit01 = 0

frameFlpbit20 :: Int
frameFlpbit20 = 19

frameFlphor01 :: Int
frameFlphor01 = 41

frameFlphor05 :: Int
frameFlphor05 = 45

frameFlphor24 :: Int
frameFlphor24 = 64

frameFlpver01 :: Int
frameFlpver01 = 65

frameFlpver06 :: Int
frameFlpver06 = 70

frameFlpver29 :: Int
frameFlpver29 = 93

frameFlppn101 :: Int
frameFlppn101 = 94

frameFlppn105 :: Int
frameFlppn105 = 98

frameFlppn201 :: Int
frameFlppn201 = 99

frameFlppn205 :: Int
frameFlppn205 = 103

frameFlpdth01 :: Int
frameFlpdth01 = 104

frameFlpdth56 :: Int
frameFlpdth56 = 159

flipperRunSpeed :: Float
flipperRunSpeed = 24

flipperFramesStand :: V.Vector MFrameT
flipperFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing ]

flipperMoveStand :: MMoveT
flipperMoveStand = MMoveT "flipperMoveStand" frameFlphor01 frameFlphor01 flipperFramesStand Nothing

flipperStand :: EntThink
flipperStand =
  GenericEntThink "flipper_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flipperMoveStand
    return True

flipperFramesRun :: V.Vector MFrameT
flipperFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing -- 6
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
                 -- 10

               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
                 -- 20

               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing -- 29
               ]

flipperMoveRunLoop :: MMoveT
flipperMoveRunLoop = MMoveT "flipperMoveRunLoop" frameFlpver06 frameFlpver29 flipperFramesRun Nothing

flipperRunLoop :: EntThink
flipperRunLoop =
  GenericEntThink "flipper_run_loop" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flipperMoveRunLoop
    return True

flipperFramesRunStart :: V.Vector MFrameT
flipperFramesRunStart =
    V.fromList [ MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               ]

flipperMoveRunStart :: MMoveT
flipperMoveRunStart = MMoveT "flipperMoveRunStart" frameFlpver01 frameFlpver06 flipperFramesRunStart (Just flipperRunLoop)

flipperRun :: EntThink
flipperRun =
  GenericEntThink "flipper_run" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flipperMoveRunStart
    return True

-- Standard Swimming
flipperFramesWalk :: V.Vector MFrameT
flipperFramesWalk =
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
               ]

flipperMoveWalk :: MMoveT
flipperMoveWalk = MMoveT "flipperMoveWalk" frameFlphor01 frameFlphor24 flipperFramesWalk Nothing

flipperWalk :: EntThink
flipperWalk =
  GenericEntThink "flipper_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flipperMoveWalk
    return True

flipperFramesStartRun :: V.Vector MFrameT
flipperFramesStartRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 (Just flipperRun)
               ]

flipperMoveStartRun :: MMoveT
flipperMoveStartRun = MMoveT "flipperMoveStartRun" frameFlphor01 frameFlphor05 flipperFramesStartRun Nothing

flipperStartRun :: EntThink
flipperStartRun =
  GenericEntThink "flipper_start_run" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flipperMoveStartRun
    return True

flipperFramesPain2 :: V.Vector MFrameT
flipperFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flipperMovePain2 :: MMoveT
flipperMovePain2 = MMoveT "flipperMovePain2" frameFlppn101 frameFlppn105 flipperFramesPain2 (Just flipperRun)

flipperFramesPain1 :: V.Vector MFrameT
flipperFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flipperMovePain1 :: MMoveT
flipperMovePain1 = MMoveT "flipperMovePain1" frameFlppn201 frameFlppn205 flipperFramesPain1 (Just flipperRun)

flipperBite :: EntThink
flipperBite =
  GenericEntThink "flipper_bite" $ \_ -> do
    io (putStrLn "MFlipper.flipperBite") >> undefined -- TODO

flipperPreAttack :: EntThink
flipperPreAttack =
  GenericEntThink "flipper_preattack" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundChomp <- use $ mFlipperGlobals.mFlipperSoundChomp
    sound (Just selfRef) Constants.chanWeapon soundChomp 1 Constants.attnNorm 0
    return True

flipperFramesAttack :: V.Vector MFrameT
flipperFramesAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just flipperPreAttack)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just flipperBite)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just flipperBite)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

flipperMoveAttack :: MMoveT
flipperMoveAttack = MMoveT "flipperMoveAttack" frameFlpbit01 frameFlpbit20 flipperFramesAttack (Just flipperRun)

flipperMelee :: EntThink
flipperMelee =
  GenericEntThink "flipper_melee" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just flipperMoveAttack
    return True

flipperPain :: EntPain
flipperPain =
  GenericEntPain "flipper_pain" $ \_ _ _ _ -> do
    io (putStrLn "MFlipper.flipperPain") >> undefined -- TODO

flipperDead :: EntThink
flipperDead =
  GenericEntThink "flipper_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEdictMinMax.eMins .= V3 (-16) (-16) (-24)
      eEdictMinMax.eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

flipperFramesDeath :: V.Vector MFrameT
flipperFramesDeath =
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
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flipperMoveDeath :: MMoveT
flipperMoveDeath = MMoveT "flipperMoveDeath" frameFlpdth01 frameFlpdth56 flipperFramesDeath (Just flipperDead)

flipperSight :: EntInteract
flipperSight =
  GenericEntInteract "flipper_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mFlipperGlobals.mFlipperSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

flipperDie :: EntDie
flipperDie =
  GenericEntDie "flipper_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MFlipper.flipperDie") >> undefined -- TODO

{-
- QUAKED monster_flipper (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterFlipper :: EdictReference -> Quake ()
spMonsterFlipper _ = do
    io (putStrLn "MFlipper.spMonsterFlipper") >> undefined -- TODO
