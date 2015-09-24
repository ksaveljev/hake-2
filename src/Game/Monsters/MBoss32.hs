{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MBoss32 where

import Control.Lens (use, preuse, ix, (^.), (.=), (%=), zoom, (+=), (&), (.~), (%~), (+~))
import Data.Bits ((.|.), (.&.))
import Linear (V3(..), norm)
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

frameActive01 :: Int
frameActive01 = 188

frameActive13 :: Int
frameActive13 = 200

frameAttack301 :: Int
frameAttack301 = 201

frameAttack308 :: Int
frameAttack308 = 208

frameAttack401 :: Int
frameAttack401 = 209

frameAttack426 :: Int
frameAttack426 = 234

frameAttack501 :: Int
frameAttack501 = 235

frameAttack516 :: Int
frameAttack516 = 250

frameDeath201 :: Int
frameDeath201 = 251

frameDeath295 :: Int
frameDeath295 = 345

frameDeath301 :: Int
frameDeath301 = 346

frameDeath320 :: Int
frameDeath320 = 365

framePain401 :: Int
framePain401 = 379

framePain404 :: Int
framePain404 = 382

framePain501 :: Int
framePain501 = 383

framePain504 :: Int
framePain504 = 386

framePain601 :: Int
framePain601 = 387

framePain627 :: Int
framePain627 = 413

frameStand201 :: Int
frameStand201 = 414

frameStand260 :: Int
frameStand260 = 473

frameWalk204 :: Int
frameWalk204 = 477

frameWalk213 :: Int
frameWalk213 = 486

makronTaunt :: EntThink
makronTaunt =
  GenericEntThink "makron_taunt" $ \selfRef -> do
    r <- Lib.randomF

    soundTaunt <- if | r <= 0.3 -> use $ mBoss32Globals.mb32SoundTaunt1
                     | r <= 0.6 -> use $ mBoss32Globals.mb32SoundTaunt2
                     | otherwise -> use $ mBoss32Globals.mb32SoundTaunt3

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanAuto soundTaunt 1 Constants.attnNone 0

    return True

makronStand :: EntThink
makronStand =
  GenericEntThink "makron_stand" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just makronMoveStand)
    return True

makronHit :: EntThink
makronHit =
  GenericEntThink "makron_hit" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundHit <- use $ mBoss32Globals.mb32SoundHit
    sound (Just selfRef) Constants.chanAuto soundHit 1 Constants.attnNone 0

    return True

makronPopUp :: EntThink
makronPopUp =
  GenericEntThink "makron_popup" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundPopUp <- use $ mBoss32Globals.mb32SoundPopUp
    sound (Just selfRef) Constants.chanBody soundPopUp 1 Constants.attnNone 0

    return True

makronStepLeft :: EntThink
makronStepLeft =
  GenericEntThink "makron_step_left" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundStepLeft <- use $ mBoss32Globals.mb32SoundStepLeft
    sound (Just selfRef) Constants.chanBody soundStepLeft 1 Constants.attnNorm 0

    return True

makronStepRight :: EntThink
makronStepRight =
  GenericEntThink "makron_step_right" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundStepRight <- use $ mBoss32Globals.mb32SoundStepRight
    sound (Just selfRef) Constants.chanBody soundStepRight 1 Constants.attnNorm 0

    return True

makronBrainSplorch :: EntThink
makronBrainSplorch =
  GenericEntThink "makron_brainsplorch" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundBrainSplorch <- use $ mBoss32Globals.mb32SoundBrainSplorch
    sound (Just selfRef) Constants.chanVoice soundBrainSplorch 1 Constants.attnNorm 0

    return True

makronPreRailGun :: EntThink
makronPreRailGun =
  GenericEntThink "makron_prerailgun" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundPreRailGun <- use $ mBoss32Globals.mb32SoundPreRailGun
    sound (Just selfRef) Constants.chanWeapon soundPreRailGun 1 Constants.attnNorm 0

    return True

makronFramesStand :: V.Vector MFrameT
makronFramesStand =
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
                 -- 40
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
                 -- 50
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing -- 60
               ]

makronMoveStand :: MMoveT
makronMoveStand = MMoveT "makronMoveStand" frameStand201 frameStand260 makronFramesStand Nothing

makronFramesRun :: V.Vector MFrameT
makronFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)  3 (Just makronStepLeft)
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun)  8 Nothing
               , MFrameT (Just GameAI.aiRun)  8 Nothing
               , MFrameT (Just GameAI.aiRun)  8 (Just makronStepRight)
               , MFrameT (Just GameAI.aiRun)  6 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               , MFrameT (Just GameAI.aiRun)  6 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               ]

makronMoveRun :: MMoveT
makronMoveRun = MMoveT "makronMoveRun" frameWalk204 frameWalk213 makronFramesRun Nothing

makronFramesWalk :: V.Vector MFrameT
makronFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  3 (Just makronStepLeft)
               , MFrameT (Just GameAI.aiWalk) 12 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 (Just makronStepRight)
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk) 12 Nothing
               , MFrameT (Just GameAI.aiWalk)  9 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk) 12 Nothing
               ]

makronMoveWalk :: MMoveT
makronMoveWalk = MMoveT "makronMoveWalk" frameWalk204 frameWalk213 makronFramesRun Nothing

makronDead :: EntThink
makronDead =
  GenericEntThink "makron_dead" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMins .~ V3 (-60) (-60) 0
                                  & eMaxs .~ V3 60 60 72
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

makronWalk :: EntThink
makronWalk =
  GenericEntThink "makron_walk" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just makronMoveWalk)
    return True

makronRun :: EntThink
makronRun =
  GenericEntThink "makron_run" $ \selfRef -> do
    self <- readEdictT selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then makronMoveStand
                   else makronMoveRun

    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

makronFramesPain6 :: V.Vector MFrameT
makronFramesPain6 =
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
               , MFrameT (Just GameAI.aiMove) 0 (Just makronPopUp)
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
                 -- 20
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 (Just makronTaunt)
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

makronMovePain6 :: MMoveT
makronMovePain6 = MMoveT "makronMovePain6" framePain601 framePain627 makronFramesPain6 (Just makronRun)

makronFramesPain5 :: V.Vector MFrameT
makronFramesPain5 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

makronMovePain5 :: MMoveT
makronMovePain5 = MMoveT "makronMovePain5" framePain501 framePain504 makronFramesPain5 (Just makronRun)

makronFramesPain4 :: V.Vector MFrameT
makronFramesPain4 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

makronMovePain4 :: MMoveT
makronMovePain4 = MMoveT "makronMovePain4" framePain401 framePain404 makronFramesPain4 (Just makronRun)

makronFramesDeath2 :: V.Vector MFrameT
makronFramesDeath2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-15) Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove) (-12) Nothing
               , MFrameT (Just GameAI.aiMove)    0  (Just makronStepLeft)
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
                 -- 10
               , MFrameT (Just GameAI.aiMove)  0 Nothing
               , MFrameT (Just GameAI.aiMove)  0 Nothing
               , MFrameT (Just GameAI.aiMove)  0 Nothing
               , MFrameT (Just GameAI.aiMove)  0 Nothing
               , MFrameT (Just GameAI.aiMove)  0 Nothing
               , MFrameT (Just GameAI.aiMove) 11 Nothing
               , MFrameT (Just GameAI.aiMove) 12 Nothing
               , MFrameT (Just GameAI.aiMove) 11 (Just makronStepRight)
               , MFrameT (Just GameAI.aiMove)  0 Nothing
               , MFrameT (Just GameAI.aiMove)  0 Nothing
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
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               , MFrameT (Just GameAI.aiMove)   7  Nothing
               , MFrameT (Just GameAI.aiMove)   6  (Just makronStepLeft)
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
                 -- 40
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
                 -- 50
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-6) Nothing
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove) (-6) (Just makronStepRight)
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove) (-4) (Just makronStepLeft)
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
                 -- 60
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove) (-3) (Just makronStepRight)
               , MFrameT (Just GameAI.aiMove) (-8) Nothing
               , MFrameT (Just GameAI.aiMove) (-3) (Just makronStepLeft)
               , MFrameT (Just GameAI.aiMove) (-7) Nothing
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove) (-4) (Just makronStepRight)
                 -- 70
               , MFrameT (Just GameAI.aiMove) (-6) Nothing
               , MFrameT (Just GameAI.aiMove) (-7) Nothing
               , MFrameT (Just GameAI.aiMove)   0  (Just makronStepLeft)
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
                 -- 80
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
                 -- 90
               , MFrameT (Just GameAI.aiMove) 27 (Just makronHit)
               , MFrameT (Just GameAI.aiMove) 26 Nothing
               , MFrameT (Just GameAI.aiMove)  0 (Just makronBrainSplorch)
               , MFrameT (Just GameAI.aiMove)  0 Nothing
               , MFrameT (Just GameAI.aiMove)  0 Nothing -- 95
               ]

makronMoveDeath2 :: MMoveT
makronMoveDeath2 = MMoveT "makronMoveDeath2" frameDeath201 frameDeath295 makronFramesDeath2 (Just makronDead)

makronFramesDeath3 :: V.Vector MFrameT
makronFramesDeath3 =
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
               ]

makronMoveDeath3 :: MMoveT
makronMoveDeath3 = MMoveT "makronMoveDeath3" frameDeath301 frameDeath320 makronFramesDeath3 Nothing

makronFramesSight :: V.Vector MFrameT
makronFramesSight =
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
               ]

makronMoveSight :: MMoveT
makronMoveSight = MMoveT "makronMoveSight" frameActive01 frameActive13 makronFramesSight (Just makronRun)

makronBFG :: EntThink
makronBFG =
  GenericEntThink "makronBFG" $ \selfRef -> do
    io (putStrLn "MBoss32.makronBFG") >> undefined -- TODO

makronSaveLoc :: EntThink
makronSaveLoc =
  GenericEntThink "MakronSaveloc" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef

    let V3 a b c = (enemy^.eEntityState.esOrigin)

    -- save for aiming the shot
    modifyEdictT selfRef (\v -> v & ePos1 .~ V3 a b (c + fromIntegral (enemy^.eViewHeight)))
    return True

-- FIXME: He's not firing from the proper Z
makronRailGun :: EntThink
makronRailGun =
  GenericEntThink "MakronRailgun" $ \selfRef -> do
    io (putStrLn "MBoss32.makronRailGun") >> undefined -- TODO

-- FIXME: This is all wrong. He's not firing at the proper angles.
makronHyperBlaster :: EntThink
makronHyperBlaster =
  GenericEntThink "MakronHyperblaster" $ \selfRef -> do
    io (putStrLn "MBoss32.makronHyperBlaster") >> undefined -- TODO

makronPain :: EntPain
makronPain =
  GenericEntPain "makron_pain" $ \_ _ _ _ -> do
    io (putStrLn "MBoss32.makronPain") >> undefined -- TODO

makronSight :: EntInteract
makronSight =
  GenericEntInteract "makron_sight" $ \selfRef _ -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just makronMoveSight)
    return True

makronAttack :: EntThink
makronAttack =
  GenericEntThink "makron_attack" $ \selfRef -> do
    r <- Lib.randomF

    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef

    let vec = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        range = norm vec

    let action = if | r <= 0.3 -> makronMoveAttack3
                    | r <= 0.6 -> makronMoveAttack4
                    | otherwise -> makronMoveAttack5

    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

-- Makron Torso. This needs to be spawned in
makronTorsoThink :: EntThink
makronTorsoThink =
  GenericEntThink "makron_torso_think" $ \selfRef -> do
    self <- readEdictT selfRef

    if (self^.eEntityState.esFrame) < 365
      then modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1)
      else modifyEdictT selfRef (\v -> v & eEntityState.esFrame .~ 346)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)

    return True

makronTorso :: EntThink
makronTorso =
  GenericEntThink "makron_torso" $ \edictRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modelIdx <- modelIndex (Just "models/monsters/boss3/rider/tris.md2")
    soundIdx <- soundIndex (Just "makron/spine.wav")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidNot
                                   & eMins .~ V3 (-8) (-8) 0
                                   & eMaxs .~ V3 8 8 8
                                   & eEntityState.esFrame .~ 346
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eThink .~ Just makronTorsoThink
                                   & eNextThink .~ levelTime + 2 * Constants.frameTime
                                   & eEntityState.esSound .~ soundIdx)

    linkEntity edictRef

    return True

makronDie :: EntDie
makronDie =
  GenericEntDie "makron_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MBoss32.makronDie") >> undefined -- TODO

makronCheckAttack :: EntThink
makronCheckAttack =
  GenericEntThink "Makron_CheckAttack" $ \_ -> do
    io (putStrLn "MBoss32.makronCheckAttack") >> undefined -- TODO

makronFramesAttack3 :: V.Vector MFrameT
makronFramesAttack3 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just makronBFG)
                 -- FIXME: BFG Attack here
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

makronMoveAttack3 :: MMoveT
makronMoveAttack3 = MMoveT "makronMoveAttack3" frameAttack301 frameAttack308 makronFramesAttack3 (Just makronRun)

makronFramesAttack4 :: V.Vector MFrameT
makronFramesAttack4 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 (Just makronHyperBlaster) -- fire
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               ]

makronMoveAttack4 :: MMoveT
makronMoveAttack4 = MMoveT "makronMoveAttack4" frameAttack401 frameAttack426 makronFramesAttack4 (Just makronRun)

makronFramesAttack5 :: V.Vector MFrameT
makronFramesAttack5 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just makronPreRailGun)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just makronSaveLoc)
               , MFrameT (Just GameAI.aiMove)   0 (Just makronRailGun) -- Fire railgun
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               ]

makronMoveAttack5 :: MMoveT
makronMoveAttack5 = MMoveT "makronMoveAttack5" frameAttack501 frameAttack516 makronFramesAttack5 (Just makronRun)

makronSpawn :: EntThink
makronSpawn =
  GenericEntThink "MakronSpawn" $ \_ -> do
    io (putStrLn "MBoss32.makronSpawn") >> undefined -- TODO

makronToss :: EntThink
makronToss =
  GenericEntThink "MakronToss" $ \_ -> do
    io (putStrLn "MBoss32.makronToss") >> undefined -- TODO

makronPrecache :: Quake ()
makronPrecache = do
    io (putStrLn "MBoss32.makronPrecache") >> undefined -- TODO

{-
- QUAKED monster_makron (1 .5 0) (-30 -30 0) (30 30 90) Ambush
- Trigger_Spawn Sight
-}
spMonsterMakron :: Quake ()
spMonsterMakron = do
    io (putStrLn "MBoss32.spMonsterMakron") >> undefined -- TODO
