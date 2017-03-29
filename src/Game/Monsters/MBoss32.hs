{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MBoss32 where

import Control.Lens (use, preuse, ix, (^.), (.=), (%=), zoom, (+=), (&), (.~), (%~), (+~), (-~))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (isJust)
import Linear (V3(..), norm, normalize, _x, _y, _z)
import qualified Data.Vector as V

import Types
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameMisc as GameMisc
import qualified Game.Monster as Monster
import qualified Game.Monsters.MFlash as MFlash
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

modelScale :: Float
modelScale = 1.0

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

frameAttack405 :: Int
frameAttack405 = 213

frameAttack413 :: Int
frameAttack413 = 221

frameAttack421 :: Int
frameAttack421 = 229

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
    self <- readEdictT selfRef
    
    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2MakronBfg) forward right
        Just enemyRef = self^.eEnemy
        
    enemy <- readEdictT enemyRef
    
    let V3 a b c = enemy^.eEntityState.esOrigin
        vec = V3 a b (c + fromIntegral (enemy^.eViewHeight))
        dir = normalize (vec - start)
        
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let sound = gameImport^.giSound
    
    soundAttackBfg <- use $ mBoss32Globals.mb32SoundAttackBfg
    sound (Just selfRef) Constants.chanVoice soundAttackBfg 1 Constants.attnNorm 0
    Monster.monsterFireBFG selfRef start dir 50 300 100 300 Constants.mz2MakronBfg
    
    return True

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
    self <- readEdictT selfRef
    
    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2MakronRailgun1) forward right
        dir = normalize ((self^.ePos1) - start)
    
    Monster.monsterFireRailgun selfRef start dir 50 100 Constants.mz2MakronRailgun1
    
    return True

-- FIXME: This is all wrong. He's not firing at the proper angles.
makronHyperBlaster :: EntThink
makronHyperBlaster =
  GenericEntThink "MakronHyperblaster" $ \selfRef -> do
    self <- readEdictT selfRef
    
    let flashNumber = Constants.mz2MakronBlaster1 + (self^.eEntityState.esFrame) - frameAttack405
        (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
    
    a <- case self^.eEnemy of
           Nothing ->
             return 0
           
           Just enemyRef -> do
             enemy <- readEdictT enemyRef
             let V3 a b c = enemy^.eEntityState.esOrigin
                 vec = (V3 a b (c + fromIntegral (enemy^.eViewHeight))) - start
                 vec' = Math3D.vectorAngles vec
             return (vec'^._x)
    
    let b = if (self^.eEntityState.esFrame) <= frameAttack413
              then (self^.eEntityState.esAngles._y) - 10 * fromIntegral ((self^.eEntityState.esFrame) - frameAttack413)
              else (self^.eEntityState.esAngles._y) + 10 * fromIntegral ((self^.eEntityState.esFrame) - frameAttack421)
              
        dir = V3 a b 0
        (Just forward', _, _) = Math3D.angleVectors dir True False False
    
    Monster.monsterFireBlaster selfRef start forward' 15 1000 Constants.mz2MakronBlaster1 Constants.efBlaster
    
    return True

makronPain :: EntPain
makronPain =
  GenericEntPain "makron_pain" $ \selfRef _ _ damage -> do
    self <- readEdictT selfRef
    
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyEdictT selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    --Lessen the chance of him going into his pain frames
    skipPainFrames <- if damage <= 25
                        then do
                          r <- Lib.randomF
                          return (r < 0.2)
                        else
                          return False
    
    unless (levelTime < (self^.ePainDebounceTime) || skipPainFrames) $ do
      modifyEdictT selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
      skillValue <- liftM (^.cvValue) skillCVar
      
      -- no pain anims in nightmare
      unless (skillValue == 3) $ do
        soundMove <- if | damage <= 40 -> do
                            soundPain <- use $ mBoss32Globals.mb32SoundPain4
                            return $ Just (soundPain, makronMovePain4)
                            
                        | damage <= 110 -> do
                            soundPain <- use $ mBoss32Globals.mb32SoundPain5
                            return $ Just (soundPain, makronMovePain5)
                            
                        | damage <= 150 -> do
                            r <- Lib.randomF
                            
                            if r <= 0.45
                              then do
                                soundPain <- use $ mBoss32Globals.mb32SoundPain6
                                return $ Just (soundPain, makronMovePain6)
                              else
                                return Nothing
                                
                            
                        | otherwise -> do
                            r <- Lib.randomF
                            
                            if r <= 0.35
                              then do
                                soundPain <- use $ mBoss32Globals.mb32SoundPain6
                                return $ Just (soundPain, makronMovePain6)
                              else
                                return Nothing
                                
        case soundMove of
          Nothing ->
            return ()
            
          Just (soundPain, currentMove) -> do
            sound <- use $ gameBaseGlobals.gbGameImport.giSound
            sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNone 0
            modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

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
  GenericEntDie "makron_die" $ \selfRef _ _ damage _ -> do
    modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ 0)
    
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex
    
    if | (self^.eHealth) <= (self^.eGibHealth) -> do
           soundIdx <- soundIndex (Just "misc/udeath.wav")
           sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
           
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           
           GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
           
           GameMisc.throwHead selfRef "models/objects/gibs/gear/tris.md2" damage Constants.gibMetallic
           
           modifyEdictT selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        
       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()
           
       | otherwise -> do
           -- regular death
           soundDeath <- use $ mBoss32Globals.mb32SoundDeath
           sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0
           
           modifyEdictT selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         )
            
           tempEntRef <- GameUtil.spawn
           
           modifyEdictT tempEntRef (\v -> v & eEntityState.esOrigin .~ (self^.eEntityState.esOrigin)
                                            & eEntityState.esAngles .~ (self^.eEntityState.esAngles)
                                            & eEntityState.esOrigin._y -~ 84
                                            )
                                            
           void $ think makronTorso tempEntRef
           
           modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just makronMoveDeath2)

makronCheckAttack :: EntThink
makronCheckAttack =
  GenericEntThink "Makron_CheckAttack" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef
    
    done <- if (enemy^.eHealth) > 0
              then do
                let spot1 = (self^.eEntityState.esOrigin) & _z +~ fromIntegral (self^.eViewHeight)
                    spot2 = (enemy^.eEntityState.esOrigin) & _z +~ fromIntegral (enemy^.eViewHeight)
                
                trace <- use $ gameBaseGlobals.gbGameImport.giTrace
                traceT <- trace spot1 Nothing Nothing spot2 (Just selfRef) (Constants.contentsSolid .|. Constants.contentsMonster .|. Constants.contentsSlime .|. Constants.contentsLava)
                
                -- do we have a clear shot?
                if (traceT^.tEnt) == (self^.eEnemy)
                  then return False
                  else return True
              
              else
                return False
    
    if done
      then
        return False
      
      else do
        let enemyRange = GameUtil.range self enemy
            temp = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
            enemyYaw = Math3D.vectorYaw temp
        
        modifyEdictT selfRef (\v -> v & eIdealYaw .~ enemyYaw)
        
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        
             -- melee attack
        if | enemyRange == Constants.rangeMelee -> do
               let attackState = case self^.eMonsterInfo.miMelee of
                                   Nothing -> Constants.asMissile
                                   Just _ -> Constants.asMelee
               modifyEdictT selfRef (\v -> v & eMonsterInfo.miAttackState .~ attackState)
               return True
                
             -- missile attack
           | isJust (self^.eMonsterInfo.miAttack) ->
               return False
          
           | levelTime < (self^.eMonsterInfo.miAttackFinished) ->
               return False
          
           | enemyRange == Constants.rangeFar ->
               return False
               
           | otherwise -> do
               let mChance = if | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 -> Just 0.4
                                | enemyRange == Constants.rangeMelee -> Just 0.8
                                | enemyRange == Constants.rangeNear -> Just 0.4
                                | enemyRange == Constants.rangeMid -> Just 0.2
                                | otherwise -> Nothing
                
               case mChance of
                 Nothing ->
                   return False
                
                 Just chance -> do
                   r <- Lib.randomF
                   
                   if r < chance
                     then do
                       r' <- Lib.randomF
                       modifyEdictT selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asMissile
                                                     & eMonsterInfo.miAttackFinished .~ levelTime + 2 * r'
                                                     )
                       return True
                       
                     else do
                       when ((self^.eFlags) .&. Constants.flFly /= 0) $ do
                         r' <- Lib.randomF
                         let attackState = if r' < 0.3
                                             then Constants.asSliding
                                             else Constants.asStraight
                         modifyEdictT selfRef (\v -> v & eMonsterInfo.miAttackState .~ attackState)
                      
                       return False

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
  GenericEntThink "MakronSpawn" $ \selfRef -> do
    spMonsterMakron selfRef
    
    -- jump at player
    sightClient <- use $ gameBaseGlobals.gbLevel.llSightClient
    
    case sightClient of
      Nothing ->
        return True
        
      Just playerRef -> do
        self <- readEdictT selfRef
        player <- readEdictT playerRef
        v3o <- use $ globals.vec3Origin
        
        let vec = (player^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
            vec' = normalize vec
        
        modifyEdictT selfRef (\v -> v & eEntityState.esAngles._y .~ Math3D.vectorYaw vec -- IMPROVE: use Constants.yaw instead of using _y directly
                                      & eVelocity .~ ((v3o + fmap (* 400) vec') & _z .~ 200)
                                      & eGroundEntity .~ Nothing
                                      )
        
        return True

makronToss :: EntThink
makronToss =
  GenericEntThink "MakronToss" $ \selfRef -> do
    self <- readEdictT selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    entRef <- GameUtil.spawn
    
    modifyEdictT entRef (\v -> v & eNextThink .~ levelTime + 0.8
                                 & eThink .~ Just makronSpawn
                                 & eTarget .~ (self^.eTarget)
                                 & eEntityState.esOrigin .~ (self^.eEntityState.esOrigin)
                                 )
    
    return True

makronPrecache :: Quake ()
makronPrecache = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        
    soundIndex (Just "makron/pain3.wav") >>= (mBoss32Globals.mb32SoundPain4 .=)
    soundIndex (Just "makron/pain2.wav") >>= (mBoss32Globals.mb32SoundPain5 .=)
    soundIndex (Just "makron/pain1.wav") >>= (mBoss32Globals.mb32SoundPain6 .=)
    soundIndex (Just "makron/death.wav") >>= (mBoss32Globals.mb32SoundDeath .=)
    soundIndex (Just "makron/step1.wav") >>= (mBoss32Globals.mb32SoundStepLeft .=)
    soundIndex (Just "makron/step2.wav") >>= (mBoss32Globals.mb32SoundStepRight .=)
    soundIndex (Just "makron/bfg_fire.wav") >>= (mBoss32Globals.mb32SoundAttackBfg .=)
    soundIndex (Just "makron/brain1.wav") >>= (mBoss32Globals.mb32SoundBrainSplorch .=)
    soundIndex (Just "makron/rail_up.wav") >>= (mBoss32Globals.mb32SoundPreRailGun .=)
    soundIndex (Just "makron/popup.wav") >>= (mBoss32Globals.mb32SoundPopUp .=)
    soundIndex (Just "makron/voice4.wav") >>= (mBoss32Globals.mb32SoundTaunt1 .=)
    soundIndex (Just "makron/voice3.wav") >>= (mBoss32Globals.mb32SoundTaunt2 .=)
    soundIndex (Just "makron/voice.wav") >>= (mBoss32Globals.mb32SoundTaunt3 .=)
    soundIndex (Just "makron/bhit.wav") >>= (mBoss32Globals.mb32SoundHit .=)
    
    void $ modelIndex (Just "models/monsters/boss3/rider/tris.md2")

{-
- QUAKED monster_makron (1 .5 0) (-30 -30 0) (30 30 90) Ambush
- Trigger_Spawn Sight
-}
spMonsterMakron :: EdictReference -> Quake ()
spMonsterMakron selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    
    if deathmatchValue /= 0
      then do
        GameUtil.freeEdict selfRef
        
      else do
        makronPrecache
        
        gameImport <- use $ gameBaseGlobals.gbGameImport
        
        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity
        
        modelIdx <- modelIndex (Just "models/monsters/boss3/rider/tris.md2")
        
        modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-30) (-30) 0
                                      & eMaxs .~ V3 30 30 90
                                      & eHealth .~ 3000
                                      & eGibHealth .~ (-2000)
                                      & eMass .~ 500
                                      & ePain .~ Just makronPain
                                      & eDie .~ Just makronDie
                                      & eMonsterInfo.miStand .~ Just makronStand
                                      & eMonsterInfo.miWalk .~ Just makronWalk
                                      & eMonsterInfo.miRun .~ Just makronRun
                                      & eMonsterInfo.miDodge .~ Nothing
                                      & eMonsterInfo.miAttack .~ Just makronAttack
                                      & eMonsterInfo.miMelee .~ Nothing
                                      & eMonsterInfo.miSight .~ Just makronSight
                                      & eMonsterInfo.miCheckAttack .~ Just makronCheckAttack
                                      )
        
        linkEntity selfRef
        
        modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just makronMoveSight
                                      & eMonsterInfo.miScale .~ modelScale
                                      )
        
        void $ think GameAI.walkMonsterStart selfRef