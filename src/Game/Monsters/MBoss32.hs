{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MBoss32 where

import Control.Lens (use, preuse, ix, (^.), (.=), (%=), zoom)
import Data.Bits ((.|.))
import Linear (V3(..))
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

frameStand201 :: Int
frameStand201 = 414

frameStand260 :: Int
frameStand260 = 473

makronTaunt :: EntThink
makronTaunt =
  GenericEntThink "makron_taunt" $ \selfRef@(EdictReference selfIdx) -> do
    r <- Lib.randomF

    soundTaunt <- if | r <= 0.3 -> use $ mBoss32Globals.mb32SoundTaunt1
                     | r <= 0.6 -> use $ mBoss32Globals.mb32SoundTaunt2
                     | otherwise -> use $ mBoss32Globals.mb32SoundTaunt3

    sound <- use $ gameBaseGlobals.gbGameImport.giSound

    sound (Just selfRef) Constants.chanAuto soundTaunt 1 Constants.attnNone 0
    return True

makronStand :: EntThink
makronStand =
  GenericEntThink "makron_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just makronMoveStand
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
  GenericEntThink "makron_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEdictMinMax.eMins .= V3 (-60) (-60) 0
      eEdictMinMax.eMaxs .= V3 60 60 72
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eEdictAction.eaNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

makronToss :: EntThink
makronToss =
  GenericEntThink "MakronToss" $ \_ -> do
    io (putStrLn "MBoss32.makronToss") >> undefined -- TODO
