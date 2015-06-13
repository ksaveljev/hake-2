{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MBrain where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (-=), (%=), (+=))
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing)
import Linear (_z)
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

brainSight :: EntInteract
brainSight =
  GenericEntInteract "brain_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mBrainGlobals.mBrainSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

brainSearch :: EntThink
brainSearch =
  GenericEntThink "brain_search" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSearch <- use $ mBrainGlobals.mBrainSoundSearch
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

brainFramesStand :: V.Vector MFrameT
brainFramesStand =
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

brainMoveStand :: MMoveT
brainMoveStand = MMoveT "brainMoveStand" frameStand01 frameStand30 brainFramesStand Nothing

brainStand :: EntThink
brainStand =
  GenericEntThink "brain_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just brainMoveStand
    return True

brainFramesIdle :: V.Vector MFrameT
brainFramesIdle =
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

brainMoveIdle :: MMoveT
brainMoveIdle = MMoveT "brainMoveIdle" frameStand31 frameStand60 brainFramesIdle (Just brainStand)

brainIdle :: EntThink
brainIdle =
  GenericEntThink "brain_idle" $ \selfRef@(EdictReference selfIdx) -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle3 <- use $ mBrainGlobals.mBrainSoundIdle3

    sound (Just selfRef) Constants.chanAuto soundIdle3 1 Constants.attnIdle 0
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just brainMoveIdle
    return True

brainFramesWalk1 :: V.Vector MFrameT
brainFramesWalk1 =
    V.fromList [ MFrameT (Just GameAI.aiWalk)   7  Nothing
               , MFrameT (Just GameAI.aiWalk)   2  Nothing
               , MFrameT (Just GameAI.aiWalk)   3  Nothing
               , MFrameT (Just GameAI.aiWalk)   3  Nothing
               , MFrameT (Just GameAI.aiWalk)   1  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   9  Nothing
               , MFrameT (Just GameAI.aiWalk) (-4) Nothing
               , MFrameT (Just GameAI.aiWalk) (-1) Nothing
               , MFrameT (Just GameAI.aiWalk)   2  Nothing
               ]

brainMoveWalk1 :: MMoveT
brainMoveWalk1 = MMoveT "brainMoveWalk1" frameWalk101 frameWalk111 brainFramesWalk1 Nothing

-- walk2 is FUBAR, do not use
{-
- # if 0 void brain_walk2_cycle(edict_t self) { if (random() > 0.1)
- self.monsterinfo.nextframe= FRAME_walk220; }
- 
- static mframe_t brain_frames_walk2[]= new mframe_t[] { new
- mframe_t(ai_walk, 3, null), new mframe_t(ai_walk, -2, null), new
- mframe_t(ai_walk, -4, null), new mframe_t(ai_walk, -3, null), new
- mframe_t(ai_walk, 0, null), new mframe_t(ai_walk, 1, null), new
- mframe_t(ai_walk, 12, null), new mframe_t(ai_walk, 0, null), new
- mframe_t(ai_walk, -3, null), new mframe_t(ai_walk, 0, null), new
- mframe_t(ai_walk, -2, null), new mframe_t(ai_walk, 0, null), new
- mframe_t(ai_walk, 0, null), new mframe_t(ai_walk, 1, null), new
- mframe_t(ai_walk, 0, null), new mframe_t(ai_walk, 0, null), new
- mframe_t(ai_walk, 0, null), new mframe_t(ai_walk, 0, null), new
- mframe_t(ai_walk, 0, null), new mframe_t(ai_walk, 10, null, // Cycle
- Start)
- 
- new mframe_t(ai_walk, -1, null), new mframe_t(ai_walk, 7, null), new
- mframe_t(ai_walk, 0, null), new mframe_t(ai_walk, 3, null), new
- mframe_t(ai_walk, -3, null), new mframe_t(ai_walk, 2, null), new
- mframe_t(ai_walk, 4, null), new mframe_t(ai_walk, -3, null), new
- mframe_t(ai_walk, 2, null), new mframe_t(ai_walk, 0, null), new
- mframe_t(ai_walk, 4, brain_walk2_cycle), new mframe_t(ai_walk, -1, null),
- new mframe_t(ai_walk, -1, null), new mframe_t(ai_walk, -8, null,) new
- mframe_t(ai_walk, 0, null), new mframe_t(ai_walk, 1, null), new
- mframe_t(ai_walk, 5, null), new mframe_t(ai_walk, 2, null), new
- mframe_t(ai_walk, -1, null), new mframe_t(ai_walk, -5, null)}; static
- mmove_t brain_move_walk2= new mmove_t(FRAME_walk201, FRAME_walk240,
- brain_frames_walk2, null);
-  # endif
-}

brainWalk :: EntThink
brainWalk =
  GenericEntThink "brain_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just brainMoveWalk1
    return True

brainDuckDown :: EntThink
brainDuckDown =
  GenericEntThink "brain_duck_down" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked /= 0
      then return True
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eMonsterInfo.miAIFlags %= (.|. Constants.aiDucked)
          eEdictMinMax.eMaxs._z -= 32
          eTakeDamage .= Constants.damageYes

        linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
        linkEntity selfRef

        return True

brainDuckHold :: EntThink
brainDuckHold =
  GenericEntThink "brain_duck_hold" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if levelTime >= (self^.eMonsterInfo.miPauseTime)
      then gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiHoldFrame))
      else gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiHoldFrame)

    return True

brainDuckUp :: EntThink
brainDuckUp =
  GenericEntThink "brain_duck_up" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiDucked))
      eEdictMinMax.eMaxs._z += 32
      eTakeDamage .= Constants.damageAim

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

brainDodge :: EntDodge
brainDodge =
  GenericEntDodge "brain_dodge" $ \(EdictReference selfIdx) attackerRef eta -> do
    r <- Lib.randomF

    unless (r > 0.25) $ do
      Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

      when (isNothing (self^.eEdictOther.eoEnemy)) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoEnemy .= attackerRef

      levelTime <- use $ gameBaseGlobals.gbLevel.llTime

      zoom (gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo) $ do
        miPauseTime .= levelTime + eta + 0.5
        miCurrentMove .= Just brainMoveDuck

spMonsterBrain :: EdictReference -> Quake ()
spMonsterBrain _ = io (putStrLn "MBrain.spMonsterBrain") >> undefined -- TODO
