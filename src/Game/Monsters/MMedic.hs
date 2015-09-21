{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MMedic where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=), (+=), (-=))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing)
import Linear (V3(..), _z)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameUtil as GameUtil
import qualified Game.Monster as Monster
import qualified Game.Monsters.MFlash as MFlash
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

frameWalk1 :: Int
frameWalk1 = 0

frameWalk12 :: Int
frameWalk12 = 11

frameWait1 :: Int
frameWait1 = 12

frameWait90 :: Int
frameWait90 = 101

frameRun1 :: Int
frameRun1 = 102

frameRun6 :: Int
frameRun6 = 107

framePainA1 :: Int
framePainA1 = 108

framePainA8 :: Int
framePainA8 = 115

framePainB1 :: Int
framePainB1 = 116

framePainB15 :: Int
framePainB15 = 130

frameDuck1 :: Int
frameDuck1 = 131

frameDuck16 :: Int
frameDuck16 = 146

frameDeath1 :: Int
frameDeath1 = 147

frameDeath30 :: Int
frameDeath30 = 176

frameAttack1 :: Int
frameAttack1 = 177

frameAttack9 :: Int
frameAttack9 = 185

frameAttack12 :: Int
frameAttack12 = 188

frameAttack14 :: Int
frameAttack14 = 190

frameAttack15 :: Int
frameAttack15 = 191

frameAttack19 :: Int
frameAttack19 = 195

frameAttack22 :: Int
frameAttack22 = 198

frameAttack25 :: Int
frameAttack25 = 201

frameAttack28 :: Int
frameAttack28 = 204

frameAttack30 :: Int
frameAttack30 = 206

frameAttack33 :: Int
frameAttack33 = 209

frameAttack60 :: Int
frameAttack60 = 236

medicFindDeadMonster :: EdictReference -> Quake (Maybe EdictReference)
medicFindDeadMonster selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    findDeadMonster (self^.eEntityState.esOrigin) Nothing Nothing

  where findDeadMonster :: V3 Float -> Maybe EdictReference -> Maybe EdictReference -> Quake (Maybe EdictReference)
        findDeadMonster origin currentRef bestRef = do
          foundRef <- GameBase.findRadius currentRef origin 1024

          case foundRef of
            Nothing ->
              return bestRef

            Just edictRef@(EdictReference edictIdx) -> do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

              if edictRef == selfRef || (edict^.eSvFlags) .&. Constants.svfMonster == 0 || (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy /= 0 || isNothing (edict^.eOwner) || (edict^.eHealth) > 0 || (edict^.eNextThink) == 0
                then
                  findDeadMonster origin (Just edictRef) bestRef
                else do
                  vis <- GameUtil.visible selfRef edictRef

                  if | not vis -> findDeadMonster origin (Just edictRef) bestRef
                     | isNothing bestRef -> findDeadMonster origin (Just edictRef) (Just edictRef)
                     | otherwise -> do
                         let Just (EdictReference bestIdx) = bestRef
                         Just best <- preuse $ gameBaseGlobals.gbGEdicts.ix bestIdx

                         if (edict^.eMaxHealth) <= (best^.eMaxHealth)
                           then findDeadMonster origin (Just edictRef) bestRef
                           else findDeadMonster origin (Just edictRef) (Just edictRef)

medicIdle :: EntThink
medicIdle =
  GenericEntThink "medic_idle" $ \selfRef@(EdictReference selfIdx) -> do
    soundIdle1 <- use $ mMedicGlobals.mMedicSoundIdle1
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundIdle1 1 Constants.attnIdle 0

    deadMonster <- medicFindDeadMonster selfRef

    case deadMonster of
      Nothing ->
        return True

      Just (EdictReference deadMonsterIdx) -> do
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eEnemy .= deadMonster
          eMonsterInfo.miAIFlags %= (.|. Constants.aiMedic)

        gameBaseGlobals.gbGEdicts.ix deadMonsterIdx.eOwner .= Just selfRef

        GameUtil.foundTarget selfRef
        return True

medicSearch :: EntThink
medicSearch =
  GenericEntThink "medic_search" $ \selfRef@(EdictReference selfIdx) -> do
    soundSearch <- use $ mMedicGlobals.mMedicSoundSearch
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnIdle 0

    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    case self^.eOldEnemy of
      Nothing -> do
        deadMonster <- medicFindDeadMonster selfRef

        case deadMonster of
          Nothing ->
            return True

          Just (EdictReference deadMonsterIdx) -> do
            zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
              eOldEnemy .= (self^.eEnemy)
              eEnemy .= deadMonster
              eMonsterInfo.miAIFlags %= (.|. Constants.aiMedic)

            gameBaseGlobals.gbGEdicts.ix deadMonsterIdx.eOwner .= Just selfRef

            GameUtil.foundTarget selfRef
            return True

      Just _ ->
        return True

medicSight :: EntInteract
medicSight =
  GenericEntInteract "medic_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mMedicGlobals.mMedicSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

medicFramesStand :: V.Vector MFrameT
medicFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 (Just medicIdle)
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
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

medicMoveStand :: MMoveT
medicMoveStand = MMoveT "medicMoveStand" frameWait1 frameWait90 medicFramesStand Nothing

medicStand :: EntThink
medicStand =
  GenericEntThink "medic_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just medicMoveStand
    return True

medicFramesWalk :: V.Vector MFrameT
medicFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  6.2 Nothing
               , MFrameT (Just GameAI.aiWalk) 18.1 Nothing
               , MFrameT (Just GameAI.aiWalk)    1 Nothing
               , MFrameT (Just GameAI.aiWalk)    9 Nothing
               , MFrameT (Just GameAI.aiWalk)   10 Nothing
               , MFrameT (Just GameAI.aiWalk)    9 Nothing
               , MFrameT (Just GameAI.aiWalk)   11 Nothing
               , MFrameT (Just GameAI.aiWalk) 11.6 Nothing
               , MFrameT (Just GameAI.aiWalk)    2 Nothing
               , MFrameT (Just GameAI.aiWalk)  9.9 Nothing
               , MFrameT (Just GameAI.aiWalk)   14 Nothing
               , MFrameT (Just GameAI.aiWalk)  9.3 Nothing
               ]

medicMoveWalk :: MMoveT
medicMoveWalk = MMoveT "medicMoveWalk" frameWalk1 frameWalk12 medicFramesWalk Nothing

medicWalk :: EntThink
medicWalk =
  GenericEntThink "medic_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just medicMoveWalk
    return True

medicFramesRun :: V.Vector MFrameT
medicFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)   18 Nothing
               , MFrameT (Just GameAI.aiRun) 22.5 Nothing
               , MFrameT (Just GameAI.aiRun) 25.4 Nothing
               , MFrameT (Just GameAI.aiRun) 23.4 Nothing
               , MFrameT (Just GameAI.aiRun)   24 Nothing
               , MFrameT (Just GameAI.aiRun) 35.6 Nothing
               ]

medicMoveRun :: MMoveT
medicMoveRun = MMoveT "medicMoveRun" frameRun1 frameRun6 medicFramesRun Nothing

medicRun :: EntThink
medicRun =
  GenericEntThink "medic_run" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    done <- if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiMedic == 0
              then do
                deadMonster <- medicFindDeadMonster selfRef

                case deadMonster of
                  Nothing ->
                    return False

                  Just (EdictReference deadMonsterIdx) -> do
                    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
                      eOldEnemy .= (self^.eEnemy)
                      eEnemy .= deadMonster
                      eMonsterInfo.miAIFlags %= (.|. Constants.aiMedic)

                    gameBaseGlobals.gbGEdicts.ix deadMonsterIdx.eOwner .= Just selfRef
                    GameUtil.foundTarget selfRef
                    return True

              else
                return False

    unless done $ do
      let currentMove = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                          then medicMoveStand
                          else medicMoveRun

      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove

    return True

medicFramesPain1 :: V.Vector MFrameT
medicFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

medicMovePain1 :: MMoveT
medicMovePain1 = MMoveT "medicMovePain1" framePainA1 framePainA8 medicFramesPain1 (Just medicRun)

medicFramesPain2 :: V.Vector MFrameT
medicFramesPain2 =
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
               ]

medicMovePain2 :: MMoveT
medicMovePain2 = MMoveT "medicMovePain2" framePainB1 framePainB15 medicFramesPain2 (Just medicRun)

medicPain :: EntPain
medicPain =
  GenericEntPain "medic_pain" $ \selfRef@(EdictReference selfIdx) _ _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSkinNum .= 1

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      gameBaseGlobals.gbGEdicts.ix selfIdx.ePainDebounceTime .= levelTime + 3

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        r <- Lib.randomF

        (soundPain, currentMove) <- if r < 0.5
                                      then do
                                        soundPain <- use $ mMedicGlobals.mMedicSoundPain1
                                        return (soundPain, medicMovePain1)
                                      else do
                                        soundPain <- use $ mMedicGlobals.mMedicSoundPain2
                                        return (soundPain, medicMovePain2)

        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove

        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0

medicFireBlaster :: EntThink
medicFireBlaster =
  GenericEntThink "medic_fire_blaster" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let frame = self^.eEntityState.esFrame
        effect = if | frame `elem` [ frameAttack9, frameAttack12 ] -> Constants.efBlaster
                    | frame `elem` [ frameAttack19, frameAttack22, frameAttack25, frameAttack28 ] -> Constants.efHyperblaster
                    | otherwise -> 0
        (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2MedicBlaster1) forward right
        Just (EdictReference enemyIdx) = self^.eEnemy

    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    let V3 a b c = enemy^.eEntityState.esOrigin
        end = V3 a b (c + fromIntegral (enemy^.eViewHeight))
        dir = end - start

    Monster.monsterFireBlaster selfRef start dir 2 1000 Constants.mz2MedicBlaster1 effect
    return True

medicDead :: EntThink
medicDead =
  GenericEntThink "medic_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMins .= V3 (-16) (-16) (-24)
      eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

medicFramesDeath :: V.Vector MFrameT
medicFramesDeath =
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
               ]

medicMoveDeath :: MMoveT
medicMoveDeath = MMoveT "medicMoveDeath" frameDeath1 frameDeath30 medicFramesDeath (Just medicDead)

medicDie :: EntDie
medicDie =
  GenericEntDie "medic_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MMedic.medicDie") >> undefined -- TODO

medicDuckDown :: EntThink
medicDuckDown =
  GenericEntThink "medic_duck_down" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked /= 0
      then return True
      else do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eMonsterInfo.miAIFlags %= (.|. Constants.aiDucked)
          eMaxs._z -= 32
          eTakeDamage .= Constants.damageYes
          eMonsterInfo.miPauseTime .= levelTime + 1

        linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
        linkEntity selfRef

        return True

medicDuckHold :: EntThink
medicDuckHold =
  GenericEntThink "medic_duck_hold" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if levelTime >= (self^.eMonsterInfo.miPauseTime)
      then gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiHoldFrame))
      else gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiHoldFrame)

    return True

medicDuckUp :: EntThink
medicDuckUp =
  GenericEntThink "medic_duck_up" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiDucked))
      eMaxs._z += 32
      eTakeDamage .= Constants.damageAim

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

medicFramesDuck :: V.Vector MFrameT
medicFramesDuck =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) (Just medicDuckDown)
               , MFrameT (Just GameAI.aiMove) (-1) (Just medicDuckHold)
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) (Just medicDuckUp)
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               ]

medicMoveDuck :: MMoveT
medicMoveDuck = MMoveT "medicMoveDuck" frameDuck1 frameDuck16 medicFramesDuck (Just medicRun)

medicDodge :: EntDodge
medicDodge =
  GenericEntDodge "medic_dodge" $ \_ _ _ -> do
    io (putStrLn "MMedic.medicDodge") >> undefined -- TODO

medicFramesAttackHyperBlaster :: V.Vector MFrameT
medicFramesAttackHyperBlaster =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               ]

medicMoveAttackHyperBlaster :: MMoveT
medicMoveAttackHyperBlaster = MMoveT "medicMoveAttackHyperBlaster" frameAttack15 frameAttack30 medicFramesAttackHyperBlaster (Just medicRun)

medicContinue :: EntThink
medicContinue =
  GenericEntThink "medic_continue" $ \_ -> do
    io (putStrLn "MMedic.medicContinue") >> undefined -- TODO

medicFramesAttackBlaster :: V.Vector MFrameT
medicFramesAttackBlaster =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 5 Nothing
               , MFrameT (Just GameAI.aiCharge) 5 Nothing
               , MFrameT (Just GameAI.aiCharge) 3 Nothing
               , MFrameT (Just GameAI.aiCharge) 2 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just medicContinue) -- Change to medic_continue... Else, go to frame 32
               ]

medicMoveAttackBlaster :: MMoveT
medicMoveAttackBlaster = MMoveT "medicMoveAttackBlaster" frameAttack1 frameAttack14 medicFramesAttackBlaster (Just medicRun)

medicHookLaunch :: EntThink
medicHookLaunch =
  GenericEntThink "medic_hook_launch" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundHookLaunch <- use $ mMedicGlobals.mMedicSoundHookLaunch
    sound (Just selfRef) Constants.chanWeapon soundHookLaunch 1 Constants.attnNorm 0
    return True

medicCableOffset :: V.Vector (V3 Float)
medicCableOffset =
    V.fromList [ V3 45.0  (-9.2) 15.5
               , V3 48.4  (-9.7) 15.2
               , V3 47.8  (-9.8) 15.8
               , V3 47.3  (-9.3) 14.3
               , V3 45.4 (-10.1) 13.1
               , V3 41.9 (-12.7) 12.0
               , V3 37.8 (-15.8) 11.2
               , V3 34.3 (-18.4) 10.7
               , V3 32.7 (-19.7) 10.4
               , V3 32.7 (-19.7) 10.4
               ]

medicCableAttack :: EntThink
medicCableAttack =
  GenericEntThink "medic_cable_attack" $ \_ -> do
    io (putStrLn "MMedic.medicCableAttack") >> undefined -- TODO

medicHookRetract :: EntThink
medicHookRetract =
  GenericEntThink "medic_hook_retract" $ \selfRef@(EdictReference selfIdx) -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundHookRetract <- use $ mMedicGlobals.mMedicSoundHookRetract
    sound (Just selfRef) Constants.chanWeapon soundHookRetract 1 Constants.attnNorm 0
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiResurrecting))
    return True

medicFramesAttackCable :: V.Vector MFrameT
medicFramesAttackCable =
    V.fromList [ MFrameT (Just GameAI.aiMove)     2  Nothing
               , MFrameT (Just GameAI.aiMove)     3  Nothing
               , MFrameT (Just GameAI.aiMove)     5  Nothing
               , MFrameT (Just GameAI.aiMove)   4.4  Nothing
               , MFrameT (Just GameAI.aiCharge) 4.7  Nothing
               , MFrameT (Just GameAI.aiCharge)   5  Nothing
               , MFrameT (Just GameAI.aiCharge)   6  Nothing
               , MFrameT (Just GameAI.aiCharge)   4  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  (Just medicHookLaunch)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)     0  (Just medicCableAttack)
               , MFrameT (Just GameAI.aiMove)  (-15) (Just medicHookRetract)
               , MFrameT (Just GameAI.aiMove) (-1.5) Nothing
               , MFrameT (Just GameAI.aiMove) (-1.2) Nothing
               , MFrameT (Just GameAI.aiMove)   (-3) Nothing
               , MFrameT (Just GameAI.aiMove)   (-2) Nothing
               , MFrameT (Just GameAI.aiMove)   0.3  Nothing
               , MFrameT (Just GameAI.aiMove)   0.7  Nothing
               , MFrameT (Just GameAI.aiMove)   1.2  Nothing
               , MFrameT (Just GameAI.aiMove)   1.3  Nothing
               ]

medicMoveAttackCable :: MMoveT
medicMoveAttackCable = MMoveT "medicMoveAttackCable" frameAttack33 frameAttack60 medicFramesAttackCable (Just medicRun)

medicAttack :: EntThink
medicAttack =
  GenericEntThink "medic_attack" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiMedic /= 0
                   then medicMoveAttackCable
                   else medicMoveAttackBlaster

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

medicCheckAttack :: EntThink
medicCheckAttack =
  GenericEntThink "medic_checkattack" $ \_ -> do
    io (putStrLn "MMedic.medicCheckAttack") >> undefined -- TODO


{-
- QUAKED monster_medic (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterMedic :: EdictReference -> Quake ()
spMonsterMedic _ = do
    io (putStrLn "MMedic.spMonsterMedic") >> undefined -- TODO
