{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MInfantry where

import Control.Lens ((^.), use, (.=), ix, zoom, preuse)
import Control.Monad (liftM, void, when, unless)
import Data.Bits ((.&.))
import Linear (V3(..))
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import Game.MFrameT
import Game.MMoveT
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

modelScale :: Float
modelScale = 1

frameStand01 :: Int
frameStand01 = 1

frameStand49 :: Int
frameStand49 = 49

frameStand50 :: Int
frameStand50 = 50

frameStand71 :: Int
frameStand71 = 71

frameWalk03 :: Int
frameWalk03 = 74

frameWalk14 :: Int
frameWalk14 = 85

frameRun01 :: Int
frameRun01 = 92

frameRun08 :: Int
frameRun08 = 99

framePain101 :: Int
framePain101 = 100

framePain110 :: Int
framePain110 = 109

framePain201 :: Int
framePain201 = 110

framePain210 :: Int
framePain210 = 119

infantryFramesStand :: V.Vector MFrameT
infantryFramesStand =
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
               ]

infantryMoveStand :: MMoveT
infantryMoveStand = MMoveT "infantryMoveStand" frameStand50 frameStand71 infantryFramesStand Nothing

infantryStand :: EntThink
infantryStand =
  GenericEntThink "infantry_stand" $ \(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just infantryMoveStand
    return True

infantryFramesFidget :: V.Vector MFrameT
infantryFramesFidget =
    V.fromList [ MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   3  Nothing
               , MFrameT (Just GameAI.aiStand)   6  Nothing
               , MFrameT (Just GameAI.aiStand)   3  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand) (-1) Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand) (-2) Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand) (-1) Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand) (-1) Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand) (-1) Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   1  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand) (-1) Nothing
               , MFrameT (Just GameAI.aiStand) (-1) Nothing
               , MFrameT (Just GameAI.aiStand)   0  Nothing
               , MFrameT (Just GameAI.aiStand) (-3) Nothing
               , MFrameT (Just GameAI.aiStand) (-2) Nothing
               , MFrameT (Just GameAI.aiStand) (-3) Nothing
               , MFrameT (Just GameAI.aiStand) (-3) Nothing
               , MFrameT (Just GameAI.aiStand) (-2) Nothing
               ]

infantryMoveFidget :: MMoveT
infantryMoveFidget = MMoveT "infantryMoveFidget" frameStand01 frameStand49 infantryFramesFidget (Just infantryStand)

infantryFidget :: EntThink
infantryFidget =
  GenericEntThink "infantry_fidget" $ \er@(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just infantryMoveFidget
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mInfantryGlobals.miSoundIdle
    sound er Constants.chanVoice soundIdle 1 (fromIntegral Constants.attnIdle) 0
    return True

infantryFramesWalk :: V.Vector MFrameT
infantryFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 6 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               ]

infantryMoveWalk :: MMoveT
infantryMoveWalk = MMoveT "infantryMoveWalk" frameWalk03 frameWalk14 infantryFramesWalk Nothing

infantryWalk :: EntThink
infantryWalk =
  GenericEntThink "infantry_walk" $ \(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just infantryMoveWalk
    return True

infantryFramesRun :: V.Vector MFrameT
infantryFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 20 Nothing
               , MFrameT (Just GameAI.aiRun)  5 Nothing
               , MFrameT (Just GameAI.aiRun)  7 Nothing
               , MFrameT (Just GameAI.aiRun) 30 Nothing
               , MFrameT (Just GameAI.aiRun) 35 Nothing
               , MFrameT (Just GameAI.aiRun)  2 Nothing
               , MFrameT (Just GameAI.aiRun)  6 Nothing
               ]

infantryMoveRun :: MMoveT
infantryMoveRun = MMoveT "infantryMoveRun" frameRun01 frameRun08 infantryFramesRun Nothing

infantryRun :: EntThink
infantryRun =
  GenericEntThink "infantry_run" $ \(EdictReference edictIdx) -> do
    Just aiFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miAIFlags

    let nextMove = if aiFlags .&. Constants.aiStandGround /= 0
                     then infantryMoveStand
                     else infantryMoveRun

    gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just nextMove
    return True

infantryFramesPain1 :: V.Vector MFrameT
infantryFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   6  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               ]

infantryMovePain1 :: MMoveT
infantryMovePain1 = MMoveT "infantryMovePain1" framePain101 framePain110 infantryFramesPain1 (Just infantryRun)

infantryFramesPain2 :: V.Vector MFrameT
infantryFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               ]

infantryMovePain2 :: MMoveT
infantryMovePain2 = MMoveT "infantryMovePain2" framePain201 framePain210 infantryFramesPain2 (Just infantryRun)

infantryPain :: EntPain
infantryPain =
  GenericEntPain "infantry_pain" $ \er@(EdictReference edictIdx) _ _ _ -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    time <- use $ gameBaseGlobals.gbLevel.llTime

    when ((edict^.eEdictStatus.eHealth) < (edict^.eEdictStatus.eMaxHealth) `div` 2) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSkinNum .= 1

    unless (time < (edict^.eEdictTiming.etPainDebounceTime)) $ do
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictTiming.etPainDebounceTime .= time + 3

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        n <- (liftM (`mod` 2) Lib.rand)
        sound <- use $ gameBaseGlobals.gbGameImport.giSound

        if n == 0
          then do
            gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just infantryMovePain1
            soundPain1 <- use $ mInfantryGlobals.miSoundPain1
            sound er Constants.chanVoice soundPain1 1 (fromIntegral Constants.attnNorm) 0
          else do
            gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just infantryMovePain2
            soundPain2 <- use $ mInfantryGlobals.miSoundPain2
            sound er Constants.chanVoice soundPain2 1 (fromIntegral Constants.attnNorm) 0

infantrySight :: EntInteract
infantrySight =
  GenericEntInteract "infantry_sight" $ \er _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mInfantryGlobals.miSoundSight
    sound er Constants.chanBody soundSight 1 (fromIntegral Constants.attnNorm) 0
    return True

infantryDie :: EntDie
infantryDie =
  GenericEntDie "infantry_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MInfantry.infantryDie") >> undefined -- TODO

infantryDodge :: EntDodge
infantryDodge =
  GenericEntDodge "infantry_dodge" $ \_ _ _ -> do
    io (putStrLn "MInfantry.infantryDodge") >> undefined -- TODO

infantryAttack :: EntThink
infantryAttack =
  GenericEntThink "infantry_attack" $ \_ -> do
    io (putStrLn "MInfantry.infantryAttack") >> undefined -- TODO

{-
- QUAKED monster_infantry (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterInfantry :: EdictReference -> Quake ()
spMonsterInfantry er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport
        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex "infantry/infpain1.wav" >>= (mInfantryGlobals.miSoundPain1 .=)
        soundIndex "infantry/infpain2.wav" >>= (mInfantryGlobals.miSoundPain2 .=)
        soundIndex "infantry/infdeth1.wav" >>= (mInfantryGlobals.miSoundDie1 .=)
        soundIndex "infantry/infdeth2.wav" >>= (mInfantryGlobals.miSoundDie2 .=)

        soundIndex "infantry/infatck1.wav" >>= (mInfantryGlobals.miSoundGunShot .=)
        soundIndex "infantry/infatck3.wav" >>= (mInfantryGlobals.miSoundWeaponCock .=)
        soundIndex "infantry/infatck2.wav" >>= (mInfantryGlobals.miSoundPunchSwing .=)
        soundIndex "infantry/melee2.wav" >>= (mInfantryGlobals.miSoundPunchHit .=)

        soundIndex "infantry/infsght1.wav" >>= (mInfantryGlobals.miSoundSight .=)
        soundIndex "infantry/infsrch1.wav" >>= (mInfantryGlobals.miSoundSearch .=)
        soundIndex "infantry/infidle1.wav" >>= (mInfantryGlobals.miSoundIdle .=)

        tris <- modelIndex "models/monsters/infantry/tris.md2"

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eMoveType                 .= Constants.moveTypeStep
          eSolid                    .= Constants.solidBbox
          eEntityState.esModelIndex .= tris
          eEdictMinMax.eMins        .= V3 (-16) (-16) (-24)
          eEdictMinMax.eMaxs        .= V3 16 16 32

          eEdictStatus.eHealth    .= 100
          eEdictStatus.eGibHealth .= -40
          eEdictPhysics.eMass     .= 200

          eEdictAction.eaPain .= Just infantryPain
          eEdictAction.eaDie  .= Just infantryDie

          eMonsterInfo.miStand  .= Just infantryStand
          eMonsterInfo.miWalk   .= Just infantryWalk
          eMonsterInfo.miRun    .= Just infantryRun
          eMonsterInfo.miDodge  .= Just infantryDodge
          eMonsterInfo.miAttack .= Just infantryAttack
          eMonsterInfo.miMelee  .= Nothing
          eMonsterInfo.miSight  .= Just infantrySight
          eMonsterInfo.miIdle   .= Just infantryFidget

        linkEntity er

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo) $ do
          miCurrentMove .= Just infantryMoveStand
          miScale       .= modelScale

        void $ think GameAI.walkMonsterStart er
