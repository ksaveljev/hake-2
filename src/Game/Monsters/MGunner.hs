{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MGunner where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (+=))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.), complement)
import Linear (V3(..), _z)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

frameStand01 :: Int
frameStand01 = 0

frameStand30 :: Int
frameStand30 = 29

frameStand31 :: Int
frameStand31 = 30

frameStand70 :: Int
frameStand70 = 69

frameWalk07 :: Int
frameWalk07 = 76

frameWalk19 :: Int
frameWalk19 = 88

frameRun01 :: Int
frameRun01 = 94

frameRun08 :: Int
frameRun08 = 101

frameRunShoot01 :: Int
frameRunShoot01 = 102

frameRunShoot06 :: Int
frameRunShoot06 = 107

frameAttack101 :: Int
frameAttack101 = 108

frameAttack121 :: Int
frameAttack121 = 128

frameAttack209 :: Int
frameAttack209 = 137

frameAttack215 :: Int
frameAttack215 = 143

frameAttack216 :: Int
frameAttack216 = 144

frameAttack223 :: Int
frameAttack223 = 151

frameAttack224 :: Int
frameAttack224 = 152

frameAttack230 :: Int
frameAttack230 = 158

framePain101 :: Int
framePain101 = 159

framePain118 :: Int
framePain118 = 176

framePain201 :: Int
framePain201 = 177

framePain208 :: Int
framePain208 = 184

framePain301 :: Int
framePain301 = 185

framePain305 :: Int
framePain305 = 189

frameDeath01 :: Int
frameDeath01 = 190

frameDeath11 :: Int
frameDeath11 = 200

frameDuck01 :: Int
frameDuck01 = 201

frameDuck08 :: Int
frameDuck08 = 208

gunnerIdleSound :: EntThink
gunnerIdleSound =
  GenericEntThink "gunner_idlesound" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mGunnerGlobals.mGunnerSoundIdle
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

gunnerSight :: EntInteract
gunnerSight =
  GenericEntInteract "gunner_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mGunnerGlobals.mGunnerSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

gunnerSearch :: EntThink
gunnerSearch =
  GenericEntThink "gunner_search" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSearch <- use $ mGunnerGlobals.mGunnerSoundSearch
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

gunnerFramesFidget :: V.Vector MFrameT
gunnerFramesFidget =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just gunnerIdleSound)
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

gunnerStand :: EntThink
gunnerStand =
  GenericEntThink "gunner_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just gunnerMoveStand
    return True

gunnerMoveFidget :: MMoveT
gunnerMoveFidget = MMoveT "gunnerMoveFidget" frameStand31 frameStand70 gunnerFramesFidget (Just gunnerStand)

gunnerFidget :: EntThink
gunnerFidget =
  GenericEntThink "gunner_fidget" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
      then return True
      else do
        r <- Lib.randomF

        when (r <= 0.05) $
          gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just gunnerMoveFidget

        return True

gunnerFramesStand :: V.Vector MFrameT
gunnerFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just gunnerFidget)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just gunnerFidget)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just gunnerFidget)
               ]

gunnerMoveStand :: MMoveT
gunnerMoveStand = MMoveT "gunnerMoveStand" frameStand01 frameStand30 gunnerFramesStand Nothing

gunnerFramesWalk :: V.Vector MFrameT
gunnerFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 3 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 7 Nothing
               , MFrameT (Just GameAI.aiWalk) 2 Nothing
               , MFrameT (Just GameAI.aiWalk) 6 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 2 Nothing
               , MFrameT (Just GameAI.aiWalk) 7 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 7 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               ]

gunnerMoveWalk :: MMoveT
gunnerMoveWalk = MMoveT "gunnerMoveWalk" frameWalk07 frameWalk19 gunnerFramesWalk Nothing

gunnerWalk :: EntThink
gunnerWalk =
  GenericEntThink "gunner_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just gunnerMoveWalk
    return True

gunnerFramesRun :: V.Vector MFrameT
gunnerFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 26 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               , MFrameT (Just GameAI.aiRun) 15 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun)  6 Nothing
               ]

gunnerMoveRun :: MMoveT
gunnerMoveRun = MMoveT "gunnerMoveRun" frameRun01 frameRun08 gunnerFramesRun Nothing

gunnerRun :: EntThink
gunnerRun =
  GenericEntThink "gunner_run" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then gunnerMoveStand
                   else gunnerMoveRun

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

gunnerFramesRunAndShoot :: V.Vector MFrameT
gunnerFramesRunAndShoot =
    V.fromList [ MFrameT (Just GameAI.aiRun) 32 Nothing
               , MFrameT (Just GameAI.aiRun) 15 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 18 Nothing
               , MFrameT (Just GameAI.aiRun)  8 Nothing
               , MFrameT (Just GameAI.aiRun) 20 Nothing
               ]

gunnerMoveRunAndShoot :: MMoveT
gunnerMoveRunAndShoot = MMoveT "gunnerMoveRunAndShoot" frameRunShoot01 frameRunShoot06 gunnerFramesRunAndShoot Nothing

gunnerRunAndShoot :: EntThink
gunnerRunAndShoot =
  GenericEntThink "gunner_runandshoot" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just gunnerMoveRunAndShoot
    return True

gunnerFramesPain3 :: V.Vector MFrameT
gunnerFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               ]

gunnerMovePain3 :: MMoveT
gunnerMovePain3 = MMoveT "gunnerMovePain3" framePain301 framePain305 gunnerFramesPain3 (Just gunnerRun)

gunnerFramesPain2 :: V.Vector MFrameT
gunnerFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove)  11  Nothing
               , MFrameT (Just GameAI.aiMove)   6  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-7) Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove) (-7) Nothing
               ]

gunnerMovePain2 :: MMoveT
gunnerMovePain2 = MMoveT "gunnerMovePain2" framePain201 framePain208 gunnerFramesPain2 (Just gunnerRun)

gunnerFramesPain1 :: V.Vector MFrameT
gunnerFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

gunnerMovePain1 :: MMoveT
gunnerMovePain1 = MMoveT "gunnerMovePain1" framePain101 framePain118 gunnerFramesPain1 (Just gunnerRun)

gunnerPain :: EntPain
gunnerPain =
  GenericEntPain "gunner_pain" $ \selfRef@(EdictReference selfIdx) _ _ damage -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSkinNum .= 1

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      gameBaseGlobals.gbGEdicts.ix selfIdx.ePainDebounceTime .= levelTime + 3

      sound <- use $ gameBaseGlobals.gbGameImport.giSound
      r <- Lib.rand
      soundPain <- if r .&. 1 /= 0
                     then use $ mGunnerGlobals.mGunnerSoundPain
                     else use $ mGunnerGlobals.mGunnerSoundPain2

      sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nighmare
        let currentMove = if | damage <= 10 -> gunnerMovePain3
                             | damage <= 25 -> gunnerMovePain2
                             | otherwise -> gunnerMovePain1

        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove

gunnerDead :: EntThink
gunnerDead =
  GenericEntThink "gunner_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMins .= V3 (-16) (-16) (-24)
      eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

gunnerFramesDeath :: V.Vector MFrameT
gunnerFramesDeath =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-7) Nothing
               , MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove)   8  Nothing
               , MFrameT (Just GameAI.aiMove)   6  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

gunnerMoveDeath :: MMoveT
gunnerMoveDeath = MMoveT "gunnerMoveDeath" frameDeath01 frameDeath11 gunnerFramesDeath (Just gunnerDead)

gunnerDie :: EntDie
gunnerDie =
  GenericEntDie "gunner_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MGunner.gunnerDie") >> undefined -- TODO

gunnerDuckDown :: EntThink
gunnerDuckDown =
  GenericEntThink "gunner_duck_down" $ \_ -> do
    io (putStrLn "MGunner.gunnerDuckDown") >> undefined -- TODO

gunnerDuckHold :: EntThink
gunnerDuckHold =
  GenericEntThink "gunner_duck_hold" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if levelTime >= (self^.eMonsterInfo.miPauseTime)
      then gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiHoldFrame))
      else gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiHoldFrame)

    return True

gunnerDuckUp :: EntThink
gunnerDuckUp =
  GenericEntThink "gunner_duck_up" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiDucked))
      eMaxs._z += 32
      eTakeDamage .= Constants.damageAim

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

gunnerFramesDuck :: V.Vector MFrameT
gunnerFramesDuck =
    V.fromList [ MFrameT (Just GameAI.aiMove)   1  (Just gunnerDuckDown)
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   1  (Just gunnerDuckHold)
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   0  (Just gunnerDuckUp)
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               ]

gunnerMoveDuck :: MMoveT
gunnerMoveDuck = MMoveT "gunnerMoveDuck" frameDuck01 frameDuck08 gunnerFramesDuck (Just gunnerRun)

gunnerDodge :: EntDodge
gunnerDodge =
  GenericEntDodge "gunner_dodge" $ \_ _ _ -> do
    io (putStrLn "MGunner.gunnerDodge") >> undefined -- TODO

gunnerOpenGun :: EntThink
gunnerOpenGun =
  GenericEntThink "gunner_opengun" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundOpen <- use $ mGunnerGlobals.mGunnerSoundOpen
    sound (Just selfRef) Constants.chanVoice soundOpen 1 Constants.attnIdle 0
    return True

gunnerFire :: EntThink
gunnerFire =
  GenericEntThink "GunnerFire" $ \_ -> do
    io (putStrLn "MGunner.gunnerFire") >> undefined -- TODO

gunnerGrenade :: EntThink
gunnerGrenade =
  GenericEntThink "GunnerGrenade" $ \_ -> do
    io (putStrLn "MGunner.gunnerGrenade") >> undefined -- TODO

gunnerAttack :: EntThink
gunnerAttack =
  GenericEntThink "gunner_attack" $ \_ -> do
    io (putStrLn "MGunner.gunnerAttack") >> undefined -- TODO

gunnerFireChain :: EntThink
gunnerFireChain =
  GenericEntThink "gunner_fire_chain" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just gunnerMoveFireChain
    return True

gunnerFramesAttackChain :: V.Vector MFrameT
gunnerFramesAttackChain =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just gunnerOpenGun)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

gunnerMoveAttackChain :: MMoveT
gunnerMoveAttackChain = MMoveT "gunnerMoveAttackChain" frameAttack209 frameAttack215 gunnerFramesAttackChain (Just gunnerFireChain)

gunnerFramesFireChain :: V.Vector MFrameT
gunnerFramesFireChain =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerFire)
               ]

gunnerReFireChain :: EntThink
gunnerReFireChain =
  GenericEntThink "gunner_refire_chain" $ \_ -> do
    io (putStrLn "MGunner.gunnerReFireChain") >> undefined -- TODO

gunnerMoveFireChain :: MMoveT
gunnerMoveFireChain = MMoveT "gunnerMoveFireChain" frameAttack216 frameAttack223 gunnerFramesFireChain (Just gunnerReFireChain)

gunnerFramesEndFireChain :: V.Vector MFrameT
gunnerFramesEndFireChain =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

gunnerMoveEndFireChain :: MMoveT
gunnerMoveEndFireChain = MMoveT "gunnerMoveEndFireChain" frameAttack224 frameAttack230 gunnerFramesEndFireChain (Just gunnerRun)

gunnerFramesAttackGrenade :: V.Vector MFrameT
gunnerFramesAttackGrenade =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gunnerGrenade)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

gunnerMoveAttackGrenade :: MMoveT
gunnerMoveAttackGrenade = MMoveT "gunnerMoveAttackGrenade" frameAttack101 frameAttack121 gunnerFramesAttackGrenade (Just gunnerRun)

{-
- QUAKED monster_gunner (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterGunner :: EdictReference -> Quake ()
spMonsterGunner _ = do
    io (putStrLn "MGunner.spMonsterGunner") >> undefined -- TODO
