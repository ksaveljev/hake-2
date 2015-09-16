{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MChick where

import Control.Lens (use, preuse, ix, (^.), (.=), (%=), (+=), (-=), zoom)
import Control.Monad (when, unless, liftM)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isJust)
import Linear (V3(..), _x, _y, _z, normalize)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameMisc as GameMisc
import qualified Game.GameWeapon as GameWeapon
import qualified Game.Monster as Monster
import qualified Game.Monsters.MFlash as MFlash
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

frameAttack101 :: Int
frameAttack101 = 0

frameAttack113 :: Int
frameAttack113 = 12

frameAttack114 :: Int
frameAttack114 = 13

frameAttack127 :: Int
frameAttack127 = 26

frameAttack128 :: Int
frameAttack128 = 27

frameAttack132 :: Int
frameAttack132 = 31

frameAttack201 :: Int
frameAttack201 = 32

frameAttack203 :: Int
frameAttack203 = 34

frameAttack204 :: Int
frameAttack204 = 35

frameAttack212 :: Int
frameAttack212 = 43

frameAttack213 :: Int
frameAttack213 = 44

frameAttack216 :: Int
frameAttack216 = 47

frameDeath101 :: Int
frameDeath101 = 48

frameDeath112 :: Int
frameDeath112 = 59

frameDeath201 :: Int
frameDeath201 = 60

frameDeath223 :: Int
frameDeath223 = 82

frameDuck01 :: Int
frameDuck01 = 83

frameDuck07 :: Int
frameDuck07 = 89

framePain101 :: Int
framePain101 = 90

framePain105 :: Int
framePain105 = 94

framePain201 :: Int
framePain201 = 95

framePain205 :: Int
framePain205 = 99

framePain301 :: Int
framePain301 = 100

framePain321 :: Int
framePain321 = 120

frameStand101 :: Int
frameStand101 = 121

frameStand130 :: Int
frameStand130 = 150

frameStand201 :: Int
frameStand201 = 151

frameStand230 :: Int
frameStand230 = 180

frameWalk01 :: Int
frameWalk01 = 181

frameWalk10 :: Int
frameWalk10 = 190

frameWalk11 :: Int
frameWalk11 = 191

frameWalk20 :: Int
frameWalk20 = 200

chickMoan :: EntThink
chickMoan =
  GenericEntThink "ChickMoan" $ \selfRef -> do
    r <- Lib.randomF

    sound <- use $ gameBaseGlobals.gbGameImport.giSound

    soundIdle <- if r < 0.5
                   then use $ mChickGlobals.mChickSoundIdle1
                   else use $ mChickGlobals.mChickSoundIdle2

    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

chickFramesFidget :: V.Vector MFrameT
chickFramesFidget =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just chickMoan)
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

chickStand :: EntThink
chickStand =
  GenericEntThink "chick_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveStand
    return True

chickMoveFidget :: MMoveT
chickMoveFidget = MMoveT "chickMoveFidget" frameStand201 frameStand230 chickFramesFidget (Just chickStand)

chickFidget :: EntThink
chickFidget =
  GenericEntThink "chick_fidget" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
      then return True
      else do
        r <- Lib.randomF
        when (r <= 0.3) $ 
          gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveFidget

        return True

chickFramesStand :: V.Vector MFrameT
chickFramesStand =
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
               , MFrameT (Just GameAI.aiStand) 0 (Just chickFidget)
               ]

chickMoveStand :: MMoveT
chickMoveStand = MMoveT "chickMoveStand" frameStand101 frameStand130 chickFramesStand Nothing

chickRun :: EntThink
chickRun =
  GenericEntThink "chick_run" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
      then
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveStand
      else do
        let action = case self^.eMonsterInfo.miCurrentMove of
                       Nothing -> chickMoveStartRun
                       Just move -> if (move^.mmId) == (chickMoveWalk^.mmId) || (move^.mmId) == (chickMoveStartRun^.mmId)
                                      then chickMoveRun
                                      else chickMoveStartRun

        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action

    return True

chickFramesStartRun :: V.Vector MFrameT
chickFramesStartRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)   1  Nothing
               , MFrameT (Just GameAI.aiRun)   0  Nothing
               , MFrameT (Just GameAI.aiRun)   0  Nothing
               , MFrameT (Just GameAI.aiRun) (-1) Nothing
               , MFrameT (Just GameAI.aiRun) (-1) Nothing
               , MFrameT (Just GameAI.aiRun)   0  Nothing
               , MFrameT (Just GameAI.aiRun)   1  Nothing
               , MFrameT (Just GameAI.aiRun)   3  Nothing
               , MFrameT (Just GameAI.aiRun)   6  Nothing
               , MFrameT (Just GameAI.aiRun)   3  Nothing
               ]

chickMoveStartRun :: MMoveT
chickMoveStartRun = MMoveT "chickMoveStartRun" frameWalk01 frameWalk10 chickFramesStartRun (Just chickRun)

chickFramesRun :: V.Vector MFrameT
chickFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)  6 Nothing
               , MFrameT (Just GameAI.aiRun)  8 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun)  5 Nothing
               , MFrameT (Just GameAI.aiRun)  7 Nothing
               , MFrameT (Just GameAI.aiRun)  4 Nothing
               , MFrameT (Just GameAI.aiRun) 11 Nothing
               , MFrameT (Just GameAI.aiRun)  5 Nothing
               , MFrameT (Just GameAI.aiRun)  9 Nothing
               , MFrameT (Just GameAI.aiRun)  7 Nothing
               ]

chickMoveRun :: MMoveT
chickMoveRun = MMoveT "chickMoveRun" frameWalk11 frameWalk20 chickFramesRun Nothing

chickFramesWalk :: V.Vector MFrameT
chickFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 Nothing
               , MFrameT (Just GameAI.aiWalk) 13 Nothing
               , MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk)  7 Nothing
               , MFrameT (Just GameAI.aiWalk)  4 Nothing
               , MFrameT (Just GameAI.aiWalk) 11 Nothing
               , MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk)  9 Nothing
               , MFrameT (Just GameAI.aiWalk)  7 Nothing
               ]

chickMoveWalk :: MMoveT
chickMoveWalk = MMoveT "chickMoveWalk" frameWalk11 frameWalk20 chickFramesWalk Nothing

chickWalk :: EntThink
chickWalk =
  GenericEntThink "chick_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveWalk
    return True

chickFramesPain1 :: V.Vector MFrameT
chickFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

chickMovePain1 :: MMoveT
chickMovePain1 = MMoveT "chickMovePain1" framePain101 framePain105 chickFramesPain1 (Just chickRun)

chickFramesPain2 :: V.Vector MFrameT
chickFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

chickMovePain2 :: MMoveT
chickMovePain2 = MMoveT "chickMovePain2" framePain201 framePain205 chickFramesPain2 (Just chickRun)

chickFramesPain3 :: V.Vector MFrameT
chickFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-6) Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)  11  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               , MFrameT (Just GameAI.aiMove)   7  Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove) (-8) Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               ]

chickMovePain3 :: MMoveT
chickMovePain3 = MMoveT "chickMovePain3" framePain301 framePain321 chickFramesPain3 (Just chickRun)

chickPain :: EntPain
chickPain =
  GenericEntPain "chick_pain" $ \selfRef@(EdictReference selfIdx) _ _ damage -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSkinNum .= 1

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      gameBaseGlobals.gbGEdicts.ix selfIdx.ePainDebounceTime .= levelTime + 3

      r <- Lib.randomF

      sfx <- if | r < 0.33 -> use $ mChickGlobals.mChickSoundPain1
                | r < 0.66 -> use $ mChickGlobals.mChickSoundPain2
                | otherwise -> use $ mChickGlobals.mChickSoundPain3

      sound <- use $ gameBaseGlobals.gbGameImport.giSound
      sound (Just selfRef) Constants.chanVoice sfx 1 Constants.attnNorm 0

      skillValue <- liftM (^.cvValue) skillCVar
      
      unless (skillValue == 3) $ do -- no pain anims in nightmare
        let painMove = if | damage <= 10 -> chickMovePain1
                          | damage <= 25 -> chickMovePain2
                          | otherwise -> chickMovePain3

        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just painMove

chickDead :: EntThink
chickDead =
  GenericEntThink "chick_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMins .= V3 (-16) (-16) 0
      eMaxs .= V3 16 16 16
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

chickFramesDeath2 :: V.Vector MFrameT
chickFramesDeath2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-6) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-2) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)  10  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)  15  Nothing
               , MFrameT (Just GameAI.aiMove)  14  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               ]

chickMoveDeath2 :: MMoveT
chickMoveDeath2 = MMoveT "chickMoveDeath2" frameDeath201 frameDeath223 chickFramesDeath2 (Just chickDead)

chickFramesDeath1 :: V.Vector MFrameT
chickFramesDeath1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove) (-7) Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)  11  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

chickMoveDeath1 :: MMoveT
chickMoveDeath1 = MMoveT "chickMoveDeath1" frameDeath101 frameDeath112 chickFramesDeath1 (Just chickDead)

chickDie :: EntDie
chickDie =
  GenericEntDie "chick_die" $ \selfRef@(EdictReference selfIdx) _ _ damage _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    -- check for gib
    if | (self^.eHealth) <= (self^.eGibHealth) -> do
           soundIdx <- soundIndex (Just "misc/udeath.wav")
           sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic

           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

           GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic

           gameBaseGlobals.gbGEdicts.ix selfIdx.eDeadFlag .= Constants.deadDead

       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()

       | otherwise -> do -- regular death
           n <- liftM (`mod` 2) Lib.rand

           let currentMove = if n == 0 then chickMoveDeath1 else chickMoveDeath2

           zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
             eDeadFlag .= Constants.deadDead
             eTakeDamage .= Constants.damageYes
             eMonsterInfo.miCurrentMove .= Just currentMove

           sfx <- if n == 0
                    then use $ mChickGlobals.mChickSoundDeath1
                    else use $ mChickGlobals.mChickSoundDeath2

           sound (Just selfRef) Constants.chanVoice sfx 1 Constants.attnNorm 0

chickDuckDown :: EntThink
chickDuckDown =
  GenericEntThink "chick_duck_down" $ \selfRef@(EdictReference selfIdx) -> do
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

chickDuckHold :: EntThink
chickDuckHold =
  GenericEntThink "chick_duck_hold" $ \(EdictReference selfIdx) -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if levelTime >= (self^.eMonsterInfo.miPauseTime)
      then gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiHoldFrame))
      else gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiHoldFrame)

    return True

chickDuckUp :: EntThink
chickDuckUp =
  GenericEntThink "chick_duck_up" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiDucked))
      eMaxs._z += 32
      eTakeDamage .= Constants.damageAim

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

chickFramesDuck :: V.Vector MFrameT
chickFramesDuck =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  (Just chickDuckDown)
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   4  (Just chickDuckHold)
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove) (-5) (Just chickDuckUp)
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               ]

chickMoveDuck :: MMoveT
chickMoveDuck = MMoveT "chickMoveDuck" frameDuck01 frameDuck07 chickFramesDuck (Just chickRun)

chickDodge :: EntDodge
chickDodge =
  GenericEntDodge "chick_dodge" $ \selfRef@(EdictReference selfIdx) attackerRef _ -> do
    r <- Lib.randomF

    unless (r > 0.25) $ do
      Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

      when (isJust (self^.eEnemy)) $ do
        gameBaseGlobals.gbGEdicts.ix selfIdx.eEnemy .= Just attackerRef

      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveDuck

chickSlash :: EntThink
chickSlash =
  GenericEntThink "ChickSlash" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) 10

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundMeleeSwing <- use $ mChickGlobals.mChickSoundMeleeSwing
    sound (Just selfRef) Constants.chanWeapon soundMeleeSwing 1 Constants.attnNorm 0

    r <- Lib.rand

    GameWeapon.fireHit selfRef aim (10 + fromIntegral (r `mod` 6)) 100

    return True

chickRocket :: EntThink
chickRocket =
  GenericEntThink "ChickRocket" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2ChickRocket1) forward right
        Just (EdictReference enemyIdx) = self^.eEnemy

    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    let V3 a b c = enemy^.eEntityState.esOrigin
        vec = V3 a b (c + fromIntegral (enemy^.eViewHeight))
        dir = normalize (vec - start)

    Monster.monsterFireRocket selfRef start dir 50 500 Constants.mz2ChickRocket1

    return True

chickPreAttack1 :: EntThink
chickPreAttack1 =
  GenericEntThink "Chick_PreAttack1" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundMissilePrelaunch <- use $ mChickGlobals.mChickSoundMissilePrelaunch
    sound (Just selfRef) Constants.chanVoice soundMissilePrelaunch 1 Constants.attnNorm 0
    return True

chickReload :: EntThink
chickReload =
  GenericEntThink "ChickReload" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundMissileReload <- use $ mChickGlobals.mChickSoundMissileReload
    sound (Just selfRef) Constants.chanVoice soundMissileReload 1 Constants.attnNorm 0
    return True

chickAttack1 :: EntThink
chickAttack1 =
  GenericEntThink "chick_attack1" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveAttack1
    return True

chickReRocket :: EntThink
chickReRocket =
  GenericEntThink "chick_rerocket" $ \_ -> do
    io (putStrLn "MChick.chickReRocket") >> undefined -- TODO

chickFramesStartAttack1 :: V.Vector MFrameT
chickFramesStartAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge)   0  (Just chickPreAttack1)
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   4  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-3) Nothing
               , MFrameT (Just GameAI.aiCharge)   3  Nothing
               , MFrameT (Just GameAI.aiCharge)   5  Nothing
               , MFrameT (Just GameAI.aiCharge)   7  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  (Just chickAttack1)
               ]

chickMoveStartAttack1 :: MMoveT
chickMoveStartAttack1 = MMoveT "chickMoveStartAttack1" frameAttack101 frameAttack113 chickFramesStartAttack1 Nothing

chickFramesAttack1 :: V.Vector MFrameT
chickFramesAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge)  19  (Just chickRocket)
               , MFrameT (Just GameAI.aiCharge) (-6) Nothing
               , MFrameT (Just GameAI.aiCharge) (-5) Nothing
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               , MFrameT (Just GameAI.aiCharge) (-7) Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   1  Nothing
               , MFrameT (Just GameAI.aiCharge)  10  (Just chickReload)
               , MFrameT (Just GameAI.aiCharge)   4  Nothing
               , MFrameT (Just GameAI.aiCharge)   5  Nothing
               , MFrameT (Just GameAI.aiCharge)   6  Nothing
               , MFrameT (Just GameAI.aiCharge)   6  Nothing
               , MFrameT (Just GameAI.aiCharge)   4  Nothing
               , MFrameT (Just GameAI.aiCharge)   3  (Just chickReRocket)
               ]

chickMoveAttack1 :: MMoveT
chickMoveAttack1 = MMoveT "chickMoveAttack1" frameAttack114 frameAttack127 chickFramesAttack1 Nothing

chickFramesEndAttack1 :: V.Vector MFrameT
chickFramesEndAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) (-3) Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-6) Nothing
               , MFrameT (Just GameAI.aiCharge) (-4) Nothing
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               ]

chickMoveEndAttack1 :: MMoveT
chickMoveEndAttack1 = MMoveT "chickMoveEndAttack1" frameAttack128 frameAttack132 chickFramesEndAttack1 (Just chickRun)

chickReSlash :: EntThink
chickReSlash =
  GenericEntThink "chick_reslash" $ \_ -> do
    io (putStrLn "MChick.chickReSlash") >> undefined -- TODO

chickFramesSlash :: V.Vector MFrameT
chickFramesSlash =
    V.fromList [ MFrameT (Just GameAI.aiCharge)   1  Nothing
               , MFrameT (Just GameAI.aiCharge)   7  (Just chickSlash)
               , MFrameT (Just GameAI.aiCharge) (-7) Nothing
               , MFrameT (Just GameAI.aiCharge)   1  Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge)   1  Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)   1  Nothing
               , MFrameT (Just GameAI.aiCharge) (-2) (Just chickReSlash)
               ]

chickMoveSlash :: MMoveT
chickMoveSlash = MMoveT "chickMoveSlash" frameAttack204 frameAttack212 chickFramesSlash Nothing

chickFramesEndSlash :: V.Vector MFrameT
chickFramesEndSlash =
    V.fromList [ MFrameT (Just GameAI.aiCharge) (-6) Nothing
               , MFrameT (Just GameAI.aiCharge) (-1) Nothing
               , MFrameT (Just GameAI.aiCharge) (-6) Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               ]

chickMoveEndSlash :: MMoveT
chickMoveEndSlash = MMoveT "chickMoveEndSlash" frameAttack213 frameAttack216 chickFramesEndSlash (Just chickRun)

chickStartSlash :: EntThink
chickStartSlash =
  GenericEntThink "chick_slash" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveSlash
    return True

chickFramesStartSlash :: V.Vector MFrameT
chickFramesStartSlash =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 1 Nothing
               , MFrameT (Just GameAI.aiCharge) 8 Nothing
               , MFrameT (Just GameAI.aiCharge) 3 Nothing
               ]

chickMoveStartSlash :: MMoveT
chickMoveStartSlash = MMoveT "chickMoveStartSlash" frameAttack201 frameAttack203 chickFramesStartSlash (Just chickStartSlash)

chickMelee :: EntThink
chickMelee =
  GenericEntThink "chick_melee" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveStartSlash
    return True

chickAttack :: EntThink
chickAttack =
  GenericEntThink "chick_attack" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just chickMoveStartAttack1
    return True

chickSight :: EntInteract
chickSight =
  GenericEntInteract "chick_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mChickGlobals.mChickSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

{-
- QUAKED monster_chick (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterChick :: EdictReference -> Quake ()
spMonsterChick _ = do
    io (putStrLn "MChick.spMonsterChick") >> undefined -- TODO
