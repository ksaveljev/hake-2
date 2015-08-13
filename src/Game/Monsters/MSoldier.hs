{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MSoldier where

import Control.Lens ((^.), (.=), (%=), (+=), (-=), use, ix, zoom, preuse)
import Control.Monad (liftM, void, when, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil
import qualified QCommon.Com as Com
import qualified Util.Lib as Lib

modelScale :: Float
modelScale = 1.20000

frameAttak101 :: Int
frameAttak101 = 0

frameAttak102 :: Int
frameAttak102 = 1

frameAttak110 :: Int
frameAttak110 = 9

frameAttak112 :: Int
frameAttak112 = 11

frameAttak201 :: Int
frameAttak201 = 12

frameAttak204 :: Int
frameAttak204 = 15

frameAttak216 :: Int
frameAttak216 = 27

frameAttak218 :: Int
frameAttak218 = 29

frameAttak301 :: Int
frameAttak301 = 30

frameAttak303 :: Int
frameAttak303 = 32

frameAttak309 :: Int
frameAttak309 = 38

frameAttak401 :: Int
frameAttak401 = 39

frameAttak406 :: Int
frameAttak406 = 44

frameDuck01 :: Int
frameDuck01 = 45

frameDuck05 :: Int
frameDuck05 = 49

framePain101 :: Int
framePain101 = 50

framePain105 :: Int
framePain105 = 54

framePain201 :: Int
framePain201 = 55

framePain207 :: Int
framePain207 = 61

framePain301 :: Int
framePain301 = 62

framePain318 :: Int
framePain318 = 79

framePain401 :: Int
framePain401 = 80

framePain417 :: Int
framePain417  = 96

frameRun01 :: Int
frameRun01 = 97

frameRun02 :: Int
frameRun02 = 98

frameRun03 :: Int
frameRun03 = 99

frameRun08 :: Int
frameRun08 = 104

frameRuns01 :: Int
frameRuns01 = 109

frameRuns03 :: Int
frameRuns03 = 111

frameRuns14 :: Int
frameRuns14 = 122

frameStand101 :: Int
frameStand101 = 146

frameStand130 :: Int
frameStand130 = 175

frameStand301 :: Int
frameStand301 = 176

frameStand322 :: Int
frameStand322 = 197

frameStand339 :: Int
frameStand339 = 214

frameWalk101 :: Int
frameWalk101 = 215

frameWalk133 :: Int
frameWalk133 = 247

frameWalk209 :: Int
frameWalk209 = 256

frameWalk218 :: Int
frameWalk218 = 265

soldierDead :: EntThink
soldierDead =
  GenericEntThink "soldier_dead" $ \self@(EdictReference selfIdx) -> do
    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMins .= V3 (-16) (-16) (-24)
      eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eNextThink .= 0

    linkEntity self

    return True

soldierDie :: EntDie
soldierDie =
  GenericEntDie "soldier_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MSoldier.soldierDie") >> undefined -- TODO

soldierFramesPain1 :: V.Vector MFrameT
soldierFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

soldierMovePain1 :: MMoveT
soldierMovePain1 = MMoveT "soldierMovePain1" framePain101 framePain105 soldierFramesPain1 (Just soldierRun)

soldierFramesPain2 :: V.Vector MFrameT
soldierFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-13) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    4  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               ]

soldierMovePain2 :: MMoveT
soldierMovePain2 = MMoveT "soldierMovePain2" framePain201 framePain207 soldierFramesPain2 (Just soldierRun)

soldierFramesPain3 :: V.Vector MFrameT
soldierFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-8) Nothing
               , MFrameT (Just GameAI.aiMove)  10  Nothing
               , MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove) (-3) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)   3  Nothing
               , MFrameT (Just GameAI.aiMove)   2  Nothing
               ]

soldierMovePain3 :: MMoveT
soldierMovePain3 = MMoveT "soldierMovePain3" framePain301 framePain318 soldierFramesPain3 (Just soldierRun)

soldierFramesPain4 :: V.Vector MFrameT
soldierFramesPain4 =
    V.fromList [ MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove) (-10) Nothing
               , MFrameT (Just GameAI.aiMove)  (-6) Nothing
               , MFrameT (Just GameAI.aiMove)    8  Nothing
               , MFrameT (Just GameAI.aiMove)    4  Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    5  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)    2  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               ]

soldierMovePain4 :: MMoveT
soldierMovePain4 = MMoveT "soldierMovePain4" framePain401 framePain417 soldierFramesPain4 (Just soldierRun)

soldierPain :: EntPain
soldierPain =
  GenericEntPain "soldier_pain" $ \self@(EdictReference selfIdx) _ _ _ -> do
    Just selfEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((selfEdict^.eHealth) < ((selfEdict^.eMaxHealth) `div` 2)) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSkinNum %= (.|. 1)

    time <- use $ gameBaseGlobals.gbLevel.llTime

    if time < (selfEdict^.ePainDebounceTime)
      then
        when ((selfEdict^.eVelocity._z) > 100 && isJust (selfEdict^.eMonsterInfo.miCurrentMove)) $ do
          let Just move = selfEdict^.eMonsterInfo.miCurrentMove
              moveId = move^.mmId

          when (any (== moveId) ["soldierMovePain1", "soldierMovePain2", "soldierMovePain3"]) $
            gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just soldierMovePain4

      else do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        gameBaseGlobals.gbGEdicts.ix selfIdx.ePainDebounceTime .= time + 3

        let n = (selfEdict^.eEntityState.esSkinNum) .|. 1

        soundPainLight <- use $ mSoldierGlobals.msSoundPainLight
        soundPain <- use $ mSoldierGlobals.msSoundPain
        soundPainSS <- use $ mSoldierGlobals.msSoundPainSS

        let s = if | n == 1 -> soundPainLight
                   | n == 3 -> soundPain
                   | otherwise -> soundPainSS

        sound (Just self) Constants.chanVoice s 1 Constants.attnNorm 0

        if (selfEdict^.eVelocity._z) > 100
          then gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just soldierMovePain4
          else do
            skillValue <- liftM (^.cvValue) skillCVar

            -- no pain anims in nightmare
            unless (skillValue == 3) $ do
              r <- Lib.randomF

              let nextMove = if | r < 0.33 -> soldierMovePain1
                                | r < 0.66 -> soldierMovePain2
                                | otherwise -> soldierMovePain3

              gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just nextMove

soldierFramesStand1 :: V.Vector MFrameT
soldierFramesStand1 =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 (Just soldierIdle)
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

soldierMoveStand1 :: MMoveT
soldierMoveStand1 = MMoveT "soldierMoveStand1" frameStand101 frameStand130 soldierFramesStand1 (Just soldierStand)

soldierFramesStand3 :: V.Vector MFrameT
soldierFramesStand3 =
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
               , MFrameT (Just GameAI.aiStand) 0 (Just soldierCock)
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

soldierMoveStand3 :: MMoveT
soldierMoveStand3 = MMoveT "soldierMoveStand3" frameStand301 frameStand339 soldierFramesStand3 (Just soldierStand)

soldierStand :: EntThink
soldierStand =
  GenericEntThink "soldier_stand" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    r <- Lib.randomF
    let currentMove = self^.eMonsterInfo.miCurrentMove


    let nextMove = if isJust currentMove && ((fromJust currentMove)^.mmId) == "soldierMoveStand3" || r < 0.8
                     then soldierMoveStand1
                     else soldierMoveStand3

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just nextMove

    return True

soldierFramesWalk1 :: V.Vector MFrameT
soldierFramesWalk1 =
    V.fromList [ MFrameT (Just GameAI.aiWalk)   3  Nothing
               , MFrameT (Just GameAI.aiWalk)   6  Nothing
               , MFrameT (Just GameAI.aiWalk)   2  Nothing
               , MFrameT (Just GameAI.aiWalk)   2  Nothing
               , MFrameT (Just GameAI.aiWalk)   2  Nothing
               , MFrameT (Just GameAI.aiWalk)   1  Nothing
               , MFrameT (Just GameAI.aiWalk)   6  Nothing
               , MFrameT (Just GameAI.aiWalk)   5  Nothing
               , MFrameT (Just GameAI.aiWalk)   3  Nothing
               , MFrameT (Just GameAI.aiWalk) (-1) (Just soldierWalk1Random)
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               , MFrameT (Just GameAI.aiWalk)   0  Nothing
               ]

soldierMoveWalk1 :: MMoveT
soldierMoveWalk1 = MMoveT "soldierMoveWalk1" frameWalk101 frameWalk133 soldierFramesWalk1 Nothing

soldierFramesWalk2 :: V.Vector MFrameT
soldierFramesWalk2 =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 9 Nothing
               , MFrameT (Just GameAI.aiWalk) 8 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 1 Nothing
               , MFrameT (Just GameAI.aiWalk) 3 Nothing
               , MFrameT (Just GameAI.aiWalk) 7 Nothing
               , MFrameT (Just GameAI.aiWalk) 6 Nothing
               , MFrameT (Just GameAI.aiWalk) 7 Nothing
               ]

soldierMoveWalk2 :: MMoveT
soldierMoveWalk2 = MMoveT "soldierMoveWalk2" frameWalk209 frameWalk218 soldierFramesWalk2 Nothing

soldierWalk1Random :: EntThink
soldierWalk1Random =
  GenericEntThink "soldier_walk1_random" $ \(EdictReference selfIdx) -> do
    r <- Lib.randomF

    when (r > 0.1) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= frameWalk101

    return True

soldierWalk :: EntThink
soldierWalk =
  GenericEntThink "soldier_walk" $ \(EdictReference selfIdx) -> do
    r <- Lib.randomF

    let nextMove = if r < 0.5
                     then soldierMoveWalk1
                     else soldierMoveWalk2

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just nextMove

    return True

soldierFramesRun :: V.Vector MFrameT
soldierFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 11 Nothing
               , MFrameT (Just GameAI.aiRun) 11 Nothing
               , MFrameT (Just GameAI.aiRun) 16 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun) 15 Nothing
               ]

soldierMoveRun :: MMoveT
soldierMoveRun = MMoveT "soldierMoveRun" frameRun03 frameRun08 soldierFramesRun Nothing

soldierFramesStartRun :: V.Vector MFrameT
soldierFramesStartRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 7 Nothing
               , MFrameT (Just GameAI.aiRun) 5 Nothing
               ]

soldierMoveStartRun :: MMoveT
soldierMoveStartRun = MMoveT "soldierMoveStartRun" frameRun01 frameRun02 soldierFramesStartRun (Just soldierRun)

soldierRun :: EntThink
soldierRun =
  GenericEntThink "soldier_run" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let currentMove = self^.eMonsterInfo.miCurrentMove

    let nextMove = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                     then soldierMoveStand1
                     else if isJust currentMove
                            then do
                              let moveId = (fromJust currentMove)^.mmId
                              if any (== moveId) ["soldierMoveWalk1", "soldierMoveWalk2", "soldierMoveStartRun"]
                                then soldierMoveRun
                                else soldierMoveStartRun
                            else soldierMoveStartRun

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just nextMove

    return True

soldierDodge :: EntDodge
soldierDodge =
  GenericEntDodge "soldier_dodge" $ \(EdictReference selfIdx) attacker eta -> do
    r <- Lib.randomF

    when (r > 0.25) $ do
      Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEnemy

      when (isNothing enemy) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eEnemy .= Just attacker

      skillValue <- liftM (^.cvValue) skillCVar

      nextMove <- if skillValue == 0
                    then return soldierMoveDuck
                    else do
                      time <- use $ gameBaseGlobals.gbLevel.llTime
                      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miPauseTime .= time + eta + 0.3
                      r' <- Lib.randomF

                      return $ if | skillValue == 1 ->
                                      if r' > 0.33
                                        then soldierMoveDuck
                                        else soldierMoveAttack3
                                  | skillValue >= 2 ->
                                      if r' > 0.66
                                        then soldierMoveDuck
                                        else soldierMoveAttack3
                                  | otherwise -> soldierMoveAttack3

      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just nextMove

soldierCock :: EntThink
soldierCock =
  GenericEntThink "soldier_cock" $ \self@(EdictReference selfIdx) -> do
    Just frame <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esFrame
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundCock <- use $ mSoldierGlobals.msSoundCock

    if frame == frameStand322
      then sound (Just self) Constants.chanWeapon soundCock 1 Constants.attnIdle 0
      else sound (Just self) Constants.chanWeapon soundCock 1 Constants.attnNorm 0

    return True

soldierFire1 :: EntThink
soldierFire1 =
  GenericEntThink "soldier_fire1" $ \self -> do
    soldierFire self 0
    return True

soldierFire2 :: EntThink
soldierFire2 =
  GenericEntThink "soldier_fire2" $ \self -> do
    soldierFire self 1
    return True

soldierFire3 :: EntThink
soldierFire3 =
  GenericEntThink "soldier_fire3" $ \self -> do
    void $ think soldierDuckDown self
    soldierFire self 2
    return True

soldierFire4 :: EntThink
soldierFire4 =
  GenericEntThink "soldier_fire4" $ \self -> do
    soldierFire self 3
    return True

soldierFire6 :: EntThink
soldierFire6 =
  GenericEntThink "soldier_fire6" $ \self -> do
    soldierFire self 5
    return True

soldierFire7 :: EntThink
soldierFire7 =
  GenericEntThink "soldier_fire7" $ \self -> do
    soldierFire self 6
    return True

soldierFire8 :: EntThink
soldierFire8 =
  GenericEntThink "soldier_fire8" $ \self -> do
    soldierFire self 7
    return True

soldierFire :: EdictReference -> Int -> Quake ()
soldierFire _ _ = io (putStrLn "MSoldier.soldierFire") >> undefined -- TODO

soldierAttack1Refire1 :: EntThink
soldierAttack1Refire1 =
  GenericEntThink "soldier_attack1_refire1" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    unless (self^.eEntityState.esSkinNum > 1 || enemy^.eHealth <= 0) $ do
      skillValue <- liftM (^.cvValue) skillCVar
      r <- Lib.randomF

      let nextFrame = if (skillValue == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee
                        then frameAttak102
                        else frameAttak110

      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= nextFrame

    return True

soldierAttack1Refire2 :: EntThink
soldierAttack1Refire2 =
  GenericEntThink "soldier_attack1_refire2" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    unless (self^.eEntityState.esSkinNum < 2 || enemy^.eHealth <= 0) $ do
      skillValue <- liftM (^.cvValue) skillCVar
      r <- Lib.randomF

      when ((skillValue == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= frameAttak102

    return True

soldierAttack2Refire1 :: EntThink
soldierAttack2Refire1 =
  GenericEntThink "soldier_attack2_refire1" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    unless (self^.eEntityState.esSkinNum > 1 || enemy^.eHealth <= 0) $ do
      skillValue <- liftM (^.cvValue) skillCVar
      r <- Lib.randomF

      let nextFrame = if (skillValue == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee
                        then frameAttak204
                        else frameAttak216

      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= nextFrame

    return True

soldierAttack2Refire2 :: EntThink
soldierAttack2Refire2 =
  GenericEntThink "soldier_attack2_refire2" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    unless (self^.eEntityState.esSkinNum < 2 || enemy^.eHealth <= 0) $ do
      skillValue <- liftM (^.cvValue) skillCVar
      r <- Lib.randomF

      when ((skillValue == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= frameAttak204

    return True

soldierAttack3Refire :: EntThink
soldierAttack3Refire =
  GenericEntThink "soldier_attack3_refire" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    time <- use $ gameBaseGlobals.gbLevel.llTime

    when ((time + 0.4) < (self^.eMonsterInfo.miPauseTime)) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= frameAttak303

    return True

soldierAttack6Refire :: EntThink
soldierAttack6Refire =
  GenericEntThink "soldier_attack6_refire" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    unless (enemy^.eHealth <= 0 || GameUtil.range self enemy < Constants.rangeMid) $ do
      skillValue <- liftM (^.cvValue) skillCVar

      when (skillValue == 3) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miNextFrame .= frameRuns03

    return True

soldierDuckUp :: EntThink
soldierDuckUp =
  GenericEntThink "soldier_duck_up" $ \self@(EdictReference selfIdx) -> do
    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiDucked))
      eMaxs._z += 32
      eTakeDamage .= Constants.damageAim

    linkEntity self

    return True

soldierDuckDown :: EntThink
soldierDuckDown =
  GenericEntThink "soldier_duck_down" $ \self@(EdictReference selfIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((edict^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked == 0) $ do
      linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
      time <- use $ gameBaseGlobals.gbLevel.llTime

      zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
        eMonsterInfo.miAIFlags %= (.|. Constants.aiDucked)
        eMaxs._z -= 32
        eTakeDamage .= Constants.damageYes
        eMonsterInfo.miPauseTime .= time + 1

      linkEntity self

    return True

soldierDuckHold :: EntThink
soldierDuckHold =
  GenericEntThink "soldier_duck_hold" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    time <- use $ gameBaseGlobals.gbLevel.llTime

    let updateMI = if time >= self^.eMonsterInfo.miPauseTime
                     then (.&. (complement Constants.aiHoldFrame))
                     else (.|. Constants.aiHoldFrame)

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= updateMI

    return True

soldierFramesAttack1 :: V.Vector MFrameT
soldierFramesAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire1)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack1Refire1)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierCock)
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack1Refire2)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack1 :: MMoveT
soldierMoveAttack1 = MMoveT "soldierMoveAttack1" frameAttak101 frameAttak112 soldierFramesAttack1 (Just soldierRun)

soldierFramesAttack2 :: V.Vector MFrameT
soldierFramesAttack2 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire2)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack2Refire1)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierCock)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack2Refire2)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack2 :: MMoveT
soldierMoveAttack2 = MMoveT "soldierMoveAttack2" frameAttak201 frameAttak218 soldierFramesAttack2 (Just soldierRun)

soldierFramesAttack3 :: V.Vector MFrameT
soldierFramesAttack3 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire3)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack3Refire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierDuckUp)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack3 :: MMoveT
soldierMoveAttack3 = MMoveT "soldierMoveAttack3" frameAttak301 frameAttak309 soldierFramesAttack3 (Just soldierRun)

soldierFramesAttack4 :: V.Vector MFrameT
soldierFramesAttack4 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire4)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack4 :: MMoveT
soldierMoveAttack4 = MMoveT "soldierMoveAttack4" frameAttak401 frameAttak406 soldierFramesAttack4 (Just soldierRun)

soldierFramesAttack6 :: V.Vector MFrameT
soldierFramesAttack6 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 10 Nothing
               , MFrameT (Just GameAI.aiCharge)  4 Nothing
               , MFrameT (Just GameAI.aiCharge) 12 Nothing
               , MFrameT (Just GameAI.aiCharge) 11 (Just soldierFire8)
               , MFrameT (Just GameAI.aiCharge) 13 Nothing
               , MFrameT (Just GameAI.aiCharge) 18 Nothing
               , MFrameT (Just GameAI.aiCharge) 15 Nothing
               , MFrameT (Just GameAI.aiCharge) 14 Nothing
               , MFrameT (Just GameAI.aiCharge) 11 Nothing
               , MFrameT (Just GameAI.aiCharge)  8 Nothing
               , MFrameT (Just GameAI.aiCharge) 11 Nothing
               , MFrameT (Just GameAI.aiCharge) 12 Nothing
               , MFrameT (Just GameAI.aiCharge) 12 Nothing
               , MFrameT (Just GameAI.aiCharge) 17 (Just soldierAttack6Refire)
               ]

soldierMoveAttack6 :: MMoveT
soldierMoveAttack6 = MMoveT "soldierMoveAttack6" frameRuns01 frameRuns14 soldierFramesAttack6 (Just soldierRun)

soldierFramesDuck :: V.Vector MFrameT
soldierFramesDuck =
    V.fromList [ MFrameT (Just GameAI.aiMove)   5  (Just soldierDuckDown)
               , MFrameT (Just GameAI.aiMove) (-1) (Just soldierDuckHold)
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  (Just soldierDuckUp)
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               ]

soldierMoveDuck :: MMoveT
soldierMoveDuck = MMoveT "soldierMoveDuck" frameDuck01 frameDuck05 soldierFramesDuck (Just soldierRun)

soldierAttack :: EntThink
soldierAttack =
  GenericEntThink "soldier_attack" $ \(EdictReference edictIdx) -> do
    Just skinNum <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSkinNum

    nextMove <- if skinNum < 4
                  then do
                    v <- Lib.randomF
                    return $ if v < 0.5 then soldierMoveAttack1 else soldierMoveAttack2
                  else return soldierMoveAttack4

    gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just nextMove

    return True

soldierIdle :: EntThink
soldierIdle =
  GenericEntThink "soldier_idle" $ \self -> do
    r <- Lib.randomF
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mSoldierGlobals.msSoundIdle

    when (r > 0.8) $
      sound (Just self) Constants.chanVoice soundIdle 1 Constants.attnIdle 0

    return True

soldierSight :: EntInteract
soldierSight =
  GenericEntInteract "soldier_sight" $ \self@(EdictReference selfIdx) _ -> do
    Just selfEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = selfEdict^.eEnemy
    Just enemyEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    r <- Lib.randomF
    skillValue <- liftM (^.cvValue) skillCVar
    soundSight1 <- use $ mSoldierGlobals.msSoundSight1
    soundSight2 <- use $ mSoldierGlobals.msSoundSight2

    if r < 0.5
      then sound (Just self) Constants.chanVoice soundSight1 1 Constants.attnNorm 0
      else sound (Just self) Constants.chanVoice soundSight2 1 Constants.attnNorm 0

    when (skillValue > 0 && GameUtil.range selfEdict enemyEdict >= Constants.rangeMid) $
      when (r > 0.5) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just soldierMoveAttack6

    return True

spMonsterSoldierX :: EntThink
spMonsterSoldierX =
  GenericEntThink "SP_monster_soldier_x" $ \er@(EdictReference edictIdx) -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    tris <- modelIndex (Just "models/monsters/soldier/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEntityState.esModelIndex .= tris
      eMonsterInfo.miScale .= modelScale
      eMins .= V3 (-16) (-16) (-24)
      eMaxs .= V3 16 16 32
      eMoveType .= Constants.moveTypeStep
      eSolid .= Constants.solidBbox

    soundIndex (Just "soldier/solidle1.wav") >>= (mSoldierGlobals.msSoundIdle .=)
    soundIndex (Just "soldier/solsght1.wav") >>= (mSoldierGlobals.msSoundSight1 .=)
    soundIndex (Just "soldier/solsrch1.wav") >>= (mSoldierGlobals.msSoundSight2 .=)
    soundIndex (Just "infantry/infatck3.wav") >>= (mSoldierGlobals.msSoundCock .=)

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMass .= 100

      ePain .= Just soldierPain
      eDie  .= Just soldierDie

      eMonsterInfo.miStand  .= Just soldierStand
      eMonsterInfo.miWalk   .= Just soldierWalk
      eMonsterInfo.miRun    .= Just soldierRun
      eMonsterInfo.miDodge  .= Just soldierDodge
      eMonsterInfo.miAttack .= Just soldierAttack
      eMonsterInfo.miMelee  .= Nothing
      eMonsterInfo.miSight  .= Just soldierSight

    linkEntity er

    void $ think soldierStand er
    void $ think GameAI.walkMonsterStart er

    return True

{-
- QUAKED monster_soldier (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterSoldier :: EntThink
spMonsterSoldier =
  GenericEntThink "SP_monster_soldier" $ \er@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    Com.dprintf $ "Spawning a soldier at " `B.append` BC.pack (show (edict^.eEntityState.esOrigin)) -- IMPROVE

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        void $ think spMonsterSoldierX er

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

        soundIndex (Just "soldier/solpain1.wav") >>= (mSoldierGlobals.msSoundPain .=)
        soundIndex (Just "soldier/soldeth1.wav") >>= (mSoldierGlobals.msSoundDeath .=)
        void $ soundIndex (Just "soldier/solatck1.wav")

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esSkinNum .= 2
          eHealth .= 30
          eGibHealth .= (-30)

    return True

{-
- QUAKED monster_soldier_ss (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterSoldierSS :: EntThink
spMonsterSoldierSS =
  GenericEntThink "SP_monster_soldier_ss" $ \er@(EdictReference edictIdx) -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        void $ think spMonsterSoldierX er

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

        soundIndex (Just "soldier/solpain3.wav") >>= (mSoldierGlobals.msSoundPainSS .=)
        soundIndex (Just "soldier/soldeth3.wav") >>= (mSoldierGlobals.msSoundDeathSS .=)
        void $ soundIndex (Just "soldier/solatck3.wav")

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esSkinNum .= 4
          eHealth .= 40
          eGibHealth .= (-30)

    return True

{-
- QUAKED monster_soldier_light (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterSoldierLight :: EntThink
spMonsterSoldierLight =
  GenericEntThink "SP_monster_soldier_light" $ \er@(EdictReference edictIdx) -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        void $ think spMonsterSoldierX er

        gameImport <- use $ gameBaseGlobals.gbGameImport
        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex

        soundIndex (Just "soldier/solpain2.wav") >>= (mSoldierGlobals.msSoundPainLight .=)
        soundIndex (Just "soldier/soldeth2.wav") >>= (mSoldierGlobals.msSoundDeathLight .=)
        void $ modelIndex (Just "models/objects/laser/tris.md2")
        void $ soundIndex (Just "misc/lasfly.wav")
        void $ soundIndex (Just "soldier/solatck2.wav")

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esSkinNum .= 0
          eHealth .= 20
          eGibHealth .= (-30)

    return True
