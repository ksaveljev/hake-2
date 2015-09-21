{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MSuperTank where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.))
import Linear (V3(..), _y, normalize, norm)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameMisc as GameMisc
import qualified Game.GameUtil as GameUtil
import qualified Game.Monster as Monster
import qualified Game.Monsters.MFlash as MFlash
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

frameAttack101 :: Int
frameAttack101 = 0

frameAttack106 :: Int
frameAttack106 = 5

frameAttack107 :: Int
frameAttack107 = 6

frameAttack120 :: Int
frameAttack120 = 19

frameAttack201 :: Int
frameAttack201 = 20

frameAttack208 :: Int
frameAttack208 = 27

frameAttack211 :: Int
frameAttack211 = 30

frameAttack227 :: Int
frameAttack227 = 46

frameAttack301 :: Int
frameAttack301 = 47

frameAttack327 :: Int
frameAttack327 = 73

frameAttack401 :: Int
frameAttack401 = 74

frameAttack406 :: Int
frameAttack406 = 79

frameBackward1 :: Int
frameBackward1 = 80

frameBackward18 :: Int
frameBackward18 = 97

frameDeath1 :: Int
frameDeath1 = 98

frameDeath24 :: Int
frameDeath24 = 121

frameForward1 :: Int
frameForward1 = 128

frameForward18 :: Int
frameForward18 = 145

frameLeft1 :: Int
frameLeft1 = 146

frameLeft18 :: Int
frameLeft18 = 163

framePain101 :: Int
framePain101 = 164

framePain104 :: Int
framePain104 = 167

framePain205 :: Int
framePain205 = 168

framePain208 :: Int
framePain208 = 171

framePain309 :: Int
framePain309 = 172

framePain312 :: Int
framePain312 = 175

frameRight1 :: Int
frameRight1 = 176

frameRight18 :: Int
frameRight18 = 193

frameStand1 :: Int
frameStand1 = 194

frameStand60 :: Int
frameStand60 = 253

treadSound :: EntThink
treadSound =
  GenericEntThink "TreadSound" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundTread <- use $ mSuperTankGlobals.mSuperTankTreadSound
    sound (Just selfRef) Constants.chanVoice soundTread 1 Constants.attnNorm 0
    return True

superTankSearch :: EntThink
superTankSearch =
  GenericEntThink "supertank_search" $ \selfRef -> do
    r <- Lib.randomF

    soundSearch <- if r < 0.5
                     then use $ mSuperTankGlobals.mSuperTankSoundSearch1
                     else use $ mSuperTankGlobals.mSuperTankSoundSearch2

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

superTankFramesStand :: V.Vector MFrameT
superTankFramesStand =
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

superTankMoveStand :: MMoveT
superTankMoveStand = MMoveT "superTankMoveStand" frameStand1 frameStand60 superTankFramesStand Nothing

superTankStand :: EntThink
superTankStand =
  GenericEntThink "supertank_stand" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just superTankMoveStand
    return True

superTankFramesRun :: V.Vector MFrameT
superTankFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 12 (Just treadSound)
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               ]

superTankMoveRun :: MMoveT
superTankMoveRun = MMoveT "superTankMoveRun" frameForward1 frameForward18 superTankFramesRun Nothing

superTankFramesForward :: V.Vector MFrameT
superTankFramesForward =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 4 (Just treadSound)
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

superTankMoveForward :: MMoveT
superTankMoveForward = MMoveT "superTankMoveForward" frameForward1 frameForward18 superTankFramesForward Nothing

superTankForward :: EntThink
superTankForward =
  GenericEntThink "supertank_forward" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just superTankMoveForward
    return True

superTankWalk :: EntThink
superTankWalk =
  GenericEntThink "supertank_walk" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just superTankMoveForward
    return True

superTankRun :: EntThink
superTankRun =
  GenericEntThink "supertank_run" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then superTankMoveStand
                   else superTankMoveRun

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

superTankDead :: EntThink
superTankDead =
  GenericEntThink "supertank_dead" $ \selfRef@(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMins .= V3 (-60) (-60) 0
      eMaxs .= V3 60 60 72
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eNextThink .= 0

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

superTankRocket :: EntThink
superTankRocket =
  GenericEntThink "supertankRocket" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let flashNumber = if | (self^.eEntityState.esFrame) == frameAttack208 -> Constants.mz2SupertankRocket1
                         | (self^.eEntityState.esFrame) == frameAttack211 -> Constants.mz2SupertankRocket2
                         | otherwise -> Constants.mz2SupertankRocket3

        (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
        Just (EdictReference enemyIdx) = self^.eEnemy

    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    let V3 a b c = enemy^.eEntityState.esOrigin
        vec = V3 a b (c + fromIntegral (enemy^.eViewHeight))
        dir = normalize (vec - start)

    Monster.monsterFireRocket selfRef start dir 50 500 flashNumber
    return True

superTankMachineGun :: EntThink
superTankMachineGun =
  GenericEntThink "supertankMachineGun" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let flashNumber = Constants.mz2SupertankMachinegun1 + (self^.eEntityState.esFrame) - frameAttack101
        -- FIXME!!!
        dir = V3 0 (self^.eEntityState.esAngles._y) 0
        (Just forward, Just right, _) = Math3D.angleVectors dir True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right

    case self^.eEnemy of
      Just (EdictReference enemyIdx) -> do
        Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

        let V3 a b c = (enemy^.eEntityState.esOrigin) + fmap (* 0) (enemy^.eVelocity)
            vec = V3 a b (c + fromIntegral (enemy^.eViewHeight))
            forward' = normalize (vec - start)

        Monster.monsterFireBullet selfRef start forward' 6 4 Constants.defaultBulletHspread Constants.defaultBulletVspread flashNumber

      Nothing ->
        Monster.monsterFireBullet selfRef start forward 6 4 Constants.defaultBulletHspread Constants.defaultBulletVspread flashNumber

    return True

superTankAttack :: EntThink
superTankAttack =
  GenericEntThink "supertank_attack" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let Just (EdictReference enemyIdx) = self^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    let vec = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        range = norm vec

    currentMove <- if range <= 160
                     then
                       return superTankMoveAttack1
                     else do
                       r <- Lib.randomF
                       return $ if r < 0.3
                                  then superTankMoveAttack1
                                  else superTankMoveAttack2

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove
    return True

superTankFramesTurnRight :: V.Vector MFrameT
superTankFramesTurnRight =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 (Just treadSound)
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

superTankMoveTurnRight :: MMoveT
superTankMoveTurnRight = MMoveT "superTankMoveTurnRight" frameRight1 frameRight18 superTankFramesTurnRight (Just superTankRun)

superTankFramesTurnLeft :: V.Vector MFrameT
superTankFramesTurnLeft =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 (Just treadSound)
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

superTankMoveTurnLeft :: MMoveT
superTankMoveTurnLeft = MMoveT "superTankMoveTurnLeft" frameLeft1 frameLeft18 superTankFramesTurnLeft (Just superTankRun)

superTankFramesPain3 :: V.Vector MFrameT
superTankFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

superTankMovePain3 :: MMoveT
superTankMovePain3 = MMoveT "superTankMovePain3" framePain309 framePain312 superTankFramesPain3 (Just superTankRun)

superTankFramesPain2 :: V.Vector MFrameT
superTankFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

superTankMovePain2 :: MMoveT
superTankMovePain2 = MMoveT "superTankMovePain2" framePain205 framePain208 superTankFramesPain2 (Just superTankRun)

superTankFramesPain1 :: V.Vector MFrameT
superTankFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

superTankMovePain1 :: MMoveT
superTankMovePain1 = MMoveT "superTankMovePain1" framePain101 framePain104 superTankFramesPain1 (Just superTankRun)

superTankFramesDeath1 :: V.Vector MFrameT
superTankFramesDeath1 =
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
               , MFrameT (Just GameAI.aiMove) 0 (Just bossExplode)
               ]

superTankMoveDeath :: MMoveT
superTankMoveDeath = MMoveT "superTankMoveDeath" frameDeath1 frameDeath24 superTankFramesDeath1 (Just superTankDead)

superTankFramesBackward :: V.Vector MFrameT
superTankFramesBackward =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 0 (Just treadSound)
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               , MFrameT (Just GameAI.aiWalk) 0 Nothing
               ]

superTankMoveBackward :: MMoveT
superTankMoveBackward = MMoveT "superTankMoveBackward" frameBackward1 frameBackward18 superTankFramesBackward Nothing

superTankFramesAttack4 :: V.Vector MFrameT
superTankFramesAttack4 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

superTankMoveAttack4 :: MMoveT
superTankMoveAttack4 = MMoveT "superTankMoveAttack4" frameAttack401 frameAttack406 superTankFramesAttack4 (Just superTankRun)

superTankFramesAttack3 :: V.Vector MFrameT
superTankFramesAttack3 =
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
               ]

superTankMoveAttack3 :: MMoveT
superTankMoveAttack3 = MMoveT "superTankMoveAttack3" frameAttack301 frameAttack327 superTankFramesAttack3 (Just superTankRun)

superTankFramesAttack2 :: V.Vector MFrameT
superTankFramesAttack2 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just superTankRocket)
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 (Just superTankRocket)
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 (Just superTankRocket)
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               , MFrameT (Just GameAI.aiMove)   0 Nothing
               ]

superTankMoveAttack2 :: MMoveT
superTankMoveAttack2 = MMoveT "superTankMoveAttack2" frameAttack201 frameAttack227 superTankFramesAttack2 (Just superTankRun)

superTankReAttack1 :: EntThink
superTankReAttack1 =
  GenericEntThink "supertank_reattack1" $ \_ -> do
    io (putStrLn "MSuperTank.superTankReAttack1") >> undefined -- TODO

superTankFramesAttack1 :: V.Vector MFrameT
superTankFramesAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just superTankMachineGun)
               , MFrameT (Just GameAI.aiCharge) 0 (Just superTankMachineGun)
               , MFrameT (Just GameAI.aiCharge) 0 (Just superTankMachineGun)
               , MFrameT (Just GameAI.aiCharge) 0 (Just superTankMachineGun)
               , MFrameT (Just GameAI.aiCharge) 0 (Just superTankMachineGun)
               , MFrameT (Just GameAI.aiCharge) 0 (Just superTankMachineGun)
               ]

superTankMoveAttack1 :: MMoveT
superTankMoveAttack1 = MMoveT "superTankMoveAttack1" frameAttack101 frameAttack106 superTankFramesAttack1 (Just superTankReAttack1)

superTankFramesEndAttack1 :: V.Vector MFrameT
superTankFramesEndAttack1 =
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
               ]

superTankMoveEndAttack1 :: MMoveT
superTankMoveEndAttack1 = MMoveT "superTankMoveEndAttack1" frameAttack107 frameAttack120 superTankFramesEndAttack1 (Just superTankRun)

superTankPain :: EntPain
superTankPain =
  GenericEntPain "supertank_pain" $ \_ _ _ _ -> do
    io (putStrLn "MSuperTank.superTankPain") >> undefined -- TODO

superTankDie :: EntDie
superTankDie =
  GenericEntDie "supertank_die" $ \selfRef@(EdictReference selfIdx) _ _ _ _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundDeath <- use $ mSuperTankGlobals.mSuperTankSoundDeath
    sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eDeadFlag .= Constants.deadDead
      eTakeDamage .= Constants.damageNo
      eCount .= 0
      eMonsterInfo.miCurrentMove .= Just superTankMoveDeath

{-
- QUAKED monster_supertank (1 .5 0) (-64 -64 0) (64 64 72) Ambush
- Trigger_Spawn Sight
-}
spMonsterSuperTank :: EntThink
spMonsterSuperTank =
  GenericEntThink "SP_monster_supertank" $ \_ -> do
    io (putStrLn "MSuperTank.spMonsterSuperTank") >> undefined -- TODO

bossExplode :: EntThink
bossExplode =
  GenericEntThink "BossExplode" $ \_ -> do
    io (putStrLn "MSuperTank.bossExplode") >> undefined -- TODO
