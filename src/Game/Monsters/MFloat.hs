{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MFloat where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=), (&), (.~), (%~))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.))
import Linear (V3(..))
import qualified Data.Vector as V

import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameCombat as GameCombat
import qualified Game.GameMisc as GameMisc
import qualified Game.GameWeapon as GameWeapon
import qualified Game.GameUtil as GameUtil
import qualified Game.Monster as Monster
import qualified Game.Monsters.MFlash as MFlash
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

modelScale :: Float
modelScale = 1.0

frameActivate01 :: Int
frameActivate01 = 0

frameActivate31 :: Int
frameActivate31 = 30

frameAttack101 :: Int
frameAttack101 = 31

frameAttack104 :: Int
frameAttack104 = 34

frameAttack107 :: Int
frameAttack107 = 37

frameAttack114 :: Int
frameAttack114 = 44

frameAttack201 :: Int
frameAttack201 = 45

frameAttack225 :: Int
frameAttack225 = 69

frameAttack301 :: Int
frameAttack301 = 70

frameAttack334 :: Int
frameAttack334 = 103

frameDeath01 :: Int
frameDeath01 = 104

frameDeath13 :: Int
frameDeath13 = 116

framePain101 :: Int
framePain101 = 117

framePain107 :: Int
framePain107 = 123

framePain201 :: Int
framePain201 = 124

framePain208 :: Int
framePain208 = 131

framePain301 :: Int
framePain301 = 132

framePain312 :: Int
framePain312 = 143

frameStand101 :: Int
frameStand101 = 144

frameStand152 :: Int
frameStand152 = 195

farmeStand201 :: Int
farmeStand201 = 196

frameStand252 :: Int
frameStand252 = 247

floaterSight :: EntInteract
floaterSight =
  GenericEntInteract "floater_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mFloatGlobals.mFloatSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

floaterIdle :: EntThink
floaterIdle =
  GenericEntThink "floater_idle" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mFloatGlobals.mFloatSoundIdle
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

floaterFireBlaster :: EntThink
floaterFireBlaster =
  GenericEntThink "floater_fire_blaster" $ \selfRef -> do
    self <- readRef selfRef

    let effect = if (self^.eEntityState.esFrame) == frameAttack104 || (self^.eEntityState.esFrame) == frameAttack107
                   then Constants.efHyperblaster
                   else 0
        (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2FloatBlaster1) forward right
        Just enemyRef = self^.eEnemy

    enemy <- readRef enemyRef

    let V3 a b c = enemy^.eEntityState.esOrigin
        end = V3 a b (c + fromIntegral (enemy^.eViewHeight))
        dir = end - start

    Monster.monsterFireBlaster selfRef start dir 1 1000 Constants.mz2FloatBlaster1 effect

    return True

floaterFramesStand1 :: V.Vector MFrameT
floaterFramesStand1 =
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
               ]

floaterMoveStand1 :: MMoveT
floaterMoveStand1 = MMoveT "floaterMoveStand1" frameStand101 frameStand152 floaterFramesStand1 Nothing

floaterFramesStand2 :: V.Vector MFrameT
floaterFramesStand2 =
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
               ]

floaterMoveStand2 :: MMoveT
floaterMoveStand2 = MMoveT "floaterMoveStand2" farmeStand201 frameStand252 floaterFramesStand2 Nothing

floaterStand :: EntThink
floaterStand =
  GenericEntThink "floater_stand" $ \selfRef -> do
    r <- Lib.randomF

    let action = if r <= 0.5
                   then floaterMoveStand1
                   else floaterMoveStand2

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

floaterFramesActivate :: V.Vector MFrameT
floaterFramesActivate =
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

floaterMoveActivate :: MMoveT
floaterMoveActivate = MMoveT "floaterMoveActivate" frameActivate01 frameActivate31 floaterFramesActivate Nothing

floaterRun :: EntThink
floaterRun =
  GenericEntThink "floater_run" $ \selfRef -> do
    self <- readRef selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then floaterMoveStand1
                   else floaterMoveRun

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

floaterFramesAttack1 :: V.Vector MFrameT
floaterFramesAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing -- Blaster attack
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterFireBlaster)
                 -- BOOM (0, -25.8, 32.5) -- LOOP Starts
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterFireBlaster)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               -- LOOP Ends
               ]

floaterMoveAttack1 :: MMoveT
floaterMoveAttack1 = MMoveT "floaterMoveAttack1" frameAttack101 frameAttack114 floaterFramesAttack1 (Just floaterRun)

floaterWham :: EntThink
floaterWham =
  GenericEntThink "floater_wham" $ \selfRef -> do
    soundAttack3 <- use $ mFloatGlobals.mFloatSoundAttack3
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanWeapon soundAttack3 1 Constants.attnNorm 0

    r <- Lib.rand
    let aim = V3 (fromIntegral Constants.meleeDistance) 0 0
    GameWeapon.fireHit selfRef aim (5 + fromIntegral (r `mod` 6)) (-50)

    return True

floaterFramesAttack2 :: V.Vector MFrameT
floaterFramesAttack2 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing -- Claws
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterWham)
                 -- WHAM (0, -45, 29.6) -- LOOP Starts
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
                 -- LOOP Ends
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

floaterMoveAttack2 :: MMoveT
floaterMoveAttack2 = MMoveT "floaterMoveAttack2" frameAttack201 frameAttack225 floaterFramesAttack2 (Just floaterRun)

floaterZap :: EntThink
floaterZap =
  GenericEntThink "floater_zap" $ \selfRef -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    let dir = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        -- FIXME: use a flash and replace these two lines with the
        -- commented one
        offset = V3 18.5 (-0.9) 10.0
        origin = Math3D.projectSource (self^.eEntityState.esOrigin) offset forward right
        -- origin = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let sound = gameImport^.giSound
        writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        writeDir = gameImport^.giWriteDir
        multicast = gameImport^.giMulticast

    soundAttack2 <- use $ mFloatGlobals.mFloatSoundAttack2
    sound (Just selfRef) Constants.chanWeapon soundAttack2 1 Constants.attnNorm 0

    -- FIXME: use the flash, Luke
    writeByte Constants.svcTempEntity
    writeByte Constants.teSplash
    writeByte 32
    writePosition origin
    writeDir dir
    writeByte 1 -- sparks
    multicast origin Constants.multicastPvs

    r <- Lib.rand
    v3o <- use $ globals.gVec3Origin
    GameCombat.damage enemyRef selfRef selfRef dir (enemy^.eEntityState.esOrigin) v3o (5 + fromIntegral (r `mod` 6)) (-10) Constants.damageEnergy Constants.modUnknown

    return True

floaterFramesAttack3 :: V.Vector MFrameT
floaterFramesAttack3 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just floaterZap)
                 -- LOOP Starts
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
                 -- LOOP Ends
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

floaterMoveAttack3 :: MMoveT
floaterMoveAttack3 = MMoveT "floaterMoveAttack3" frameAttack301 frameAttack334 floaterFramesAttack3 (Just floaterRun)

floaterFramesDeath :: V.Vector MFrameT
floaterFramesDeath =
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

floaterDead :: EntThink
floaterDead =
  GenericEntThink "floater_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

floaterMoveDeath :: MMoveT
floaterMoveDeath = MMoveT "floaterMoveDeath" frameDeath01 frameDeath13 floaterFramesDeath (Just floaterDead)

floaterFramesPain1 :: V.Vector MFrameT
floaterFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

floaterMovePain1 :: MMoveT
floaterMovePain1 = MMoveT "floaterMovePain1" framePain101 framePain107 floaterFramesPain1 (Just floaterRun)

floaterFramesPain2 :: V.Vector MFrameT
floaterFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

floaterMovePain2 :: MMoveT
floaterMovePain2 = MMoveT "floaterMovePain2" framePain201 framePain208 floaterFramesPain2 (Just floaterRun)

floaterFramesPain3 :: V.Vector MFrameT
floaterFramesPain3 =
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
               ]

floaterMovePain3 :: MMoveT
floaterMovePain3 = MMoveT "floaterMovePain3" framePain301 framePain312 floaterFramesPain3 (Just floaterRun)

floaterFramesWalk :: V.Vector MFrameT
floaterFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               , MFrameT (Just GameAI.aiWalk) 5 Nothing
               ]

floaterMoveWalk :: MMoveT
floaterMoveWalk = MMoveT "floaterMoveWalk" frameStand101 frameStand152 floaterFramesWalk Nothing

floaterFramesRun :: V.Vector MFrameT
floaterFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               ]

floaterMoveRun :: MMoveT
floaterMoveRun = MMoveT "floaterMoveRun" frameStand101 frameStand152 floaterFramesRun Nothing

floaterWalk :: EntThink
floaterWalk =
  GenericEntThink "floater_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just floaterMoveWalk)
    return True

floaterAttack :: EntThink
floaterAttack =
  GenericEntThink "floater_attack" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just floaterMoveAttack1)
    return True

floaterMelee :: EntThink
floaterMelee =
  GenericEntThink "floater_melee" $ \selfRef -> do
    r <- Lib.randomF

    let action = if r < 0.5
                   then floaterMoveAttack3
                   else floaterMoveAttack2

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

floaterPain :: EntPain
floaterPain =
  GenericEntPain "floater_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        n <- Lib.rand
        
        (soundPain, currentMove) <- if (n + 1) `mod` 3 == 0
                                      then do
                                        soundPain <- use $ mFloatGlobals.mFloatSoundPain1
                                        return (soundPain, floaterMovePain1)
                                      else do
                                        soundPain <- use $ mFloatGlobals.mFloatSoundPain2
                                        return (soundPain, floaterMovePain2)

        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

floaterDie :: EntDie
floaterDie =
  GenericEntDie "floater_die" $ \selfRef _ _ _ _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundDeath1 <- use $ mFloatGlobals.mFloatSoundDeath1
    sound (Just selfRef) Constants.chanVoice soundDeath1 1 Constants.attnNorm 0
    GameMisc.becomeExplosion1 selfRef

{-
- QUAKED monster_floater (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterFloater :: Ref EdictT -> Quake ()
spMonsterFloater selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex (Just "floater/fltatck2.wav") >>= (mFloatGlobals.mFloatSoundAttack2 .=)
        soundIndex (Just "floater/fltatck3.wav") >>= (mFloatGlobals.mFloatSoundAttack3 .=)
        soundIndex (Just "floater/fltdeth1.wav") >>= (mFloatGlobals.mFloatSoundDeath1 .=)
        soundIndex (Just "floater/fltidle1.wav") >>= (mFloatGlobals.mFloatSoundIdle .=)
        soundIndex (Just "floater/fltpain1.wav") >>= (mFloatGlobals.mFloatSoundPain1 .=)
        soundIndex (Just "floater/fltpain2.wav") >>= (mFloatGlobals.mFloatSoundPain2 .=)
        soundIndex (Just "floater/fltsght1.wav") >>= (mFloatGlobals.mFloatSoundSight .=)

        void $ soundIndex (Just "floater/fltatck1.wav")

        soundIdx <- soundIndex (Just "floater/fltsrch1.wav")
        modelIdx <- modelIndex (Just "models/monsters/float/tris.md2")

        modifyRef selfRef (\v -> v & eEntityState.esSound .~ soundIdx
                                      & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-24) (-24) (-24)
                                      & eMaxs .~ V3 24 24 32
                                      & eHealth .~ 200
                                      & eGibHealth .~ (-80)
                                      & eMass .~ 300
                                      & ePain .~ Just floaterPain
                                      & eDie .~ Just floaterDie
                                      & eMonsterInfo.miStand .~ Just floaterStand
                                      & eMonsterInfo.miWalk .~ Just floaterWalk
                                      & eMonsterInfo.miRun .~ Just floaterRun
                                      & eMonsterInfo.miAttack .~ Just floaterAttack
                                      & eMonsterInfo.miMelee .~ Just floaterMelee
                                      & eMonsterInfo.miSight .~ Just floaterSight
                                      & eMonsterInfo.miIdle .~ Just floaterIdle)

        linkEntity selfRef

        r <- Lib.randomF

        let currentMove = if r <= 0.5
                            then floaterMoveStand1
                            else floaterMoveStand2

        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove
                                      & eMonsterInfo.miScale .~ modelScale)

        void $ think GameAI.flyMonsterStart selfRef
