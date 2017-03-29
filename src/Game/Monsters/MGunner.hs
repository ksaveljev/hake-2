{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MGunner where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (+=), (-=), (&), (.~), (%~), (+~), (-~))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing)
import Linear (V3(..), _z, normalize)
import qualified Data.Vector as V

import Types
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

modelScale :: Float
modelScale = 1.15

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

frameAttack105 :: Int
frameAttack105 = 112

frameAttack108 :: Int
frameAttack108 = 115

frameAttack111 :: Int
frameAttack111 = 118

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
  GenericEntThink "gunner_stand" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveStand)
    return True

gunnerMoveFidget :: MMoveT
gunnerMoveFidget = MMoveT "gunnerMoveFidget" frameStand31 frameStand70 gunnerFramesFidget (Just gunnerStand)

gunnerFidget :: EntThink
gunnerFidget =
  GenericEntThink "gunner_fidget" $ \selfRef -> do
    self <- readEdictT selfRef

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
      then
        return True

      else do
        r <- Lib.randomF

        when (r <= 0.05) $
          modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveFidget)

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
  GenericEntThink "gunner_walk" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveWalk)
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
  GenericEntThink "gunner_run" $ \selfRef -> do
    self <- readEdictT selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then gunnerMoveStand
                   else gunnerMoveRun

    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
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
  GenericEntThink "gunner_runandshoot" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveRunAndShoot)
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
  GenericEntPain "gunner_pain" $ \selfRef _ _ damage -> do
    self <- readEdictT selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyEdictT selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyEdictT selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

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

        modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

gunnerDead :: EntThink
gunnerDead =
  GenericEntThink "gunner_dead" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

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
  GenericEntDie "gunner_die" $ \selfRef _ _ damage _ -> do
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    if | (self^.eHealth) <= (self^.eGibHealth) -> do -- check for gib
           soundIdx <- soundIndex (Just "misc/udeath.wav")
           sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
           
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

           GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic

           modifyEdictT selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)

       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()

       | otherwise -> do -- regular death
           soundDeath <- use $ mGunnerGlobals.mGunnerSoundDeath
           sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0

           modifyEdictT selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         & eMonsterInfo.miCurrentMove .~ Just gunnerMoveDeath)

gunnerDuckDown :: EntThink
gunnerDuckDown =
  GenericEntThink "gunner_duck_down" $ \selfRef -> do
    self <- readEdictT selfRef

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked /= 0
      then
        return True

      else do
        modifyEdictT selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked))

        skillValue <- liftM (^.cvValue) skillCVar

        when (skillValue >= 2) $ do
          r <- Lib.randomF
          when (r > 0.5) $
            void $ think gunnerGrenade selfRef

        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT selfRef (\v -> v & eMaxs._z -~ 32
                                      & eTakeDamage .~ Constants.damageYes
                                      & eMonsterInfo.miPauseTime .~ levelTime + 1)

        linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
        linkEntity selfRef

        return True

gunnerDuckHold :: EntThink
gunnerDuckHold =
  GenericEntThink "gunner_duck_hold" $ \selfRef -> do
    self <- readEdictT selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if levelTime >= (self^.eMonsterInfo.miPauseTime)
      then modifyEdictT selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiHoldFrame)))
      else modifyEdictT selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

    return True

gunnerDuckUp :: EntThink
gunnerDuckUp =
  GenericEntThink "gunner_duck_up" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiDucked))
                                  & eMaxs._z +~ 32
                                  & eTakeDamage .~ Constants.damageAim)

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
  GenericEntDodge "gunner_dodge" $ \selfRef attackerRef _ -> do
    r <- Lib.randomF

    unless (r > 0.25) $ do
      self <- readEdictT selfRef

      when (isNothing (self^.eEnemy)) $
        modifyEdictT selfRef (\v -> v & eEnemy .~ Just attackerRef)

      modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveDuck)

gunnerOpenGun :: EntThink
gunnerOpenGun =
  GenericEntThink "gunner_opengun" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundOpen <- use $ mGunnerGlobals.mGunnerSoundOpen
    sound (Just selfRef) Constants.chanVoice soundOpen 1 Constants.attnIdle 0
    return True

gunnerFire :: EntThink
gunnerFire =
  GenericEntThink "GunnerFire" $ \selfRef -> do
    self <- readEdictT selfRef

    let flashNumber = Constants.mz2GunnerMachinegun1 + (self^.eEntityState.esFrame) - frameAttack216
        (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
        Just enemyRef = self^.eEnemy

    enemy <- readEdictT enemyRef

    let V3 a b c = (enemy^.eEntityState.esOrigin) + fmap (* (-0.2)) (enemy^.eVelocity)
        target = V3 a b (c + fromIntegral (enemy^.eViewHeight))
        aim = normalize (target - start)

    Monster.monsterFireBullet selfRef start aim 3 4 Constants.defaultBulletHspread Constants.defaultBulletVspread flashNumber

    return True

gunnerGrenade :: EntThink
gunnerGrenade =
  GenericEntThink "GunnerGrenade" $ \selfRef -> do
    self <- readEdictT selfRef

    let flashNumber = if | (self^.eEntityState.esFrame) == frameAttack105 -> Constants.mz2GunnerGrenade1
                         | (self^.eEntityState.esFrame) == frameAttack108 -> Constants.mz2GunnerGrenade2
                         | (self^.eEntityState.esFrame) == frameAttack111 -> Constants.mz2GunnerGrenade3
                         | otherwise -> Constants.mz2GunnerGrenade4

    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
        -- FIXME: do a spread -225 -75 75 255 degrees around forward
        aim = forward

    Monster.monsterFireGrenade selfRef start aim 50 600 flashNumber

    return True

gunnerAttack :: EntThink
gunnerAttack =
  GenericEntThink "gunner_attack" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef
    r <- Lib.randomF

    let currentMove = if GameUtil.range self enemy == Constants.rangeMelee
                        then gunnerMoveAttackChain
                        else if r <= 0.5
                               then gunnerMoveAttackGrenade
                               else gunnerMoveAttackChain

    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
    return True

gunnerFireChain :: EntThink
gunnerFireChain =
  GenericEntThink "gunner_fire_chain" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveFireChain)
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
  GenericEntThink "gunner_refire_chain" $ \selfRef -> do
    self <- readEdictT selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readEdictT enemyRef

    currentMove <- if (enemy^.eHealth) > 0
                     then do
                       vis <- GameUtil.visible selfRef enemyRef
                       r <- Lib.randomF

                       return $ if vis && r <= 0.5
                                  then gunnerMoveFireChain
                                  else gunnerMoveEndFireChain
                     else
                       return gunnerMoveEndFireChain

    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
    return True

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
spMonsterGunner selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex (Just "gunner/death1.wav") >>= (mGunnerGlobals.mGunnerSoundDeath .=)
        soundIndex (Just "gunner/gunpain2.wav") >>= (mGunnerGlobals.mGunnerSoundPain .=)
        soundIndex (Just "gunner/gunpain1.wav") >>= (mGunnerGlobals.mGunnerSoundPain2 .=)
        soundIndex (Just "gunner/gunidle1.wav") >>= (mGunnerGlobals.mGunnerSoundIdle .=)
        soundIndex (Just "gunner/gunatck1.wav") >>= (mGunnerGlobals.mGunnerSoundOpen .=)
        soundIndex (Just "gunner/gunsrch1.wav") >>= (mGunnerGlobals.mGunnerSoundSearch .=)
        soundIndex (Just "gunner/sight1.wav") >>= (mGunnerGlobals.mGunnerSoundSight .=)

        void $ soundIndex (Just "gunner/gunatck2.wav")
        void $ soundIndex (Just "gunner/gunatck3.wav")

        modelIdx <- modelIndex (Just "models/monsters/gunner/tris.md2")

        modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-16) (-16) (-24)
                                      & eMaxs .~ V3 16 16 32
                                      & eHealth .~ 175
                                      & eGibHealth .~ (-70)
                                      & eMass .~ 200
                                      & ePain .~ Just gunnerPain
                                      & eDie .~ Just gunnerDie
                                      & eMonsterInfo.miStand .~ Just gunnerStand
                                      & eMonsterInfo.miWalk .~ Just gunnerWalk
                                      & eMonsterInfo.miRun .~ Just gunnerRun
                                      & eMonsterInfo.miDodge .~ Just gunnerDodge
                                      & eMonsterInfo.miAttack .~ Just gunnerAttack
                                      & eMonsterInfo.miMelee .~ Nothing
                                      & eMonsterInfo.miSight .~ Just gunnerSight
                                      & eMonsterInfo.miSearch .~ Just gunnerSearch)

        linkEntity selfRef

        modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gunnerMoveStand
                                      & eMonsterInfo.miScale .~ modelScale)

        void $ think GameAI.walkMonsterStart selfRef
