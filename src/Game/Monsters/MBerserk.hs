{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MBerserk where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (&), (.~), (%~))
import Control.Monad (void, unless, liftM, when)
import Data.Bits ((.&.), (.|.))
import Linear (V3(..), _x)
import qualified Data.Vector as V

import {-# SOURCE #-} Game.GameImportT
import Game.LevelLocalsT
import Game.GameLocalsT
import Game.CVarT
import Game.SpawnTempT
import Game.EntityStateT
import Game.EdictT
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
import qualified Game.GameMisc as GameMisc
import qualified Game.GameWeapon as GameWeapon
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

modelScale :: Float
modelScale = 1.0

frameStand1 :: Int
frameStand1 = 0

frameStand5 :: Int
frameStand5 = 4

frameStandB1 :: Int
frameStandB1 = 5

frameStandB20 :: Int
frameStandB20 = 24

frameWalkC1 :: Int
frameWalkC1 = 25

frameWalkC11 :: Int
frameWalkC11 = 35

frameRun1 :: Int
frameRun1 = 36

frameRun6 :: Int
frameRun6 = 41

frameAttC1 :: Int
frameAttC1 = 76

frameAttC8 :: Int
frameAttC8 = 83

frameAttC9 :: Int
frameAttC9 = 84

frameAttC20 :: Int
frameAttC20 = 95

frameAttC21 :: Int
frameAttC21 = 96

frameAttC34 :: Int
frameAttC34 = 109

framePainC1 :: Int
framePainC1 = 199

framePainC4 :: Int
framePainC4 = 202

framePainB1 :: Int
framePainB1 = 203

framePainB20 :: Int
framePainB20 = 222

frameDeath1 :: Int
frameDeath1 = 223

frameDeath13 :: Int
frameDeath13 = 235

frameDeathC1 :: Int
frameDeathC1 = 236

frameDeathC8 :: Int
frameDeathC8 = 243

berserkSight :: EntInteract
berserkSight =
  GenericEntInteract "berserk_sight" $ \selfRef _ -> do
    soundSight <- use $ mBerserkGlobals.mBerserkSoundSight
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

berserkSearch :: EntThink
berserkSearch =
  GenericEntThink "berserk_search" $ \selfRef -> do
    soundSearch <- use $ mBerserkGlobals.mBerserkSoundSearch
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

berserkFidget :: EntThink
berserkFidget =
  GenericEntThink "berserk_fidget" $ \selfRef -> do
    self <- readRef selfRef

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
      then
        return True

      else do
        r <- Lib.randomF

        if r > 0.15
          then
            return True

          else do
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just berserkMoveStandFidget)
            soundIdle <- use $ mBerserkGlobals.mBerserkSoundIdle
            sound <- use $ gameBaseGlobals.gbGameImport.giSound
            sound (Just selfRef) Constants.chanWeapon soundIdle 1 Constants.attnIdle 0
            return True

berserkFramesStand :: V.Vector MFrameT
berserkFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 (Just berserkFidget)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

berserkMoveStand :: MMoveT
berserkMoveStand = MMoveT "berserkMoveStand" frameStand1 frameStand5 berserkFramesStand Nothing

berserkStand :: EntThink
berserkStand =
  GenericEntThink "berserk_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just berserkMoveStand)
    return True

berserkFramesStandFidget :: V.Vector MFrameT
berserkFramesStandFidget =
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
               ]

berserkMoveStandFidget :: MMoveT
berserkMoveStandFidget = MMoveT "berserkMoveStandFidget" frameStandB1 frameStandB20 berserkFramesStandFidget (Just berserkStand)

berserkFramesWalk :: V.Vector MFrameT
berserkFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 9.1 Nothing
               , MFrameT (Just GameAI.aiWalk) 6.3 Nothing
               , MFrameT (Just GameAI.aiWalk) 4.9 Nothing
               , MFrameT (Just GameAI.aiWalk) 6.7 Nothing
               , MFrameT (Just GameAI.aiWalk) 6.0 Nothing
               , MFrameT (Just GameAI.aiWalk) 8.2 Nothing
               , MFrameT (Just GameAI.aiWalk) 7.2 Nothing
               , MFrameT (Just GameAI.aiWalk) 6.1 Nothing
               , MFrameT (Just GameAI.aiWalk) 4.9 Nothing
               , MFrameT (Just GameAI.aiWalk) 4.7 Nothing
               , MFrameT (Just GameAI.aiWalk) 4.7 Nothing
               , MFrameT (Just GameAI.aiWalk) 4.8 Nothing
               ]

berserkMoveWalk :: MMoveT
berserkMoveWalk = MMoveT "berserkMoveWalk" frameWalkC1 frameWalkC11 berserkFramesWalk Nothing

berserkWalk :: EntThink
berserkWalk =
  GenericEntThink "berserk_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just berserkMoveWalk)
    return True

{-
- 
- **************************** SKIPPED THIS FOR NOW!
- ****************************
- 
- Running . Arm raised in air
- 
- void() berserk_runb1 =[ $r_att1 , berserk_runb2 ] {ai_run(21);}; void()
- berserk_runb2 =[ $r_att2 , berserk_runb3 ] {ai_run(11);}; void()
- berserk_runb3 =[ $r_att3 , berserk_runb4 ] {ai_run(21);}; void()
- berserk_runb4 =[ $r_att4 , berserk_runb5 ] {ai_run(25);}; void()
- berserk_runb5 =[ $r_att5 , berserk_runb6 ] {ai_run(18);}; void()
- berserk_runb6 =[ $r_att6 , berserk_runb7 ] {ai_run(19);}; // running with
- arm in air : start loop void() berserk_runb7 =[ $r_att7 , berserk_runb8 ]
- {ai_run(21);}; void() berserk_runb8 =[ $r_att8 , berserk_runb9 ]
- {ai_run(11);}; void() berserk_runb9 =[ $r_att9 , berserk_runb10 ]
- {ai_run(21);}; void() berserk_runb10 =[ $r_att10 , berserk_runb11 ]
- {ai_run(25);}; void() berserk_runb11 =[ $r_att11 , berserk_runb12 ]
- {ai_run(18);}; void() berserk_runb12 =[ $r_att12 , berserk_runb7 ]
- {ai_run(19);}; // running with arm in air : end loop
-}

berserkFramesRun1 :: V.Vector MFrameT
berserkFramesRun1 =
    V.fromList [ MFrameT (Just GameAI.aiRun) 21 Nothing
               , MFrameT (Just GameAI.aiRun) 11 Nothing
               , MFrameT (Just GameAI.aiRun) 21 Nothing
               , MFrameT (Just GameAI.aiRun) 25 Nothing
               , MFrameT (Just GameAI.aiRun) 18 Nothing
               , MFrameT (Just GameAI.aiRun) 19 Nothing
               ]

berserkMoveRun1 :: MMoveT
berserkMoveRun1 = MMoveT "berserkMoveRun1" frameRun1 frameRun6 berserkFramesRun1 Nothing

berserkRun :: EntThink
berserkRun =
  GenericEntThink "berserk_run" $ \selfRef -> do
    self <- readRef selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then berserkMoveStand
                   else berserkMoveRun1

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

berserkAttackSpike :: EntThink
berserkAttackSpike =
  GenericEntThink "berserk_attack_spike" $ \selfRef -> do
    r <- Lib.rand
    let n = r `mod` 6 + 15
        aim = V3 (fromIntegral Constants.meleeDistance) 0 (-24)
    void $ GameWeapon.fireHit selfRef aim (fromIntegral n) 400
    -- Faster attack -- upwards and backwards
    return True

berserkSwing :: EntThink
berserkSwing =
  GenericEntThink "berserk_swing" $ \selfRef -> do
    soundPunch <- use $ mBerserkGlobals.mBerserkSoundPunch
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanWeapon soundPunch 1 Constants.attnNorm 0
    return True

berserkFramesAttackSpike :: V.Vector MFrameT
berserkFramesAttackSpike =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just berserkSwing)
               , MFrameT (Just GameAI.aiCharge) 0 (Just berserkAttackSpike)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

berserkMoveAttackSpike :: MMoveT
berserkMoveAttackSpike = MMoveT "berserkMoveAttackSpike" frameAttC1 frameAttC8 berserkFramesAttackSpike (Just berserkRun)

berserkAttackClub :: EntThink
berserkAttackClub =
  GenericEntThink "berserk_attack_club" $ \selfRef -> do
    self <- readRef selfRef

    r <- Lib.rand
    let n = r `mod` 6 + 5
        aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) (-4)

    void $ GameWeapon.fireHit selfRef aim (fromIntegral n) 400 -- slower attack
    return True

berserkFramesAttackClub :: V.Vector MFrameT
berserkFramesAttackClub =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just berserkSwing)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just berserkAttackClub)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

berserkMoveAttackClub :: MMoveT
berserkMoveAttackClub = MMoveT "berserkMoveAttackClub" frameAttC9 frameAttC20 berserkFramesAttackClub (Just berserkRun)

berserkStrike :: EntThink
berserkStrike =
  GenericEntThink "berserk_strike" $ \_ -> return True

berserkFramesAttackStrike :: V.Vector MFrameT
berserkFramesAttackStrike =
    V.fromList [ MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 (Just berserkSwing)
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 (Just berserkStrike)
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               , MFrameT (Just GameAI.aiMove)  9.7 Nothing
               , MFrameT (Just GameAI.aiMove) 13.6 Nothing
               ]

berserkMoveAttackStrike :: MMoveT
berserkMoveAttackStrike = MMoveT "berserkMoveAttackStrike" frameAttC21 frameAttC34 berserkFramesAttackStrike (Just berserkRun)

berserkMelee :: EntThink
berserkMelee =
  GenericEntThink "berserk_melee" $ \selfRef -> do
    r <- Lib.rand

    let action = if r .&. 1 == 0
                   then berserkMoveAttackSpike
                   else berserkMoveAttackClub

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

{-
- void() berserk_atke1 =[ $r_attb1, berserk_atke2 ] {ai_run(9);}; void()
- berserk_atke2 =[ $r_attb2, berserk_atke3 ] {ai_run(6);}; void()
- berserk_atke3 =[ $r_attb3, berserk_atke4 ] {ai_run(18.4);}; void()
- berserk_atke4 =[ $r_attb4, berserk_atke5 ] {ai_run(25);}; void()
- berserk_atke5 =[ $r_attb5, berserk_atke6 ] {ai_run(14);}; void()
- berserk_atke6 =[ $r_attb6, berserk_atke7 ] {ai_run(20);}; void()
- berserk_atke7 =[ $r_attb7, berserk_atke8 ] {ai_run(8.5);}; void()
- berserk_atke8 =[ $r_attb8, berserk_atke9 ] {ai_run(3);}; void()
- berserk_atke9 =[ $r_attb9, berserk_atke10 ] {ai_run(17.5);}; void()
- berserk_atke10 =[ $r_attb10, berserk_atke11 ] {ai_run(17);}; void()
- berserk_atke11 =[ $r_attb11, berserk_atke12 ] {ai_run(9);}; void()
- berserk_atke12 =[ $r_attb12, berserk_atke13 ] {ai_run(25);}; void()
- berserk_atke13 =[ $r_attb13, berserk_atke14 ] {ai_run(3.7);}; void()
- berserk_atke14 =[ $r_attb14, berserk_atke15 ] {ai_run(2.6);}; void()
- berserk_atke15 =[ $r_attb15, berserk_atke16 ] {ai_run(19);}; void()
- berserk_atke16 =[ $r_attb16, berserk_atke17 ] {ai_run(25);}; void()
- berserk_atke17 =[ $r_attb17, berserk_atke18 ] {ai_run(19.6);}; void()
- berserk_atke18 =[ $r_attb18, berserk_run1 ] {ai_run(7.8);};
-}

berserkFramesPain1 :: V.Vector MFrameT
berserkFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

berserkMovePain1 :: MMoveT
berserkMovePain1 = MMoveT "berserkMovePain1" framePainC1 framePainC4 berserkFramesPain1 (Just berserkRun)

berserkFramesPain2 :: V.Vector MFrameT
berserkFramesPain2 =
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
               ]

berserkMovePain2 :: MMoveT
berserkMovePain2 = MMoveT "berserkMovePain2" framePainB1 framePainB20 berserkFramesPain2 (Just berserkRun)

berserkPain :: EntPain
berserkPain =
  GenericEntPain "berserk_pain" $ \selfRef _ _ damage -> do
    self <- readRef selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

      sound <- use $ gameBaseGlobals.gbGameImport.giSound
      soundPain <- use $ mBerserkGlobals.mBerserkSoundPain
      sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        r <- Lib.randomF

        let currentMove = if damage < 20 || r < 0.5
                            then berserkMovePain1
                            else berserkMovePain2

        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

berserkDead :: EntThink
berserkDead =
  GenericEntThink "berserk_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

berserkFramesDeath1 :: V.Vector MFrameT
berserkFramesDeath1 =
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

berserkMoveDeath1 :: MMoveT
berserkMoveDeath1 = MMoveT "berserkMoveDeath1" frameDeath1 frameDeath13 berserkFramesDeath1 (Just berserkDead)

berserkFramesDeath2 :: V.Vector MFrameT
berserkFramesDeath2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

berserkMoveDeath2 :: MMoveT
berserkMoveDeath2 = MMoveT "berserkMoveDeath2" frameDeathC1 frameDeathC8 berserkFramesDeath2 (Just berserkDead)

berserkDie :: EntDie
berserkDie =
  GenericEntDie "berserk_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex

    if | (self^.eHealth) < (self^.eGibHealth) -> do
           soundIdx <- soundIndex (Just "misc/udeath.wav")
           sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic

           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

           GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)

       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()

       | otherwise -> do
           soundDie <- use $ mBerserkGlobals.mBerserkSoundDie
           sound (Just selfRef) Constants.chanVoice soundDie 1 Constants.attnNorm 0

           let currentMove = if damage >= 50
                               then berserkMoveDeath1
                               else berserkMoveDeath2

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         & eMonsterInfo.miCurrentMove .~ Just currentMove)

{-
- QUAKED monster_berserk (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterBerserk :: Ref EdictT -> Quake ()
spMonsterBerserk selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex (Just "berserk/berpain2.wav") >>= (mBerserkGlobals.mBerserkSoundPain .=)
        soundIndex (Just "berserk/berdeth2.wav") >>= (mBerserkGlobals.mBerserkSoundDie .=)
        soundIndex (Just "berserk/beridle1.wav") >>= (mBerserkGlobals.mBerserkSoundIdle .=)
        soundIndex (Just "berserk/attack.wav") >>= (mBerserkGlobals.mBerserkSoundPunch .=)
        soundIndex (Just "berserk/bersrch1.wav") >>= (mBerserkGlobals.mBerserkSoundSearch .=)
        soundIndex (Just "berserk/sight.wav") >>= (mBerserkGlobals.mBerserkSoundSight .=)

        modelIdx <- modelIndex (Just "models/monsters/berserk/tris.md2")

        modifyRef selfRef (\v -> v & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-16) (-16) (-24)
                                      & eMaxs .~ V3 16 16 32
                                      & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eHealth .~ 240
                                      & eGibHealth .~ (-60)
                                      & eMass .~ 250
                                      & ePain .~ Just berserkPain
                                      & eDie .~ Just berserkDie
                                      & eMonsterInfo.miStand .~ Just berserkStand
                                      & eMonsterInfo.miWalk .~ Just berserkWalk
                                      & eMonsterInfo.miRun .~ Just berserkRun
                                      & eMonsterInfo.miDodge .~ Nothing
                                      & eMonsterInfo.miAttack .~ Nothing
                                      & eMonsterInfo.miMelee .~ Just berserkMelee
                                      & eMonsterInfo.miSight .~ Just berserkSight
                                      & eMonsterInfo.miSearch .~ Just berserkSearch
                                      & eMonsterInfo.miCurrentMove .~ Just berserkMoveStand
                                      & eMonsterInfo.miScale .~ modelScale)

        linkEntity selfRef

        void $ think GameAI.walkMonsterStart selfRef
