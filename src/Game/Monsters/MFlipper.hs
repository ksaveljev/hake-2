{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MFlipper where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=), (&), (.~), (%~))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.|.))
import Linear (V3(..))
import qualified Data.Vector as V

import Quake
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

frameFlpbit01 :: Int
frameFlpbit01 = 0

frameFlpbit20 :: Int
frameFlpbit20 = 19

frameFlphor01 :: Int
frameFlphor01 = 41

frameFlphor05 :: Int
frameFlphor05 = 45

frameFlphor24 :: Int
frameFlphor24 = 64

frameFlpver01 :: Int
frameFlpver01 = 65

frameFlpver06 :: Int
frameFlpver06 = 70

frameFlpver29 :: Int
frameFlpver29 = 93

frameFlppn101 :: Int
frameFlppn101 = 94

frameFlppn105 :: Int
frameFlppn105 = 98

frameFlppn201 :: Int
frameFlppn201 = 99

frameFlppn205 :: Int
frameFlppn205 = 103

frameFlpdth01 :: Int
frameFlpdth01 = 104

frameFlpdth56 :: Int
frameFlpdth56 = 159

flipperRunSpeed :: Float
flipperRunSpeed = 24

flipperFramesStand :: V.Vector MFrameT
flipperFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing ]

flipperMoveStand :: MMoveT
flipperMoveStand = MMoveT "flipperMoveStand" frameFlphor01 frameFlphor01 flipperFramesStand Nothing

flipperStand :: EntThink
flipperStand =
  GenericEntThink "flipper_stand" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveStand)
    return True

flipperFramesRun :: V.Vector MFrameT
flipperFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing -- 6
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
                 -- 10

               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
                 -- 20

               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing
               , MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing -- 29
               ]

flipperMoveRunLoop :: MMoveT
flipperMoveRunLoop = MMoveT "flipperMoveRunLoop" frameFlpver06 frameFlpver29 flipperFramesRun Nothing

flipperRunLoop :: EntThink
flipperRunLoop =
  GenericEntThink "flipper_run_loop" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveRunLoop)
    return True

flipperFramesRunStart :: V.Vector MFrameT
flipperFramesRunStart =
    V.fromList [ MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               ]

flipperMoveRunStart :: MMoveT
flipperMoveRunStart = MMoveT "flipperMoveRunStart" frameFlpver01 frameFlpver06 flipperFramesRunStart (Just flipperRunLoop)

flipperRun :: EntThink
flipperRun =
  GenericEntThink "flipper_run" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveRunStart)
    return True

-- Standard Swimming
flipperFramesWalk :: V.Vector MFrameT
flipperFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 4 Nothing
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
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               , MFrameT (Just GameAI.aiWalk) 4 Nothing
               ]

flipperMoveWalk :: MMoveT
flipperMoveWalk = MMoveT "flipperMoveWalk" frameFlphor01 frameFlphor24 flipperFramesWalk Nothing

flipperWalk :: EntThink
flipperWalk =
  GenericEntThink "flipper_walk" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveWalk)
    return True

flipperFramesStartRun :: V.Vector MFrameT
flipperFramesStartRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 (Just flipperRun)
               ]

flipperMoveStartRun :: MMoveT
flipperMoveStartRun = MMoveT "flipperMoveStartRun" frameFlphor01 frameFlphor05 flipperFramesStartRun Nothing

flipperStartRun :: EntThink
flipperStartRun =
  GenericEntThink "flipper_start_run" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveStartRun)
    return True

flipperFramesPain2 :: V.Vector MFrameT
flipperFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flipperMovePain2 :: MMoveT
flipperMovePain2 = MMoveT "flipperMovePain2" frameFlppn101 frameFlppn105 flipperFramesPain2 (Just flipperRun)

flipperFramesPain1 :: V.Vector MFrameT
flipperFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

flipperMovePain1 :: MMoveT
flipperMovePain1 = MMoveT "flipperMovePain1" frameFlppn201 frameFlppn205 flipperFramesPain1 (Just flipperRun)

flipperBite :: EntThink
flipperBite =
  GenericEntThink "flipper_bite" $ \selfRef -> do
    let aim = V3 (fromIntegral Constants.meleeDistance) 0 0
    GameWeapon.fireHit selfRef aim 5 0
    return True

flipperPreAttack :: EntThink
flipperPreAttack =
  GenericEntThink "flipper_preattack" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundChomp <- use $ mFlipperGlobals.mFlipperSoundChomp
    sound (Just selfRef) Constants.chanWeapon soundChomp 1 Constants.attnNorm 0
    return True

flipperFramesAttack :: V.Vector MFrameT
flipperFramesAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just flipperPreAttack)
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
               , MFrameT (Just GameAI.aiCharge) 0 (Just flipperBite)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just flipperBite)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

flipperMoveAttack :: MMoveT
flipperMoveAttack = MMoveT "flipperMoveAttack" frameFlpbit01 frameFlpbit20 flipperFramesAttack (Just flipperRun)

flipperMelee :: EntThink
flipperMelee =
  GenericEntThink "flipper_melee" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveAttack)
    return True

flipperPain :: EntPain
flipperPain =
  GenericEntPain "flipper_pain" $ \selfRef _ _ _ -> do
    self <- readEdictT selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyEdictT selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyEdictT selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        n <- Lib.rand
        sound <- use $ gameBaseGlobals.gbGameImport.giSound

        (soundPain, currentMove) <- if (n + 1) `mod` 2 == 0
                                      then do
                                        soundPain <- use $ mFlipperGlobals.mFlipperSoundPain1
                                        return (soundPain, flipperMovePain1)
                                      else do
                                        soundPain <- use $ mFlipperGlobals.mFlipperSoundPain2
                                        return (soundPain, flipperMovePain2)

        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
        modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

flipperDead :: EntThink
flipperDead =
  GenericEntThink "flipper_dead" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

flipperFramesDeath :: V.Vector MFrameT
flipperFramesDeath =
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

flipperMoveDeath :: MMoveT
flipperMoveDeath = MMoveT "flipperMoveDeath" frameFlpdth01 frameFlpdth56 flipperFramesDeath (Just flipperDead)

flipperSight :: EntInteract
flipperSight =
  GenericEntInteract "flipper_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mFlipperGlobals.mFlipperSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

flipperDie :: EntDie
flipperDie =
  GenericEntDie "flipper_die" $ \selfRef _ _ damage _ -> do
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex

    if | (self^.eHealth) <= (self^.eGibHealth) -> do -- check for gib
           soundIdx <- soundIndex (Just "misc/udeath.wav")
           sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic

           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

           GameMisc.throwHead selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

           modifyEdictT selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)

       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()

       | otherwise -> do -- regular death
           soundDeath <- use $ mFlipperGlobals.mFlipperSoundDeath
           sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0

           modifyEdictT selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         & eMonsterInfo.miCurrentMove .~ Just flipperMoveDeath)

{-
- QUAKED monster_flipper (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterFlipper :: EdictReference -> Quake ()
spMonsterFlipper selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex (Just "flipper/flppain1.wav") >>= (mFlipperGlobals.mFlipperSoundPain1 .=)
        soundIndex (Just "flipper/flppain2.wav") >>= (mFlipperGlobals.mFlipperSoundPain2 .=)
        soundIndex (Just "flipper/flpdeth1.wav") >>= (mFlipperGlobals.mFlipperSoundDeath .=)
        soundIndex (Just "flipper/flpatck1.wav") >>= (mFlipperGlobals.mFlipperSoundChomp .=)
        soundIndex (Just "flipper/flpatck2.wav") >>= (mFlipperGlobals.mFlipperSoundAttack .=)
        soundIndex (Just "flipper/flpidle1.wav") >>= (mFlipperGlobals.mFlipperSoundIdle .=)
        soundIndex (Just "flipper/flpsrch1.wav") >>= (mFlipperGlobals.mFlipperSoundSearch .=)
        soundIndex (Just "flipper/flpsght1.wav") >>= (mFlipperGlobals.mFlipperSoundSight .=)

        modelIdx <- modelIndex (Just "models/monsters/flipper/tris.md2")

        modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-16) (-16) 0
                                      & eMaxs .~ V3 16 16 32
                                      & eHealth .~ 50
                                      & eGibHealth .~ (-30)
                                      & eMass .~ 100
                                      & ePain .~ Just flipperPain
                                      & eDie .~ Just flipperDie
                                      & eMonsterInfo.miStand .~ Just flipperStand
                                      & eMonsterInfo.miWalk .~ Just flipperWalk
                                      & eMonsterInfo.miRun .~ Just flipperStartRun
                                      & eMonsterInfo.miMelee .~ Just flipperMelee
                                      & eMonsterInfo.miSight .~ Just flipperSight)

        linkEntity selfRef

        modifyEdictT selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveStand
                                      & eMonsterInfo.miScale .~ modelScale)

        void $ think GameAI.swimMonsterStart selfRef
