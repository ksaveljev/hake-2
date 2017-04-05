{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MParasite where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=), (&), (.~), (%~))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.))
import Linear (V3(..), norm)
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
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

modelScale :: Float
modelScale = 1.0

frameBreak01 :: Int
frameBreak01 = 0

frameBreak32 :: Int
frameBreak32 = 31

frameDeath101 :: Int
frameDeath101 = 32

frameDeath107 :: Int
frameDeath107 = 38

frameDrain01 :: Int
frameDrain01 = 39

frameDrain18 :: Int
frameDrain18 = 56

framePain101 :: Int
framePain101 = 57

framePain111 :: Int
framePain111 = 67

frameRun01 :: Int
frameRun01 = 68

frameRun02 :: Int
frameRun02 = 69

frameRun03 :: Int
frameRun03 = 70

frameRun09 :: Int
frameRun09 = 76

frameRun10 :: Int
frameRun10 = 77

frameRun15 :: Int
frameRun15 = 82

frameStand01 :: Int
frameStand01 = 83

frameStand17 :: Int
frameStand17 = 99

frameStand18 :: Int
frameStand18 = 100

frameStand21 :: Int
frameStand21 = 103

frameStand22 :: Int
frameStand22 = 104

frameStand27 :: Int
frameStand27 = 109

frameStand28 :: Int
frameStand28 = 110

frameStand35 :: Int
frameStand35 = 117

parasiteLaunch :: EntThink
parasiteLaunch =
  GenericEntThink "parasite_launch" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundLaunch <- use $ mParasiteGlobals.mParasiteSoundLaunch
    sound (Just selfRef) Constants.chanWeapon soundLaunch 1 Constants.attnNorm 0
    return True

parasiteReelIn :: EntThink
parasiteReelIn =
  GenericEntThink "parasite_reel_in" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundReelIn <- use $ mParasiteGlobals.mParasiteSoundReelIn
    sound (Just selfRef) Constants.chanWeapon soundReelIn 1 Constants.attnNorm 0
    return True

parasiteSight :: EntInteract
parasiteSight =
  GenericEntInteract "parasite_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mParasiteGlobals.mParasiteSoundSight
    sound (Just selfRef) Constants.chanWeapon soundSight 1 Constants.attnNorm 0
    return True

parasiteTap :: EntThink
parasiteTap =
  GenericEntThink "parasite_tap" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundTap <- use $ mParasiteGlobals.mParasiteSoundTap
    sound (Just selfRef) Constants.chanWeapon soundTap 1 Constants.attnIdle 0
    return True

parasiteScratch :: EntThink
parasiteScratch =
  GenericEntThink "parasite_scratch" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundScratch <- use $ mParasiteGlobals.mParasiteSoundScratch
    sound (Just selfRef) Constants.chanWeapon soundScratch 1 Constants.attnIdle 0
    return True

parasiteSearch :: EntThink
parasiteSearch =
  GenericEntThink "parasite_search" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSearch <- use $ mParasiteGlobals.mParasiteSoundSearch
    sound (Just selfRef) Constants.chanWeapon soundSearch 1 Constants.attnIdle 0
    return True

parasiteStartWalk :: EntThink
parasiteStartWalk =
  GenericEntThink "parasite_start_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveStartWalk)
    return True

parasiteWalk :: EntThink
parasiteWalk =
  GenericEntThink "parasite_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveWalk)
    return True

parasiteStand :: EntThink
parasiteStand =
  GenericEntThink "parasite_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveStand)
    return True

parasiteEndFidget :: EntThink
parasiteEndFidget =
  GenericEntThink "parasite_end_fidget" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveEndFidget)
    return True

parasiteDoFidget :: EntThink
parasiteDoFidget =
  GenericEntThink "parasite_do_fidget" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveFidget)
    return True

parasiteReFidget :: EntThink
parasiteReFidget =
  GenericEntThink "parasite_refidget" $ \selfRef -> do
    r <- Lib.randomF

    let action = if r <= 0.8
                   then parasiteMoveFidget
                   else parasiteMoveEndFidget

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

parasiteIdle :: EntThink
parasiteIdle =
  GenericEntThink "parasite_idle" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveStartFidget)
    return True

parasiteStartRun :: EntThink
parasiteStartRun =
  GenericEntThink "parasite_start_run" $ \selfRef -> do
    self <- readRef selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then parasiteMoveStand
                   else parasiteMoveStartRun

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

parasiteRun :: EntThink
parasiteRun =
  GenericEntThink "parasite_run" $ \selfRef -> do
    self <- readRef selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then parasiteMoveStand
                   else parasiteMoveRun

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

parasiteFramesStartFidget :: V.Vector MFrameT
parasiteFramesStartFidget =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

parasiteMoveStartFidget :: MMoveT
parasiteMoveStartFidget = MMoveT "parasiteMoveStartFidget" frameStand18 frameStand21 parasiteFramesStartFidget (Just parasiteDoFidget)

parasiteFramesFidget :: V.Vector MFrameT
parasiteFramesFidget =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 (Just parasiteScratch)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just parasiteScratch)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

parasiteMoveFidget :: MMoveT
parasiteMoveFidget = MMoveT "parasiteMoveFidget" frameStand22 frameStand27 parasiteFramesFidget (Just parasiteReFidget)

parasiteFramesEndFidget :: V.Vector MFrameT
parasiteFramesEndFidget =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 (Just parasiteScratch)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

parasiteMoveEndFidget :: MMoveT
parasiteMoveEndFidget = MMoveT "parasiteMoveEndFidget" frameStand28 frameStand35 parasiteFramesEndFidget (Just parasiteStand)

parasiteFramesStand :: V.Vector MFrameT
parasiteFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just parasiteTap)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just parasiteTap)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just parasiteTap)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just parasiteTap)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just parasiteTap)
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just parasiteTap)
               ]

parasiteMoveStand :: MMoveT
parasiteMoveStand = MMoveT "parasiteMoveStand" frameStand01 frameStand17 parasiteFramesStand (Just parasiteStand)

parasiteFramesRun :: V.Vector MFrameT
parasiteFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 30 Nothing
               , MFrameT (Just GameAI.aiRun) 30 Nothing
               , MFrameT (Just GameAI.aiRun) 22 Nothing
               , MFrameT (Just GameAI.aiRun) 19 Nothing
               , MFrameT (Just GameAI.aiRun) 24 Nothing
               , MFrameT (Just GameAI.aiRun) 28 Nothing
               , MFrameT (Just GameAI.aiRun) 25 Nothing
               ]

parasiteMoveRun :: MMoveT
parasiteMoveRun = MMoveT "parasiteMoveRun" frameRun03 frameRun09 parasiteFramesRun Nothing

parasiteFramesStartRun :: V.Vector MFrameT
parasiteFramesStartRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun) 30 Nothing
               ]

parasiteMoveStartRun :: MMoveT
parasiteMoveStartRun = MMoveT "parasiteMoveStartRun" frameRun01 frameRun02 parasiteFramesStartRun (Just parasiteRun)

parasiteFramesStopRun :: V.Vector MFrameT
parasiteFramesStopRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 20 Nothing
               , MFrameT (Just GameAI.aiRun) 20 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 10 Nothing
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               , MFrameT (Just GameAI.aiRun)  0 Nothing
               ]

parasiteMoveStopRun :: MMoveT
parasiteMoveStopRun = MMoveT "parasiteMoveStopRun" frameRun10 frameRun15 parasiteFramesStopRun Nothing

parasiteFramesWalk :: V.Vector MFrameT
parasiteFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 30 Nothing
               , MFrameT (Just GameAI.aiWalk) 30 Nothing
               , MFrameT (Just GameAI.aiWalk) 22 Nothing
               , MFrameT (Just GameAI.aiWalk) 19 Nothing
               , MFrameT (Just GameAI.aiWalk) 24 Nothing
               , MFrameT (Just GameAI.aiWalk) 28 Nothing
               , MFrameT (Just GameAI.aiWalk) 25 Nothing
               ]

parasiteMoveWalk :: MMoveT
parasiteMoveWalk = MMoveT "parasiteMoveWalk" frameRun03 frameRun09 parasiteFramesWalk (Just parasiteWalk)

parasiteFramesStartWalk :: V.Vector MFrameT
parasiteFramesStartWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk) 30 (Just parasiteWalk) 
               ]

parasiteMoveStartWalk :: MMoveT
parasiteMoveStartWalk = MMoveT "parasiteMoveStartWalk" frameRun01 frameRun02 parasiteFramesStartWalk Nothing

parasiteFramesStopWalk :: V.Vector MFrameT
parasiteFramesStopWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 20 Nothing
               , MFrameT (Just GameAI.aiWalk) 20 Nothing
               , MFrameT (Just GameAI.aiWalk) 12 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               ]

parasiteMoveStopWalk :: MMoveT
parasiteMoveStopWalk = MMoveT "parasiteMoveStopWalk" frameRun10 frameRun15 parasiteFramesStopWalk Nothing

parasiteFramesPain1 :: V.Vector MFrameT
parasiteFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               , MFrameT (Just GameAI.aiMove)   6  Nothing
               , MFrameT (Just GameAI.aiMove)  16  Nothing
               , MFrameT (Just GameAI.aiMove) (-6) Nothing
               , MFrameT (Just GameAI.aiMove) (-7) Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

parasiteMovePain1 :: MMoveT
parasiteMovePain1 = MMoveT "parasiteMovePain1" framePain101 framePain111 parasiteFramesPain1 (Just parasiteStartRun)

parasitePain :: EntPain
parasitePain =
  GenericEntPain "parasite_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        r <- Lib.randomF

        soundPain <- if r < 0.5
                       then use $ mParasiteGlobals.mParasiteSoundPain1
                       else use $ mParasiteGlobals.mParasiteSoundPain2

        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0

        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMovePain1)

parasiteDrainAttack :: EntThink
parasiteDrainAttack =
  GenericEntThink "parasite_drain_attack" $ \_ -> do
    io (putStrLn "MParasite.parasiteDrainAttack") >> undefined -- TODO

parasiteFramesDrain :: V.Vector MFrameT
parasiteFramesDrain =
    V.fromList [ MFrameT (Just GameAI.aiCharge)   0  (Just parasiteLaunch)
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               , MFrameT (Just GameAI.aiCharge)  15  (Just parasiteDrainAttack) -- Target hits
               , MFrameT (Just GameAI.aiCharge)   0  (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge)   0  (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge)   0  (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge)   0  (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge) (-2) (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge) (-2) (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge) (-3) (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge) (-2) (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge)   0  (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge) (-1) (Just parasiteDrainAttack) -- drain
               , MFrameT (Just GameAI.aiCharge)   0  (Just parasiteReelIn) -- let go
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               , MFrameT (Just GameAI.aiCharge) (-3) Nothing
               , MFrameT (Just GameAI.aiCharge)   0  Nothing
               ]

parasiteMoveDrain :: MMoveT
parasiteMoveDrain = MMoveT "parasiteMoveDrain" frameDrain01 frameDrain18 parasiteFramesDrain (Just parasiteStartRun)

parasiteFramesBreak :: V.Vector MFrameT
parasiteFramesBreak =
    V.fromList [ MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)  (-3) Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    2  Nothing
               , MFrameT (Just GameAI.aiCharge)  (-3) Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    1  Nothing
               , MFrameT (Just GameAI.aiCharge)    3  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-18) Nothing
               , MFrameT (Just GameAI.aiCharge)    3  Nothing
               , MFrameT (Just GameAI.aiCharge)    9  Nothing
               , MFrameT (Just GameAI.aiCharge)    6  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-18) Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    8  Nothing
               , MFrameT (Just GameAI.aiCharge)    9  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge) (-18) Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
               , MFrameT (Just GameAI.aiCharge)    0  Nothing
                 -- airborne
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- slides
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- slides
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- slides
               , MFrameT (Just GameAI.aiCharge)   0  Nothing -- slides
               , MFrameT (Just GameAI.aiCharge)   4  Nothing
               , MFrameT (Just GameAI.aiCharge)  11  Nothing
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               , MFrameT (Just GameAI.aiCharge) (-5) Nothing
               , MFrameT (Just GameAI.aiCharge)   1  Nothing
               ]

parasiteMoveBreak :: MMoveT
parasiteMoveBreak = MMoveT "parasiteMoveBreak" frameBreak01 frameBreak32 parasiteFramesBreak (Just parasiteStartRun)

parasiteAttack :: EntThink
parasiteAttack =
  GenericEntThink "parasite_attack" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveDrain)
    return True

parasiteDead :: EntThink
parasiteDead =
  GenericEntThink "parasite_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

parasiteFramesDeath :: V.Vector MFrameT
parasiteFramesDeath =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

parasiteMoveDeath :: MMoveT
parasiteMoveDeath = MMoveT "parasiteMoveDeath" frameDeath101 frameDeath107 parasiteFramesDeath (Just parasiteDead)

parasiteDie :: EntDie
parasiteDie =
  GenericEntDie "parasite_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
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

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)

       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()

       | otherwise -> do -- regular death
           soundDie <- use $ mParasiteGlobals.mParasiteSoundDie
           sound (Just selfRef) Constants.chanVoice soundDie 1 Constants.attnNorm 0

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         & eMonsterInfo.miCurrentMove .~ Just parasiteMoveDeath)

{-
- QUAKED monster_parasite (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterParasite :: EntThink
spMonsterParasite =
  GenericEntThink "SP_monster_parasite" $ \selfRef -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then do
        GameUtil.freeEdict selfRef
        return True

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex (Just "parasite/parpain1.wav") >>= (mParasiteGlobals.mParasiteSoundPain1 .=)
        soundIndex (Just "parasite/parpain2.wav") >>= (mParasiteGlobals.mParasiteSoundPain2 .=)
        soundIndex (Just "parasite/pardeth1.wav") >>= (mParasiteGlobals.mParasiteSoundDie .=)
        soundIndex (Just "parasite/paratck1.wav") >>= (mParasiteGlobals.mParasiteSoundLaunch .=)
        soundIndex (Just "parasite/paratck2.wav") >>= (mParasiteGlobals.mParasiteSoundImpact .=)
        soundIndex (Just "parasite/paratck3.wav") >>= (mParasiteGlobals.mParasiteSoundSuck .=)
        soundIndex (Just "parasite/paratck4.wav") >>= (mParasiteGlobals.mParasiteSoundReelIn .=)
        soundIndex (Just "parasite/parsght1.wav") >>= (mParasiteGlobals.mParasiteSoundSight .=)
        soundIndex (Just "parasite/paridle1.wav") >>= (mParasiteGlobals.mParasiteSoundTap .=)
        soundIndex (Just "parasite/paridle2.wav") >>= (mParasiteGlobals.mParasiteSoundScratch .=)
        soundIndex (Just "parasite/parsrch1.wav") >>= (mParasiteGlobals.mParasiteSoundSearch .=)

        modelIdx <- modelIndex (Just "models/monsters/parasite/tris.md2")

        modifyRef selfRef (\v -> v & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-16) (-16) (-24)
                                      & eMaxs .~ V3 16 16 24
                                      & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eHealth .~ 175
                                      & eGibHealth .~ (-50)
                                      & eMass .~ 250
                                      & ePain .~ Just parasitePain
                                      & eDie .~ Just parasiteDie
                                      & eMonsterInfo.miStand .~ Just parasiteStand
                                      & eMonsterInfo.miWalk .~ Just parasiteStartWalk
                                      & eMonsterInfo.miRun .~ Just parasiteStartRun
                                      & eMonsterInfo.miAttack .~ Just parasiteAttack
                                      & eMonsterInfo.miSight .~ Just parasiteSight
                                      & eMonsterInfo.miIdle .~ Just parasiteIdle)

        linkEntity selfRef

        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just parasiteMoveStand
                                      & eMonsterInfo.miScale .~ modelScale)

        void $ think GameAI.walkMonsterStart selfRef
        return True

parasiteDrainAttackOK :: V3 Float -> V3 Float -> Bool
parasiteDrainAttackOK start end =
    let dir = start - end
    in if norm dir > 256
         then False
         else let V3 a _ _ = Math3D.vectorAngles dir
                  a' = if a < (-180) then a + 360 else a
              in if abs a' > 30
                   then False
                   else True
