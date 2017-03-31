{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MGladiator where

import Control.Lens (use, preuse, ix, (^.), (.=), (%=), zoom, (&), (.~), (%~))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (isJust)
import Linear (V3(..), normalize, norm, _z)
import qualified Data.Vector as V

import Game.MMoveT
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
import qualified Game.Monster as Monster
import qualified Game.Monsters.MFlash as MFlash
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

modelScale :: Float
modelScale = 1.0

frameStand1 :: Int
frameStand1 = 0

frameStand7 :: Int
frameStand7 = 6

frameWalk1 :: Int
frameWalk1 = 7

frameWalk16 :: Int
frameWalk16 = 22

frameRun1 :: Int
frameRun1 = 23

frameRun6 :: Int
frameRun6 = 28

frameMelee1 :: Int
frameMelee1 = 29

frameMelee17 :: Int
frameMelee17 = 45

frameAttack1 :: Int
frameAttack1 = 46

frameAttack9 :: Int
frameAttack9 = 54

framePain1 :: Int
framePain1 = 55

framePain6 :: Int
framePain6 = 60

frameDeath1 :: Int
frameDeath1 = 61

frameDeath22 :: Int
frameDeath22 = 82

framePainUp1 :: Int
framePainUp1 = 83

framePainUp7 :: Int
framePainUp7 = 89

gladiatorIdle :: EntThink
gladiatorIdle =
  GenericEntThink "gladiator_idle" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundIdle <- use $ mGladiatorGlobals.mGladiatorSoundIdle
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

gladiatorSight :: EntInteract
gladiatorSight =
  GenericEntInteract "gladiator_sight" $ \selfRef _ -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSight <- use $ mGladiatorGlobals.mGladiatorSoundSight
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

gladiatorSearch :: EntThink
gladiatorSearch =
  GenericEntThink "gladiator_search" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundSearch <- use $ mGladiatorGlobals.mGladiatorSoundSearch
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

gladiatorCleaverSwing :: EntThink
gladiatorCleaverSwing =
  GenericEntThink "gladiator_cleaver_swing" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundCleaverSwing <- use $ mGladiatorGlobals.mGladiatorSoundCleaverSwing
    sound (Just selfRef) Constants.chanWeapon soundCleaverSwing 1 Constants.attnNorm 0
    return True

gladiatorFramesStand :: V.Vector MFrameT
gladiatorFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

gladiatorMoveStand :: MMoveT
gladiatorMoveStand = MMoveT "gladiatorMoveStand" frameStand1 frameStand7 gladiatorFramesStand Nothing

gladiatorStand :: EntThink
gladiatorStand =
  GenericEntThink "gladiator_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gladiatorMoveStand)
    return True

gladiatorFramesWalk :: V.Vector MFrameT
gladiatorFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk) 15 Nothing
               , MFrameT (Just GameAI.aiWalk)  7 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk)  2 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  2 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 Nothing
               , MFrameT (Just GameAI.aiWalk) 12 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 Nothing
               , MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk)  5 Nothing
               , MFrameT (Just GameAI.aiWalk)  2 Nothing
               , MFrameT (Just GameAI.aiWalk)  2 Nothing
               , MFrameT (Just GameAI.aiWalk)  1 Nothing
               , MFrameT (Just GameAI.aiWalk)  8 Nothing
               ]

gladiatorMoveWalk :: MMoveT
gladiatorMoveWalk = MMoveT "gladiatorMoveWalk" frameWalk1 frameWalk16 gladiatorFramesWalk Nothing

gladiatorWalk :: EntThink
gladiatorWalk =
  GenericEntThink "gladiator_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gladiatorMoveWalk)
    return True

gladiatorFramesRun :: V.Vector MFrameT
gladiatorFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 23 Nothing
               , MFrameT (Just GameAI.aiRun) 14 Nothing
               , MFrameT (Just GameAI.aiRun) 14 Nothing
               , MFrameT (Just GameAI.aiRun) 21 Nothing
               , MFrameT (Just GameAI.aiRun) 12 Nothing
               , MFrameT (Just GameAI.aiRun) 13 Nothing
               ]

gladiatorMoveRun :: MMoveT
gladiatorMoveRun = MMoveT "gladiatorMoveRun" frameRun1 frameRun6 gladiatorFramesRun Nothing

gladiatorRun :: EntThink
gladiatorRun =
  GenericEntThink "gladiator_run" $ \selfRef -> do
    self <- readRef selfRef

    let action = if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
                   then gladiatorMoveStand
                   else gladiatorMoveRun

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True

gladiatorMelee :: EntThink
gladiatorMelee =
  GenericEntThink "GladiatorMelee" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gladiatorMoveAttackMelee)
    return True

gladiatorFramesAttackMelee :: V.Vector MFrameT
gladiatorFramesAttackMelee =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gladiatorCleaverSwing)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gladiatorMelee)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gladiatorCleaverSwing)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gladiatorMelee)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

gladiatorMoveAttackMelee :: MMoveT
gladiatorMoveAttackMelee = MMoveT "gladiatorMoveAttackMelee" frameMelee1 frameMelee17 gladiatorFramesAttackMelee (Just gladiatorRun)

gladiatorAttackMelee :: EntThink
gladiatorAttackMelee =
  GenericEntThink "gladiator_melee" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gladiatorMoveAttackMelee)
    return True

gladiatorGun :: EntThink
gladiatorGun =
  GenericEntThink "GladiatorGun" $ \selfRef -> do
    self <- readRef selfRef

    let (Just forward, Just right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2GladiatorRailgun1) forward right
        -- calc direction to where we targeted
        dir = normalize ((self^.ePos1) - start)

    Monster.monsterFireRailgun selfRef start dir 50 100 Constants.mz2GladiatorRailgun1

    return True

gladiatorFramesAttackGun :: V.Vector MFrameT
gladiatorFramesAttackGun =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just gladiatorGun)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

gladiatorMoveAttackGun :: MMoveT
gladiatorMoveAttackGun = MMoveT "gladiatorMoveAttackGun" frameAttack1 frameAttack9 gladiatorFramesAttackGun (Just gladiatorRun)

gladiatorAttack :: EntThink
gladiatorAttack =
  GenericEntThink "gladiator_attack" $ \selfRef -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    -- a small safe zone
    let v = (self^.eEntityState.esOrigin) - (enemy^.eEntityState.esOrigin)
        range = norm v

    if range <= fromIntegral Constants.meleeDistance + 32
      then
        return True

      else do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        soundGun <- use $ mGladiatorGlobals.mGladiatorSoundGun
        sound (Just selfRef) Constants.chanWeapon soundGun 1 Constants.attnNorm 0

        let V3 a b c = enemy^.eEntityState.esOrigin
            pos1 = V3 a b (c + fromIntegral (enemy^.eViewHeight))

        modifyRef selfRef (\v -> v & ePos1 .~ pos1
                                      & eMonsterInfo.miCurrentMove .~ Just gladiatorMoveAttackGun)

        return True

gladiatorFramesPain :: V.Vector MFrameT
gladiatorFramesPain =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

gladiatorMovePain :: MMoveT
gladiatorMovePain = MMoveT "gladiatorMovePain" framePain1 framePain6 gladiatorFramesPain (Just gladiatorRun)

gladiatorFramesPainAir :: V.Vector MFrameT
gladiatorFramesPainAir =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

gladiatorMovePainAir :: MMoveT
gladiatorMovePainAir = MMoveT "gladiatorMovePainAir" framePainUp1 framePainUp7 gladiatorFramesPainAir (Just gladiatorRun)

gladiatorPain :: EntPain
gladiatorPain =
  GenericEntPain "gladiator_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if levelTime < (self^.ePainDebounceTime)
      then do
        when (isJust (self^.eMonsterInfo.miCurrentMove)) $ do
          let Just currentMove = self^.eMonsterInfo.miCurrentMove

          when ((self^.eVelocity._z) > 100 && (currentMove^.mmId) == "gladiatorMovePain") $
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gladiatorMovePainAir)

      else do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

        r <- Lib.randomF

        soundPain <- if r < 0.5
                       then use $ mGladiatorGlobals.mGladiatorSoundPain1
                       else use $ mGladiatorGlobals.mGladiatorSoundPain2

        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0

        skillValue <- liftM (^.cvValue) skillCVar

        unless (skillValue == 3) $ do -- no pain anims in nightmare
          let currentMove = if (self^.eVelocity._z) > 100
                              then gladiatorMovePainAir
                              else gladiatorMovePain

          modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

gladiatorDead :: EntThink
gladiatorDead =
  GenericEntThink "gladiator_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

gladiatorFramesDeath :: V.Vector MFrameT
gladiatorFramesDeath =
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
               ]

gladiatorMoveDeath :: MMoveT
gladiatorMoveDeath = MMoveT "gladiatorMoveDeath" frameDeath1 frameDeath22 gladiatorFramesDeath (Just gladiatorDead)

gladiatorDie :: EntDie
gladiatorDie =
  GenericEntDie "gladiator_die" $ \selfRef _ _ damage _ -> do
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
           soundDie <- use $ mGladiatorGlobals.mGladiatorSoundDie
           sound (Just selfRef) Constants.chanVoice soundDie 1 Constants.attnNorm 0

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         & eMonsterInfo.miCurrentMove .~ Just gladiatorMoveDeath)

{-
- QUAKED monster_gladiator (1 .5 0) (-32 -32 -24) (32 32 64) Ambush
- Trigger_Spawn Sight
-}
spMonsterGladiator :: Ref EdictT -> Quake ()
spMonsterGladiator selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    
    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex (Just "gladiator/pain.wav") >>= (mGladiatorGlobals.mGladiatorSoundPain1 .=)
        soundIndex (Just "gladiator/gldpain2.wav") >>= (mGladiatorGlobals.mGladiatorSoundPain2 .=)
        soundIndex (Just "gladiator/glddeth2.wav") >>= (mGladiatorGlobals.mGladiatorSoundDie .=)
        soundIndex (Just "gladiator/railgun.wav") >>= (mGladiatorGlobals.mGladiatorSoundGun .=)
        soundIndex (Just "gladiator/melee1.wav") >>= (mGladiatorGlobals.mGladiatorSoundCleaverSwing .=)
        soundIndex (Just "gladiator/melee2.wav") >>= (mGladiatorGlobals.mGladiatorSoundCleaverHit .=)
        soundIndex (Just "gladiator/melee3.wav") >>= (mGladiatorGlobals.mGladiatorSoundCleaverMiss .=)
        soundIndex (Just "gladiator/gldidle1.wav") >>= (mGladiatorGlobals.mGladiatorSoundIdle .=)
        soundIndex (Just "gladiator/gldsrch1.wav") >>= (mGladiatorGlobals.mGladiatorSoundSearch .=)
        soundIndex (Just "gladiator/sight.wav") >>= (mGladiatorGlobals.mGladiatorSoundSight .=)

        modelIdx <- modelIndex (Just "models/monsters/gladiatr/tris.md2")

        modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                      & eSolid .~ Constants.solidBbox
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-32) (-32) (-24)
                                      & eMaxs .~ V3 32 32 64
                                      & eHealth .~ 400
                                      & eGibHealth .~ (-175)
                                      & eMass .~ 400
                                      & ePain .~ Just gladiatorPain
                                      & eDie .~ Just gladiatorDie
                                      & eMonsterInfo.miStand .~ Just gladiatorStand
                                      & eMonsterInfo.miWalk .~ Just gladiatorWalk
                                      & eMonsterInfo.miRun .~ Just gladiatorRun
                                      & eMonsterInfo.miDodge .~ Nothing
                                      & eMonsterInfo.miAttack .~ Just gladiatorAttack
                                      & eMonsterInfo.miMelee .~ Just gladiatorAttackMelee
                                      & eMonsterInfo.miSight .~ Just gladiatorSight
                                      & eMonsterInfo.miIdle .~ Just gladiatorIdle
                                      & eMonsterInfo.miSearch .~ Just gladiatorSearch)

        linkEntity selfRef

        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just gladiatorMoveStand
                                      & eMonsterInfo.miScale .~ modelScale)

        void $ think GameAI.walkMonsterStart selfRef
