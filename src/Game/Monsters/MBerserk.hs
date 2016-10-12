module Game.Monsters.MBerserk
    ( spMonsterBerserk
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~))
import           Control.Monad         (replicateM_, void, unless, when)
import           Data.Bits             ((.&.), (.|.))
import qualified Data.Vector           as V
import           Linear                (V3(..), _x)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import {-# SOURCE #-} Game.GameImportT
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameUtil         as GameUtil
import qualified Game.GameWeapon       as GameWeapon
import           Game.LevelLocalsT
import           Game.MonsterInfoT
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib

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

spMonsterBerserk :: Ref EdictT -> Quake ()
spMonsterBerserk selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterBerserk selfRef gameImport deathmatch

proceedSpawnMonsterBerserk :: Ref EdictT -> GameImportT -> Float -> Quake ()
proceedSpawnMonsterBerserk selfRef gameImport deathmatch
    | deathmatch /= 0 = GameUtil.freeEdict selfRef
    | otherwise = do
        soundIndex (Just "berserk/berpain2.wav") >>= (mBerserkGlobals.mBerserkSoundPain   .=)
        soundIndex (Just "berserk/berdeth2.wav") >>= (mBerserkGlobals.mBerserkSoundDie    .=)
        soundIndex (Just "berserk/beridle1.wav") >>= (mBerserkGlobals.mBerserkSoundIdle   .=)
        soundIndex (Just "berserk/attack.wav")   >>= (mBerserkGlobals.mBerserkSoundPunch  .=)
        soundIndex (Just "berserk/bersrch1.wav") >>= (mBerserkGlobals.mBerserkSoundSearch .=)
        soundIndex (Just "berserk/sight.wav")    >>= (mBerserkGlobals.mBerserkSoundSight  .=)
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
        void (entThink GameAI.walkMonsterStart selfRef)
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity

berserkPain :: EntPain
berserkPain = EntPain "berserk_pain" $ \selfRef _ _ damage -> do
    self <- readRef selfRef
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        playPainSound selfRef
        skill <- fmap (^.cvValue) skillCVar
        setPainAnimation selfRef damage skill
  where
    playPainSound selfRef = do
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        soundPain <- use (mBerserkGlobals.mBerserkSoundPain)
        sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
    setPainAnimation selfRef damage skill
        | skill == 3 = return ()
        | otherwise = do
            currentMove <- pickMove damage <$> Lib.randomF
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
    pickMove damage r
        | damage < 20 || r < 0.5 = berserkMovePain1
        | otherwise = berserkMovePain2

berserkDie :: EntDie
berserkDie = EntDie "berserk_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    doBerserkDie selfRef self gameImport damage

doBerserkDie :: Ref EdictT -> EdictT -> GameImportT -> Int -> Quake ()
doBerserkDie selfRef self gameImport damage
    | (self^.eHealth) < (self^.eGibHealth) = do
        soundIdx <- soundIndex (Just "misc/udeath.wav")
        sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
        replicateM_ 2 (GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic)
        replicateM_ 4 (GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic)
        GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic
        modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
    | (self^.eDeadFlag) == Constants.deadDead = return ()
    | otherwise = do
        soundDie <- use (mBerserkGlobals.mBerserkSoundDie)
        sound (Just selfRef) Constants.chanVoice soundDie 1 Constants.attnNorm 0
        modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                   & eTakeDamage .~ Constants.damageYes
                                   & eMonsterInfo.miCurrentMove .~ Just currentMove)
  where
    sound = gameImport^.giSound
    soundIndex = gameImport^.giSoundIndex
    currentMove | damage >= 50 = berserkMoveDeath1
                | otherwise = berserkMoveDeath2

berserkStand :: EntThink
berserkStand = EntThink "berserk_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just berserkMoveStand)
    return True

berserkWalk :: EntThink
berserkWalk = EntThink "berserk_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just berserkMoveWalk)
    return True

berserkRun :: EntThink
berserkRun = EntThink "berserk_run" $ \selfRef -> do
    action <- pickAction <$> readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    pickAction self
        | shouldStandGround self = berserkMoveStand
        | otherwise = berserkMoveRun1
    shouldStandGround self =
        (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0

berserkMelee :: EntThink
berserkMelee = EntThink "berserk_melee" $ \selfRef -> do
    action <- pickAction <$> Lib.rand
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    pickAction r | r .&. 1 == 0 = berserkMoveAttackSpike
                 | otherwise = berserkMoveAttackClub

berserkSight :: EntInteract
berserkSight = EntInteract "berserk_sight" $ \selfRef _ -> do
    soundSight <- use (mBerserkGlobals.mBerserkSoundSight)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

berserkSearch :: EntThink
berserkSearch = EntThink "berserk_search" $ \selfRef -> do
    soundSearch <- use (mBerserkGlobals.mBerserkSoundSearch)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

berserkMoveStand :: MMoveT
berserkMoveStand = MMoveT "berserkMoveStand" frameStand1 frameStand5 berserkFramesStand Nothing

berserkFramesStand :: V.Vector MFrameT
berserkFramesStand = V.fromList (fmap (MFrameT (Just GameAI.aiStand) 0) think)
  where
    think = [ Just berserkFidget, Nothing, Nothing, Nothing, Nothing ]

berserkFidget :: EntThink
berserkFidget = EntThink "berserk_fidget" $ \selfRef -> do
    self <- readRef selfRef
    r <- Lib.randomF
    doBerserkFidget selfRef self r
  where
    doBerserkFidget selfRef self r
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 = return True
        | r > 0.15 = return True
        | otherwise = do
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just berserkMoveStandFidget)
            soundIdle <- use (mBerserkGlobals.mBerserkSoundIdle)
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) Constants.chanWeapon soundIdle 1 Constants.attnIdle 0
            return True

berserkMoveWalk :: MMoveT
berserkMoveWalk = MMoveT "berserkMoveWalk" frameWalkC1 frameWalkC11 berserkFramesWalk Nothing

berserkFramesWalk :: V.Vector MFrameT
berserkFramesWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dist)
  where
    dist = [ 9.1, 6.3, 4.9, 6.7, 6.0, 8.2, 7.2, 6.1, 4.9, 4.7, 4.7, 4.8 ]

berserkMovePain1 :: MMoveT
berserkMovePain1 = MMoveT "berserkMovePain1" framePainC1 framePainC4 berserkFramesPain1 (Just berserkRun)

berserkFramesPain1 :: V.Vector MFrameT
berserkFramesPain1 = V.replicate 4 (MFrameT (Just GameAI.aiMove) 0 Nothing)

berserkMovePain2 :: MMoveT
berserkMovePain2 = MMoveT "berserkMovePain2" framePainB1 framePainB20 berserkFramesPain2 (Just berserkRun)

berserkFramesPain2 :: V.Vector MFrameT
berserkFramesPain2 = V.replicate 20 (MFrameT (Just GameAI.aiMove) 0 Nothing)

berserkMoveRun1 :: MMoveT
berserkMoveRun1 = MMoveT "berserkMoveRun1" frameRun1 frameRun6 berserkFramesRun1 Nothing

berserkFramesRun1 :: V.Vector MFrameT
berserkFramesRun1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dist)
  where
    dist = [ 21, 11, 21, 25, 18, 19 ]

berserkAttackSpike :: EntThink
berserkAttackSpike = EntThink "berserk_attack_spike" $ \selfRef -> do
    r <- Lib.rand
    void (GameWeapon.fireHit selfRef aim (fromIntegral (r `mod` 6 + 15)) 400) -- Faster attack -- upwards and backwards
    return True
  where
    aim = V3 (fromIntegral Constants.meleeDistance) 0 (-24)

berserkAttackClub :: EntThink
berserkAttackClub = EntThink "berserk_attack_club" $ \selfRef -> do
    self <- readRef selfRef
    r <- Lib.rand
    void (GameWeapon.fireHit selfRef (aim self) (fromIntegral (r `mod` 6 + 5)) 400) -- slower attack
    return True
  where
    aim self = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) (-4)

berserkMoveStandFidget :: MMoveT
berserkMoveStandFidget = MMoveT "berserkMoveStandFidget" frameStandB1 frameStandB20 berserkFramesStandFidget (Just berserkStand)

berserkFramesStandFidget :: V.Vector MFrameT
berserkFramesStandFidget = V.replicate 20 (MFrameT (Just GameAI.aiStand) 0 Nothing)

berserkMoveAttackClub :: MMoveT
berserkMoveAttackClub = MMoveT "berserkMoveAttackClub" frameAttC9 frameAttC20 berserkFramesAttackClub (Just berserkRun)

berserkFramesAttackClub :: V.Vector MFrameT
berserkFramesAttackClub =
    V.fromList (fmap (MFrameT (Just GameAI.aiCharge) 0) think)
  where
    think = [ Nothing, Nothing, Nothing, Nothing, (Just berserkSwing)
            , Nothing, Nothing, Nothing, (Just berserkAttackClub)
            , Nothing, Nothing, Nothing
            ]

berserkMoveAttackSpike :: MMoveT
berserkMoveAttackSpike = MMoveT "berserkMoveAttackSpike" frameAttC1 frameAttC8 berserkFramesAttackSpike (Just berserkRun)

berserkFramesAttackSpike :: V.Vector MFrameT
berserkFramesAttackSpike =
    V.fromList (fmap (MFrameT (Just GameAI.aiCharge) 0) think)
  where
    think = [ Nothing, Nothing, (Just berserkSwing), (Just berserkAttackSpike)
            , Nothing, Nothing, Nothing, Nothing
            ]

berserkSwing :: EntThink
berserkSwing = EntThink "berserk_swing" $ \selfRef -> do
    soundPunch <- use (mBerserkGlobals.mBerserkSoundPunch)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanWeapon soundPunch 1 Constants.attnNorm 0
    return True

berserkMoveDeath1 :: MMoveT
berserkMoveDeath1 = MMoveT "berserkMoveDeath1" frameDeath1 frameDeath13 berserkFramesDeath1 (Just berserkDead)

berserkFramesDeath1 :: V.Vector MFrameT
berserkFramesDeath1 = V.replicate 13 (MFrameT (Just GameAI.aiMove) 0 Nothing)

berserkMoveDeath2 :: MMoveT
berserkMoveDeath2 = MMoveT "berserkMoveDeath2" frameDeathC1 frameDeathC8 berserkFramesDeath2 (Just berserkDead)

berserkFramesDeath2 :: V.Vector MFrameT
berserkFramesDeath2 = V.replicate 8 (MFrameT (Just GameAI.aiMove) 0 Nothing)

berserkDead :: EntThink
berserkDead = EntThink "berserk_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True