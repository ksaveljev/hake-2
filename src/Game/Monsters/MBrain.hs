{-# LANGUAGE FlexibleContexts #-}
module Game.Monsters.MBrain
    ( spMonsterBrain
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (+~), (%~), (-~))
import           Control.Monad         (replicateM_, void, unless, when)
import           Data.Bits             (complement, (.&.), (.|.))
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V
import           Linear                (V3(..), _x, _z)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
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

frameWalk101 :: Int
frameWalk101 = 0

frameWalk111 :: Int
frameWalk111 = 10

frameAttack101 :: Int
frameAttack101 = 53

frameAttack118 :: Int
frameAttack118 = 70

frameAttack201 :: Int
frameAttack201 = 71

frameAttack217 :: Int
frameAttack217 = 87

framePain101 :: Int
framePain101 = 88

framePain121 :: Int
framePain121 = 108

framePain201 :: Int
framePain201 = 109

framePain208 :: Int
framePain208 = 116

framePain301 :: Int
framePain301 = 117

framePain306 :: Int
framePain306 = 122

frameDeath101 :: Int
frameDeath101 = 123

frameDeath118 :: Int
frameDeath118 = 140

frameDeath201 :: Int
frameDeath201 = 141

frameDeath205 :: Int
frameDeath205 = 145

frameDuck01 :: Int
frameDuck01 = 146

frameDuck08 :: Int
frameDuck08 = 153

frameStand01 :: Int
frameStand01 = 162

frameStand30 :: Int
frameStand30 = 191

frameStand31 :: Int
frameStand31 = 192

frameStand60 :: Int
frameStand60 = 221

spMonsterBrain :: Ref' EdictT -> Quake ()
spMonsterBrain selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterBrain selfRef gameImport deathmatch

proceedSpawnMonsterBrain :: Ref' EdictT -> GameImportT -> Float -> Quake ()
proceedSpawnMonsterBrain selfRef gameImport deathmatch
    | deathmatch /= 0 = GameUtil.freeEdict selfRef
    | otherwise = do
        soundIndex (Just "brain/brnatck1.wav") >>= (mBrainGlobals.mBrainSoundChestOpen        .=)
        soundIndex (Just "brain/brnatck2.wav") >>= (mBrainGlobals.mBrainSoundTentaclesExtend  .=)
        soundIndex (Just "brain/brnatck3.wav") >>= (mBrainGlobals.mBrainSoundTentaclesRetract .=)
        soundIndex (Just "brain/brndeth1.wav") >>= (mBrainGlobals.mBrainSoundDeath            .=)
        soundIndex (Just "brain/brnidle1.wav") >>= (mBrainGlobals.mBrainSoundIdle1            .=)
        soundIndex (Just "brain/brnidle2.wav") >>= (mBrainGlobals.mBrainSoundIdle2            .=)
        soundIndex (Just "brain/brnlens1.wav") >>= (mBrainGlobals.mBrainSoundIdle3            .=)
        soundIndex (Just "brain/brnpain1.wav") >>= (mBrainGlobals.mBrainSoundPain1            .=)
        soundIndex (Just "brain/brnpain2.wav") >>= (mBrainGlobals.mBrainSoundPain2            .=)
        soundIndex (Just "brain/brnsght1.wav") >>= (mBrainGlobals.mBrainSoundSight            .=)
        soundIndex (Just "brain/brnsrch1.wav") >>= (mBrainGlobals.mBrainSoundSearch           .=)
        soundIndex (Just "brain/melee1.wav")   >>= (mBrainGlobals.mBrainSoundMelee1           .=)
        soundIndex (Just "brain/melee2.wav")   >>= (mBrainGlobals.mBrainSoundMelee2           .=)
        soundIndex (Just "brain/melee3.wav")   >>= (mBrainGlobals.mBrainSoundMelee3           .=)
        modelIdx <- modelIndex (Just "models/monsters/brain/tris.md2")
        modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                   & eSolid .~ Constants.solidBbox
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eMins .~ V3 (-16) (-16) (-24)
                                   & eMaxs .~ V3 16 16 32
                                   & eHealth .~ 300
                                   & eGibHealth .~ (-150)
                                   & eMass .~ 400
                                   & ePain .~ Just brainPain
                                   & eDie .~ Just brainDie
                                   & eMonsterInfo.miStand .~ Just brainStand
                                   & eMonsterInfo.miWalk .~ Just brainWalk
                                   & eMonsterInfo.miRun .~ Just brainRun
                                   & eMonsterInfo.miDodge .~ Just brainDodge
                                   & eMonsterInfo.miMelee .~ Just brainMelee
                                   & eMonsterInfo.miSight .~ Just brainSight
                                   & eMonsterInfo.miSearch .~ Just brainSearch
                                   & eMonsterInfo.miIdle .~ Just brainIdle
                                   & eMonsterInfo.miPowerArmorType .~ Constants.powerArmorScreen
                                   & eMonsterInfo.miPowerArmorPower .~ 100)
        linkEntity selfRef
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just brainMoveStand
                                   & eMonsterInfo.miScale .~ modelScale)
        void (entThink GameAI.walkMonsterStart selfRef)
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity

brainPain :: EntPain
brainPain = EntPain "brain_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        skill <- fmap (^.cvValue) skillCVar
        unless (skill == 3) $ do -- no pain anims in nightmare
            r <- Lib.randomF
            (soundPain, currentMove) <- pickSoundAndMove r
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
  where
    pickSoundAndMove r
        | r < 0.33 = do
            soundPain <- use (mBrainGlobals.mBrainSoundPain1)
            return (soundPain, brainMovePain1)
        | r < 0.66 = do
            soundPain <- use (mBrainGlobals.mBrainSoundPain2)
            return (soundPain, brainMovePain2)
        | otherwise = do
            soundPain <- use (mBrainGlobals.mBrainSoundPain1)
            return (soundPain, brainMovePain3)

brainWalk :: EntThink
brainWalk = EntThink "brain_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just brainMoveWalk1)
    return True

brainStand :: EntThink
brainStand = EntThink "brain_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just brainMoveStand)
    return True

brainRun :: EntThink
brainRun = EntThink "brain_run" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miPowerArmorType .~ Constants.powerArmorScreen)
    action <- pickAction <$> readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    pickAction self
        | shouldStandGround self = brainMoveStand
        | otherwise = brainMoveRun
    shouldStandGround self =
        (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0

brainDodge :: EntDodge
brainDodge = EntDodge "brain_dodge" $ \selfRef attackerRef eta -> do
    r <- Lib.randomF
    unless (r > 0.25) $ do
        self <- readRef selfRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        when (isNothing (self^.eEnemy)) $
            modifyRef selfRef (\v -> v & eEnemy .~ Just attackerRef
                                       & eMonsterInfo.miPauseTime .~ levelTime + eta + 0.5
                                       & eMonsterInfo.miCurrentMove .~ Just brainMoveDuck)

brainMelee :: EntThink
brainMelee = EntThink "brain_melee" $ \selfRef -> do
    action <- pickAction <$> Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    pickAction r | r <= 0.5 = brainMoveAttack1
                 | otherwise = brainMoveAttack2

brainSight :: EntInteract
brainSight = EntInteract "brain_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mBrainGlobals.mBrainSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

brainSearch :: EntThink
brainSearch = EntThink "brain_search" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSearch <- use (mBrainGlobals.mBrainSoundSearch)
    sound (Just selfRef) Constants.chanVoice soundSearch 1 Constants.attnNorm 0
    return True

brainIdle :: EntThink
brainIdle = EntThink "brain_idle" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle3 <- use (mBrainGlobals.mBrainSoundIdle3)
    sound (Just selfRef) Constants.chanAuto soundIdle3 1 Constants.attnIdle 0
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just brainMoveIdle)
    return True

brainDie :: EntDie
brainDie = EntDie "brain_die" $ \selfRef _ _ damage _ -> do
    modifyRef selfRef (\v -> v & eEntityState.esEffects .~ 0
                               & eMonsterInfo.miPowerArmorType .~ Constants.powerArmorNone)
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    doBrainDie selfRef self gameImport damage

doBrainDie :: Ref' EdictT -> EdictT -> GameImportT -> Int -> Quake ()
doBrainDie selfRef self gameImport damage
    | (self^.eHealth) <= (self^.eGibHealth) = do -- check for gib
        soundIdx <- soundIndex (Just "misc/udeath.wav")
        sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
        replicateM_ 2 (GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic)
        replicateM_ 4 (GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic)
        GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic
        modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
    | (self^.eDeadFlag) == Constants.deadDead = return ()
    | otherwise = do -- regular death
        soundDeath <- use (mBrainGlobals.mBrainSoundDeath)
        sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0
        currentMove <- pickCurrentMove <$> Lib.randomF
        modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                   & eTakeDamage .~ Constants.damageYes
                                   & eMonsterInfo.miCurrentMove .~ Just currentMove)
  where
    sound = gameImport^.giSound
    soundIndex = gameImport^.giSoundIndex
    pickCurrentMove r | r <= 0.5 = brainMoveDeath1
                      | otherwise = brainMoveDeath2

brainMoveStand :: MMoveT
brainMoveStand = MMoveT "brainMoveStand" frameStand01 frameStand30 brainFramesStand Nothing

brainFramesStand :: V.Vector MFrameT
brainFramesStand = V.replicate 30 (MFrameT (Just GameAI.aiStand) 0 Nothing)

brainMoveWalk1 :: MMoveT
brainMoveWalk1 = MMoveT "brainMoveWalk1" frameWalk101 frameWalk111 brainFramesWalk1 Nothing

brainFramesWalk1 :: V.Vector MFrameT
brainFramesWalk1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dist)
  where
    dist = [ 7, 2, 3, 3, 1, 0, 0, 9, -4, -1, 2 ]

brainMoveRun :: MMoveT
brainMoveRun = MMoveT "brainMoveRun" frameWalk101 frameWalk111 brainFramesRun Nothing

brainFramesRun :: V.Vector MFrameT
brainFramesRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dist)
  where
    dist = [ 9, 2, 3, 3, 1, 0, 0, 10, -4, -1, 2 ]

brainMoveDuck :: MMoveT
brainMoveDuck = MMoveT "brainMoveDuck" frameDuck01 frameDuck08 brainFramesDuck (Just brainRun)

brainFramesDuck :: V.Vector MFrameT
brainFramesDuck = V.fromList
    [ MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove) (-2) (Just brainDuckDown)
    , MFrameT (Just GameAI.aiMove)  17  (Just brainDuckHold)
    , MFrameT (Just GameAI.aiMove) (-3) Nothing
    , MFrameT (Just GameAI.aiMove) (-1) (Just brainDuckUp)
    , MFrameT (Just GameAI.aiMove) (-5) Nothing
    , MFrameT (Just GameAI.aiMove) (-6) Nothing
    , MFrameT (Just GameAI.aiMove) (-6) Nothing
    ]

brainMoveIdle :: MMoveT
brainMoveIdle = MMoveT "brainMoveIdle" frameStand31 frameStand60 brainFramesIdle (Just brainStand)

brainFramesIdle :: V.Vector MFrameT
brainFramesIdle = V.replicate 30 (MFrameT (Just GameAI.aiStand) 0 Nothing)

brainMoveAttack1 :: MMoveT
brainMoveAttack1 = MMoveT "brainMoveAttack1" frameAttack101 frameAttack118 brainFramesAttack1 (Just brainRun)

brainFramesAttack1 :: V.Vector MFrameT
brainFramesAttack1 = V.fromList
    [ MFrameT (Just GameAI.aiCharge)    8  Nothing
    , MFrameT (Just GameAI.aiCharge)    3  Nothing
    , MFrameT (Just GameAI.aiCharge)    5  Nothing
    , MFrameT (Just GameAI.aiCharge)    0  Nothing
    , MFrameT (Just GameAI.aiCharge)  (-3) (Just brainSwingRight)
    , MFrameT (Just GameAI.aiCharge)    0  Nothing
    , MFrameT (Just GameAI.aiCharge)  (-5) Nothing
    , MFrameT (Just GameAI.aiCharge)  (-7) (Just brainHitRight)
    , MFrameT (Just GameAI.aiCharge)    0  Nothing
    , MFrameT (Just GameAI.aiCharge)    6  (Just brainSwingLeft)
    , MFrameT (Just GameAI.aiCharge)    1  Nothing
    , MFrameT (Just GameAI.aiCharge)    2  (Just brainHitLeft)
    , MFrameT (Just GameAI.aiCharge)  (-3) Nothing
    , MFrameT (Just GameAI.aiCharge)    6  Nothing
    , MFrameT (Just GameAI.aiCharge)  (-1) Nothing
    , MFrameT (Just GameAI.aiCharge)  (-3) Nothing
    , MFrameT (Just GameAI.aiCharge)    2  Nothing
    , MFrameT (Just GameAI.aiCharge) (-11) Nothing
    ]

brainMoveAttack2 :: MMoveT
brainMoveAttack2 = MMoveT "brainMoveAttack2" frameAttack201 frameAttack217 brainFramesAttack2 (Just brainRun)

brainFramesAttack2 :: V.Vector MFrameT
brainFramesAttack2 = V.fromList
    [ MFrameT (Just GameAI.aiCharge)   5  Nothing
    , MFrameT (Just GameAI.aiCharge) (-4) Nothing
    , MFrameT (Just GameAI.aiCharge) (-4) Nothing
    , MFrameT (Just GameAI.aiCharge) (-3) Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just brainChestOpen)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)  13  (Just brainTentacleAttack)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   2  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge) (-9) (Just brainChestClosed)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   3  Nothing
    , MFrameT (Just GameAI.aiCharge)   2  Nothing
    , MFrameT (Just GameAI.aiCharge) (-3) Nothing
    , MFrameT (Just GameAI.aiCharge) (-6) Nothing
    ]

brainDuckDown :: EntThink
brainDuckDown = EntThink "brain_duck_down" $ \selfRef -> do
    isAlreadyDucked <- checkIfDucked <$> readRef selfRef
    unless isAlreadyDucked $ do
        modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked)
                                   & eMaxs._z -~ 32
                                   & eTakeDamage .~ Constants.damageYes)
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity selfRef
    return True
  where
    checkIfDucked self = (self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked /= 0

brainDuckHold :: EntThink
brainDuckHold = EntThink "brain_duck_hold" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    checkHold selfRef self levelTime
    return True
  where
    checkHold selfRef self levelTime
        | levelTime >= (self^.eMonsterInfo.miPauseTime) =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiHoldFrame)))
        | otherwise =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

brainDuckUp :: EntThink
brainDuckUp = EntThink "brain_duck_up" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiDucked))
                               & eMaxs._z +~ 32
                               & eTakeDamage .~ Constants.damageAim)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

brainSwingRight :: EntThink
brainSwingRight = EntThink "brain_swing_right" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundMelee1 <- use (mBrainGlobals.mBrainSoundMelee1)
    sound (Just selfRef) Constants.chanBody soundMelee1 1 Constants.attnNorm 0
    return True

brainHitRight :: EntThink
brainHitRight = EntThink "brain_hit_right" $ \selfRef -> do
    r <- Lib.rand
    aim <- calcAim <$> readRef selfRef
    hit <- GameWeapon.fireHit selfRef aim (15 + (fromIntegral r `mod` 5)) 40
    when hit $ do
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        soundMelee <- use (mBrainGlobals.mBrainSoundMelee3)
        sound (Just selfRef) Constants.chanWeapon soundMelee 1 Constants.attnNorm 0
    return True
  where
    calcAim self = V3 (fromIntegral Constants.meleeDistance) (self^.eMaxs._x) 8

brainSwingLeft :: EntThink
brainSwingLeft = EntThink "brain_swing_left" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundMelee2 <- use (mBrainGlobals.mBrainSoundMelee2)
    sound (Just selfRef) Constants.chanBody soundMelee2 1 Constants.attnNorm 0
    return True

brainHitLeft :: EntThink
brainHitLeft = EntThink "brain_hit_left" $ \selfRef -> do
    aim <- calcAim <$> readRef selfRef
    r <- Lib.rand
    hit <- GameWeapon.fireHit selfRef aim (15 + (fromIntegral r `mod` 5)) 40
    when hit $ do
      sound <- use (gameBaseGlobals.gbGameImport.giSound)
      soundMelee <- use (mBrainGlobals.mBrainSoundMelee3)
      sound (Just selfRef) Constants.chanWeapon soundMelee 1 Constants.attnNorm 0
    return True
  where
    calcAim self = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) 8

brainChestOpen :: EntThink
brainChestOpen = EntThink "brain_chest_open" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement 65536))
                               & eMonsterInfo.miPowerArmorType .~ Constants.powerArmorNone)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundChestOpen <- use (mBrainGlobals.mBrainSoundChestOpen)
    sound (Just selfRef) Constants.chanBody soundChestOpen 1 Constants.attnNorm 0
    return True

brainChestClosed :: EntThink
brainChestClosed = EntThink "brain_chest_closed" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miPowerArmorType .~ Constants.powerArmorScreen)
    self <- readRef selfRef
    when ((self^.eSpawnFlags) .&. 65536 /= 0) $
        modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement 65536))
                                   & eMonsterInfo.miCurrentMove .~ Just brainMoveAttack1)
    return True

brainTentacleAttack :: EntThink
brainTentacleAttack = EntThink "brain_tentacle_attack" $ \selfRef -> do
    r <- Lib.rand
    hit <- GameWeapon.fireHit selfRef aim (10 + (fromIntegral r `mod` 5)) (-600)
    when hit $
        modifyRef selfRef (\v -> v & eSpawnFlags %~ (.|. 65536))
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundTentaclesRetract <- use (mBrainGlobals.mBrainSoundTentaclesRetract)
    sound (Just selfRef) Constants.chanWeapon soundTentaclesRetract 1 Constants.attnNorm 0
    return True
  where
    aim = V3 (fromIntegral Constants.meleeDistance) 0 8

brainMovePain1 :: MMoveT
brainMovePain1 = MMoveT "brainMovePain1" framePain101 framePain121 brainFramesPain1 (Just brainRun)

brainFramesPain1 :: V.Vector MFrameT
brainFramesPain1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dist)
  where
    dist = [ -6, -2, -6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 1, 7, 0, 3, -1 ]

brainMovePain2 :: MMoveT
brainMovePain2 = MMoveT "brainMovePain2" framePain201 framePain208 brainFramesPain2 (Just brainRun)

brainFramesPain2 :: V.Vector MFrameT
brainFramesPain2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dist)
  where
    dist = [ -2, 0, 0, 0, 0, 3, 1, -2 ]

brainMovePain3 :: MMoveT
brainMovePain3 = MMoveT "brainMovePain3" framePain301 framePain306 brainFramesPain3 (Just brainRun)

brainFramesPain3 :: V.Vector MFrameT
brainFramesPain3 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dist)
  where
    dist = [ -2, 2, 1, 3, 0, -4 ]

brainMoveDeath1 :: MMoveT
brainMoveDeath1 = MMoveT "brainMoveDeath1" frameDeath101 frameDeath118 brainFramesDeath1 (Just brainDead)

brainFramesDeath1 :: V.Vector MFrameT
brainFramesDeath1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dist)
  where
    dist = [ 0, 0, -2, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

brainMoveDeath2 :: MMoveT
brainMoveDeath2 = MMoveT "brainMoveDeath2" frameDeath201 frameDeath205 brainFramesDeath2 (Just brainDead)

brainFramesDeath2 :: V.Vector MFrameT
brainFramesDeath2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dist)
  where
    dist = [ 0, 0, 0, 9, 0 ]

brainDead :: EntThink
brainDead = EntThink "brain_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True