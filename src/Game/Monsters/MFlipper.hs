module Game.Monsters.MFlipper
    ( spMonsterFlipper
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~))
import           Control.Monad         (replicateM_, when, unless, void)
import           Data.Bits             ((.|.))
import           Data.Int              (Int16)
import qualified Data.Vector           as V
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameWeapon       as GameWeapon
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import           Game.MonsterInfoT
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib

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
flipperFramesStand = V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing ]

flipperMoveStand :: MMoveT
flipperMoveStand = MMoveT "flipperMoveStand" frameFlphor01 frameFlphor01 flipperFramesStand Nothing

flipperStand :: EntThink
flipperStand = EntThink "flipper_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveStand)
    return True

flipperFramesRun :: V.Vector MFrameT
flipperFramesRun = V.replicate 24 (MFrameT (Just GameAI.aiRun) flipperRunSpeed Nothing)

flipperMoveRunLoop :: MMoveT
flipperMoveRunLoop = MMoveT "flipperMoveRunLoop" frameFlpver06 frameFlpver29 flipperFramesRun Nothing

flipperRunLoop :: EntThink
flipperRunLoop = EntThink "flipper_run_loop" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveRunLoop)
    return True

flipperFramesRunStart :: V.Vector MFrameT
flipperFramesRunStart = V.replicate 6 (MFrameT (Just GameAI.aiRun) 8 Nothing)

flipperMoveRunStart :: MMoveT
flipperMoveRunStart = MMoveT "flipperMoveRunStart" frameFlpver01 frameFlpver06 flipperFramesRunStart (Just flipperRunLoop)

flipperRun :: EntThink
flipperRun = EntThink "flipper_run" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveRunStart)
    return True

flipperFramesWalk :: V.Vector MFrameT
flipperFramesWalk = V.replicate 24 (MFrameT (Just GameAI.aiWalk) 4 Nothing)

flipperMoveWalk :: MMoveT
flipperMoveWalk = MMoveT "flipperMoveWalk" frameFlphor01 frameFlphor24 flipperFramesWalk Nothing

flipperWalk :: EntThink
flipperWalk = EntThink "flipper_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveWalk)
    return True

flipperFramesStartRun :: V.Vector MFrameT
flipperFramesStartRun =
    V.fromList [ MFrameT (Just GameAI.aiRun) 8 Nothing -- IMPROVE
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 Nothing
               , MFrameT (Just GameAI.aiRun) 8 (Just flipperRun)
               ]

flipperMoveStartRun :: MMoveT
flipperMoveStartRun = MMoveT "flipperMoveStartRun" frameFlphor01 frameFlphor05 flipperFramesStartRun Nothing

flipperStartRun :: EntThink
flipperStartRun = EntThink "flipper_start_run" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveStartRun)
    return True

flipperFramesPain2 :: V.Vector MFrameT
flipperFramesPain2 = V.replicate 5 (MFrameT (Just GameAI.aiMove) 0 Nothing)

flipperMovePain2 :: MMoveT
flipperMovePain2 = MMoveT "flipperMovePain2" frameFlppn101 frameFlppn105 flipperFramesPain2 (Just flipperRun)

flipperFramesPain1 :: V.Vector MFrameT
flipperFramesPain1 = V.replicate 5 (MFrameT (Just GameAI.aiMove) 0 Nothing)

flipperMovePain1 :: MMoveT
flipperMovePain1 = MMoveT "flipperMovePain1" frameFlppn201 frameFlppn205 flipperFramesPain1 (Just flipperRun)

flipperBite :: EntThink
flipperBite = EntThink "flipper_bite" $ \selfRef -> do
    void (GameWeapon.fireHit selfRef (V3 (fromIntegral Constants.meleeDistance) 0 0) 5 0)
    return True

flipperPreAttack :: EntThink
flipperPreAttack = EntThink "flipper_preattack" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundChomp <- use (mFlipperGlobals.mFlipperSoundChomp)
    sound (Just selfRef) Constants.chanWeapon soundChomp 1 Constants.attnNorm 0
    return True

flipperFramesAttack :: V.Vector MFrameT
flipperFramesAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 (Just flipperPreAttack) -- IMPROVE
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
flipperMelee = EntThink "flipper_melee" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveAttack)
    return True

flipperPain :: EntPain
flipperPain = EntPain "flipper_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        skill <- fmap (^.cvValue) skillCVar
        unless (skill == 3) $ do -- no pain anims in nightmare
            n <- Lib.rand
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            (soundPain, currentMove) <- getSoundAndMove n
            sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
  where
    getSoundAndMove :: Int16 -> Quake (Int, MMoveT)
    getSoundAndMove n
        | (n + 1) `mod` 2 == 0 = do
            soundPain <- use (mFlipperGlobals.mFlipperSoundPain1)
            return (soundPain, flipperMovePain1)
        | otherwise = do
            soundPain <- use (mFlipperGlobals.mFlipperSoundPain2)
            return (soundPain, flipperMovePain2)

flipperDead :: EntThink
flipperDead = EntThink "flipper_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

flipperFramesDeath :: V.Vector MFrameT
flipperFramesDeath = V.replicate 56 (MFrameT (Just GameAI.aiMove) 0 Nothing)

flipperMoveDeath :: MMoveT
flipperMoveDeath = MMoveT "flipperMoveDeath" frameFlpdth01 frameFlpdth56 flipperFramesDeath (Just flipperDead)

flipperSight :: EntInteract
flipperSight = EntInteract "flipper_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mFlipperGlobals.mFlipperSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

flipperDie :: EntDie
flipperDie = EntDie "flipper_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    doFlipperDie selfRef self damage
  where
    doFlipperDie selfRef self damage
        | (self^.eHealth) <= (self^.eGibHealth) = do -- check for gib
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "misc/udeath.wav")
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            replicateM_ 2 (GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic)
            replicateM_ 2 (GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic)
            GameMisc.throwHead selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        | (self^.eDeadFlag) == Constants.deadDead =
            return ()
        | otherwise = do -- regular death
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            soundDeath <- use (mFlipperGlobals.mFlipperSoundDeath)
            sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                       & eTakeDamage .~ Constants.damageYes
                                       & eMonsterInfo.miCurrentMove .~ Just flipperMoveDeath)

spMonsterFlipper :: Ref EdictT -> Quake ()
spMonsterFlipper selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterFlipper selfRef gameImport deathmatch

proceedSpawnMonsterFlipper :: Ref EdictT -> GameImportT -> Float -> Quake ()
proceedSpawnMonsterFlipper selfRef gameImport deathmatch
    | deathmatch /= 0 = GameUtil.freeEdict selfRef
    | otherwise = do
        soundIndex (Just "flipper/flppain1.wav") >>= (mFlipperGlobals.mFlipperSoundPain1 .=)
        soundIndex (Just "flipper/flppain2.wav") >>= (mFlipperGlobals.mFlipperSoundPain2 .=)
        soundIndex (Just "flipper/flpdeth1.wav") >>= (mFlipperGlobals.mFlipperSoundDeath .=)
        soundIndex (Just "flipper/flpatck1.wav") >>= (mFlipperGlobals.mFlipperSoundChomp .=)
        soundIndex (Just "flipper/flpatck2.wav") >>= (mFlipperGlobals.mFlipperSoundAttack .=)
        soundIndex (Just "flipper/flpidle1.wav") >>= (mFlipperGlobals.mFlipperSoundIdle .=)
        soundIndex (Just "flipper/flpsrch1.wav") >>= (mFlipperGlobals.mFlipperSoundSearch .=)
        soundIndex (Just "flipper/flpsght1.wav") >>= (mFlipperGlobals.mFlipperSoundSight .=)
        modelIdx <- modelIndex (Just "models/monsters/flipper/tris.md2")
        modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
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
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just flipperMoveStand
                                   & eMonsterInfo.miScale .~ modelScale)
        void (entThink GameAI.swimMonsterStart selfRef)
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity

