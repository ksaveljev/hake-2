module Game.Monsters.MFloat
    ( spMonsterFloater
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~))
import           Control.Monad         (when, unless, void)
import           Data.Bits             ((.&.), (.|.))
import           Data.Int              (Int16)
import qualified Data.Vector           as V
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameCombat       as GameCombat
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameWeapon       as GameWeapon
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import qualified Game.Monster          as Monster
import           Game.MonsterInfoT
import qualified Game.Monsters.MFlash  as MFlash
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

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
floaterSight = EntInteract "floater_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mFloatGlobals.mFloatSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

floaterIdle :: EntThink
floaterIdle = EntThink "floater_idle" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle <- use (mFloatGlobals.mFloatSoundIdle)
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

floaterFireBlaster :: EntThink
floaterFireBlaster = EntThink "floater_fire_blaster" $ \selfRef -> do
    self <- readRef selfRef
    enemyRef <- getEnemyRef (self^.eEnemy)
    enemy <- readRef enemyRef
    let effect | (self^.eEntityState.esFrame) == frameAttack104 || (self^.eEntityState.esFrame) == frameAttack107 = Constants.efHyperblaster
               | otherwise                                                                                        = 0
        (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2FloatBlaster1) forward right
        V3 a b c = enemy^.eEntityState.esOrigin
        end = V3 a b (c + fromIntegral (enemy^.eViewHeight))
        dir = end - start
    Monster.monsterFireBlaster selfRef start dir 1 1000 Constants.mz2FloatBlaster1 effect
    return True
  where
    getEnemyRef Nothing = do
        Com.fatalError "MFloat.floaterFireBlastser self^.eEnemy is Nothing"
        return (Ref (-1))
    getEnemyRef (Just enemyRef) = return enemyRef

floaterFramesStand1 :: V.Vector MFrameT
floaterFramesStand1 = V.replicate 52 (MFrameT (Just GameAI.aiStand) 0 Nothing)

floaterMoveStand1 :: MMoveT
floaterMoveStand1 = MMoveT "floaterMoveStand1" frameStand101 frameStand152 floaterFramesStand1 Nothing

floaterFramesStand2 :: V.Vector MFrameT
floaterFramesStand2 = V.replicate 52 (MFrameT (Just GameAI.aiStand) 0 Nothing)

floaterMoveStand2 :: MMoveT
floaterMoveStand2 = MMoveT "floaterMoveStand2" farmeStand201 frameStand252 floaterFramesStand2 Nothing

floaterStand :: EntThink
floaterStand = EntThink "floater_stand" $ \selfRef -> do
    r <- Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove r))
    return True
  where
    currentMove r
        | r <= 0.5  = floaterMoveStand1
        | otherwise = floaterMoveStand2

floaterFramesActivate :: V.Vector MFrameT
floaterFramesActivate = V.replicate 30 (MFrameT (Just GameAI.aiMove) 0 Nothing)

floaterMoveActivate :: MMoveT
floaterMoveActivate = MMoveT "floaterMoveActivate" frameActivate01 frameActivate31 floaterFramesActivate Nothing

floaterRun :: EntThink
floaterRun = EntThink "floater_run" $ \selfRef -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove self))
    return True
  where
    currentMove self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 = floaterMoveStand1
        | otherwise                                                       = floaterMoveRun

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
floaterWham = EntThink "floater_wham" $ \selfRef -> do
    soundAttack3 <- use (mFloatGlobals.mFloatSoundAttack3)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanWeapon soundAttack3 1 Constants.attnNorm 0
    r <- Lib.rand
    void (GameWeapon.fireHit selfRef (V3 (fromIntegral Constants.meleeDistance) 0 0) (5 + fromIntegral (r `mod` 6)) (-50))
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
floaterZap = EntThink "floater_zap" $ \selfRef -> do
    self <- readRef selfRef
    enemyRef <- getEnemyRef (self^.eEnemy)
    enemy <- readRef enemyRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let dir = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        -- FIXME: use a flash and replace these two lines with the
        -- commented one
        offset = V3 18.5 (-0.9) 10.0
        origin = Math3D.projectSource (self^.eEntityState.esOrigin) offset forward right
        -- origin = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
        sound = gameImport^.giSound
        writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        writeDir = gameImport^.giWriteDir
        multicast = gameImport^.giMulticast
    soundAttack2 <- use (mFloatGlobals.mFloatSoundAttack2)
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
    v3o <- use (globals.gVec3Origin)
    GameCombat.damage enemyRef selfRef selfRef dir (enemy^.eEntityState.esOrigin) v3o (5 + fromIntegral (r `mod` 6)) (-10) Constants.damageEnergy Constants.modUnknown
    return True
  where
    getEnemyRef Nothing = do
        Com.fatalError "MFloat.floaterZap self^.eEnemy is Nothing"
        return (Ref (-1))
    getEnemyRef (Just enemyRef) = return enemyRef

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
floaterFramesDeath = V.replicate 13 (MFrameT (Just GameAI.aiMove) 0 Nothing)

floaterDead :: EntThink
floaterDead = EntThink "floater_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

floaterMoveDeath :: MMoveT
floaterMoveDeath = MMoveT "floaterMoveDeath" frameDeath01 frameDeath13 floaterFramesDeath (Just floaterDead)

floaterFramesPain1 :: V.Vector MFrameT
floaterFramesPain1 = V.replicate 7 (MFrameT (Just GameAI.aiMove) 0 Nothing)

floaterMovePain1 :: MMoveT
floaterMovePain1 = MMoveT "floaterMovePain1" framePain101 framePain107 floaterFramesPain1 (Just floaterRun)

floaterFramesPain2 :: V.Vector MFrameT
floaterFramesPain2 = V.replicate 8 (MFrameT (Just GameAI.aiMove) 0 Nothing)

floaterMovePain2 :: MMoveT
floaterMovePain2 = MMoveT "floaterMovePain2" framePain201 framePain208 floaterFramesPain2 (Just floaterRun)

floaterFramesPain3 :: V.Vector MFrameT
floaterFramesPain3 = V.replicate 12 (MFrameT (Just GameAI.aiMove) 0 Nothing)

floaterMovePain3 :: MMoveT
floaterMovePain3 = MMoveT "floaterMovePain3" framePain301 framePain312 floaterFramesPain3 (Just floaterRun)

floaterFramesWalk :: V.Vector MFrameT
floaterFramesWalk = V.replicate 52 (MFrameT (Just GameAI.aiWalk) 5 Nothing)

floaterMoveWalk :: MMoveT
floaterMoveWalk = MMoveT "floaterMoveWalk" frameStand101 frameStand152 floaterFramesWalk Nothing

floaterFramesRun :: V.Vector MFrameT
floaterFramesRun = V.replicate 52 (MFrameT (Just GameAI.aiRun) 13 Nothing)

floaterMoveRun :: MMoveT
floaterMoveRun = MMoveT "floaterMoveRun" frameStand101 frameStand152 floaterFramesRun Nothing

floaterWalk :: EntThink
floaterWalk = EntThink "floater_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just floaterMoveWalk)
    return True

floaterAttack :: EntThink
floaterAttack = EntThink "floater_attack" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just floaterMoveAttack1)
    return True

floaterMelee :: EntThink
floaterMelee = EntThink "floater_melee" $ \selfRef -> do
    r <- Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove r))
    return True
  where
    currentMove r
        | r < 0.5   = floaterMoveAttack3
        | otherwise = floaterMoveAttack2

floaterPain :: EntPain
floaterPain = EntPain "floater_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        skill <- fmap (^.cvValue) skillCVar
        unless (skill == 3) $ do -- no pain anims in nightmare
            n <- Lib.rand
            (soundPain, currentMove) <- getSoundAndMove n
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)
  where
    getSoundAndMove :: Int16 -> Quake (Int, MMoveT)
    getSoundAndMove n
        | (n + 1) `mod` 3 == 0 = do
            soundPain <- use (mFloatGlobals.mFloatSoundPain1)
            return (soundPain, floaterMovePain1)
        | otherwise = do
            soundPain <- use (mFloatGlobals.mFloatSoundPain2)
            return (soundPain, floaterMovePain2)

floaterDie :: EntDie
floaterDie = EntDie "floater_die" $ \selfRef _ _ _ _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundDeath1 <- use (mFloatGlobals.mFloatSoundDeath1)
    sound (Just selfRef) Constants.chanVoice soundDeath1 1 Constants.attnNorm 0
    GameMisc.becomeExplosion1 selfRef

{-
- QUAKED monster_floater (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterFloater :: Ref EdictT -> Quake ()
spMonsterFloater selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterFloater selfRef gameImport deathmatch

proceedSpawnMonsterFloater :: Ref EdictT -> GameImportT -> Float -> Quake ()
proceedSpawnMonsterFloater selfRef gameImport deathmatch
    | deathmatch /= 0 = GameUtil.freeEdict selfRef
    | otherwise = do
        soundIndex (Just "floater/fltatck2.wav") >>= (mFloatGlobals.mFloatSoundAttack2 .=)
        soundIndex (Just "floater/fltatck3.wav") >>= (mFloatGlobals.mFloatSoundAttack3 .=)
        soundIndex (Just "floater/fltdeth1.wav") >>= (mFloatGlobals.mFloatSoundDeath1 .=)
        soundIndex (Just "floater/fltidle1.wav") >>= (mFloatGlobals.mFloatSoundIdle .=)
        soundIndex (Just "floater/fltpain1.wav") >>= (mFloatGlobals.mFloatSoundPain1 .=)
        soundIndex (Just "floater/fltpain2.wav") >>= (mFloatGlobals.mFloatSoundPain2 .=)
        soundIndex (Just "floater/fltsght1.wav") >>= (mFloatGlobals.mFloatSoundSight .=)
        void (soundIndex (Just "floater/fltatck1.wav"))
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
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove r)
                                   & eMonsterInfo.miScale .~ modelScale)
        void (entThink GameAI.flyMonsterStart selfRef)
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity
    currentMove r
        | r <= 0.5  = floaterMoveStand1
        | otherwise = floaterMoveStand2

