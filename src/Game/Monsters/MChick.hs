module Game.Monsters.MChick
    ( spMonsterChick
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~), (-~), (+~))
import           Control.Monad         (when, unless, void)
import           Data.Bits             (complement, (.&.), (.|.))
import           Data.Int              (Int16)
import           Data.Maybe            (isJust)
import qualified Data.Vector           as V
import           Linear                (V3(..), normalize, _x, _z)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameWeapon       as GameWeapon
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import           Game.MMoveT
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

frameAttack101 :: Int
frameAttack101 = 0

frameAttack113 :: Int
frameAttack113 = 12

frameAttack114 :: Int
frameAttack114 = 13

frameAttack127 :: Int
frameAttack127 = 26

frameAttack128 :: Int
frameAttack128 = 27

frameAttack132 :: Int
frameAttack132 = 31

frameAttack201 :: Int
frameAttack201 = 32

frameAttack203 :: Int
frameAttack203 = 34

frameAttack204 :: Int
frameAttack204 = 35

frameAttack212 :: Int
frameAttack212 = 43

frameAttack213 :: Int
frameAttack213 = 44

frameAttack216 :: Int
frameAttack216 = 47

frameDeath101 :: Int
frameDeath101 = 48

frameDeath112 :: Int
frameDeath112 = 59

frameDeath201 :: Int
frameDeath201 = 60

frameDeath223 :: Int
frameDeath223 = 82

frameDuck01 :: Int
frameDuck01 = 83

frameDuck07 :: Int
frameDuck07 = 89

framePain101 :: Int
framePain101 = 90

framePain105 :: Int
framePain105 = 94

framePain201 :: Int
framePain201 = 95

framePain205 :: Int
framePain205 = 99

framePain301 :: Int
framePain301 = 100

framePain321 :: Int
framePain321 = 120

frameStand101 :: Int
frameStand101 = 121

frameStand130 :: Int
frameStand130 = 150

frameStand201 :: Int
frameStand201 = 151

frameStand230 :: Int
frameStand230 = 180

frameWalk01 :: Int
frameWalk01 = 181

frameWalk10 :: Int
frameWalk10 = 190

frameWalk11 :: Int
frameWalk11 = 191

frameWalk20 :: Int
frameWalk20 = 200

spMonsterChick :: Ref EdictT -> Quake ()
spMonsterChick selfRef = do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterChick selfRef gameImport

proceedSpawnMonsterChick :: Ref EdictT -> GameImportT -> Quake ()
proceedSpawnMonsterChick selfRef gameImport = do
    soundIndex (Just "chick/chkatck1.wav") >>= (mChickGlobals.mChickSoundMissilePrelaunch .=)
    soundIndex (Just "chick/chkatck2.wav") >>= (mChickGlobals.mChickSoundMissileLaunch    .=)
    soundIndex (Just "chick/chkatck3.wav") >>= (mChickGlobals.mChickSoundMeleeSwing       .=)
    soundIndex (Just "chick/chkatck4.wav") >>= (mChickGlobals.mChickSoundMeleeHit         .=)
    soundIndex (Just "chick/chkatck5.wav") >>= (mChickGlobals.mChickSoundMissileReload    .=)
    soundIndex (Just "chick/chkdeth1.wav") >>= (mChickGlobals.mChickSoundDeath1           .=)
    soundIndex (Just "chick/chkdeth2.wav") >>= (mChickGlobals.mChickSoundDeath2           .=)
    soundIndex (Just "chick/chkfall1.wav") >>= (mChickGlobals.mChickSoundFallDown         .=)
    soundIndex (Just "chick/chkidle1.wav") >>= (mChickGlobals.mChickSoundIdle1            .=)
    soundIndex (Just "chick/chkidle2.wav") >>= (mChickGlobals.mChickSoundIdle2            .=)
    soundIndex (Just "chick/chkpain1.wav") >>= (mChickGlobals.mChickSoundPain1            .=)
    soundIndex (Just "chick/chkpain2.wav") >>= (mChickGlobals.mChickSoundPain2            .=)
    soundIndex (Just "chick/chkpain3.wav") >>= (mChickGlobals.mChickSoundPain3            .=)
    soundIndex (Just "chick/chksght1.wav") >>= (mChickGlobals.mChickSoundSight            .=)
    soundIndex (Just "chick/chksrch1.wav") >>= (mChickGlobals.mChickSoundSearch           .=)
    modelIdx <- modelIndex (Just "models/monsters/bitch/tris.md2")
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                               & eSolid .~ Constants.solidBbox
                               & eEntityState.esModelIndex .~ modelIdx
                               & eMins .~ V3 (-16) (-16) 0
                               & eMaxs .~ V3 16 16 56
                               & eHealth .~ 175
                               & eGibHealth .~ (-70)
                               & eMass .~ 200
                               & ePain .~ Just chickPain
                               & eDie .~ Just chickDie
                               & eMonsterInfo.miStand .~ Just chickStand
                               & eMonsterInfo.miWalk .~ Just chickWalk
                               & eMonsterInfo.miRun .~ Just chickRun
                               & eMonsterInfo.miDodge .~ Just chickDodge
                               & eMonsterInfo.miAttack .~ Just chickAttack
                               & eMonsterInfo.miMelee .~ Just chickMelee
                               & eMonsterInfo.miSight .~ Just chickSight)
    linkEntity selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStand
                               & eMonsterInfo.miScale .~ modelScale)
    void (entThink GameAI.walkMonsterStart selfRef)
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity

chickStand :: EntThink
chickStand = EntThink "chick_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStand)
    return True

chickWalk :: EntThink
chickWalk = EntThink "chick_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveWalk)
    return True

chickRun :: EntThink
chickRun = EntThink "chick_run" $ \selfRef -> do
    action <- pickAction <$> readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    pickAction self
        | shouldStandGround self = chickMoveStand
        | otherwise =
            case self^.eMonsterInfo.miCurrentMove of
                Nothing -> chickMoveStartRun
                Just move -> pickMoveAction move
    shouldStandGround self =
        (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
    pickMoveAction move
        | (move^.mmId) `elem` [chickMoveWalk^.mmId, chickMoveStartRun^.mmId] = chickMoveRun
        | otherwise = chickMoveStartRun

chickDodge :: EntDodge
chickDodge = EntDodge "chick_dodge" $ \selfRef attackerRef _ -> do
    r <- Lib.randomF
    unless (r > 0.25) $ do
        self <- readRef selfRef
        when (isJust (self^.eEnemy)) $
            modifyRef selfRef (\v -> v & eEnemy .~ Just attackerRef)
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveDuck)

chickAttack :: EntThink
chickAttack = EntThink "chick_attack" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStartAttack1)
    return True

chickMelee :: EntThink
chickMelee = EntThink "chick_melee" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStartSlash)
    return True

chickSight :: EntInteract
chickSight = EntInteract "chick_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mChickGlobals.mChickSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

chickFramesPain1 :: V.Vector MFrameT
chickFramesPain1 = V.replicate 5 (MFrameT (Just GameAI.aiMove) 0 Nothing)

chickMovePain1 :: MMoveT
chickMovePain1 = MMoveT "chickMovePain1" framePain101 framePain105 chickFramesPain1 (Just chickRun)

chickFramesPain2 :: V.Vector MFrameT
chickFramesPain2 = V.replicate 5 (MFrameT (Just GameAI.aiMove) 0 Nothing)

chickMovePain2 :: MMoveT
chickMovePain2 = MMoveT "chickMovePain2" framePain201 framePain205 chickFramesPain2 (Just chickRun)

chickFramesPain3 :: V.Vector MFrameT
chickFramesPain3 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [0, 0, -6, 3, 11, 3, 0, 0, 4, 1, 0, -3, -4, 5, 7, -2, 3, -5, -2, -8, 2]

chickMovePain3 :: MMoveT
chickMovePain3 = MMoveT "chickMovePain3" framePain301 framePain321 chickFramesPain3 (Just chickRun)

chickPain :: EntPain
chickPain = EntPain "chick_pain" $ \selfRef _ _ damage -> do
    self <- readRef selfRef
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    unless (levelTime < (self^.ePainDebounceTime)) $ do
        modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
        r <- Lib.randomF
        sfx <- getSfx r
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        sound (Just selfRef) Constants.chanVoice sfx 1 Constants.attnNorm 0
        skill <- fmap (^.cvValue) skillCVar
        unless (skill == 3) $ -- no pain anims in nightmare
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (painMove damage))
  where
    getSfx :: Float -> Quake Int
    getSfx r
        | r < 0.33  = use (mChickGlobals.mChickSoundPain1)
        | r < 0.66  = use (mChickGlobals.mChickSoundPain2)
        | otherwise = use (mChickGlobals.mChickSoundPain3)
    painMove damage
        | damage <= 10 = chickMovePain1
        | damage <= 25 = chickMovePain2
        | otherwise    = chickMovePain3

chickFramesDeath1 :: V.Vector MFrameT
chickFramesDeath1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [0, 0, -7, 4, 11, 0, 0, 0, 0, 0, 0, 0]

chickMoveDeath1 :: MMoveT
chickMoveDeath1 = MMoveT "chickMoveDeath1" frameDeath101 frameDeath112 chickFramesDeath1 (Just chickDead)

chickFramesDeath2 :: V.Vector MFrameT
chickFramesDeath2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-6, 0, -1, -5, 0, -1, -2, 1, 10, 2, 3, 1, 2, 0, 3, 3, 1, -3, -5, 4, 15, 14, 1]

chickMoveDeath2 :: MMoveT
chickMoveDeath2 = MMoveT "chickMoveDeath2" frameDeath201 frameDeath223 chickFramesDeath2 (Just chickDead)

chickDie :: EntDie
chickDie = EntDie "chick_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    doChickDie selfRef self damage
  where
    doChickDie selfRef self damage
        | (self^.eHealth) <= (self^.eGibHealth) = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "misc/udeath.wav")
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            -- IMPROVE
            GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        | (self^.eDeadFlag) == Constants.deadDead = return ()
        | otherwise = do -- regular death
            n <- fmap (`mod` 2) Lib.rand
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                       & eTakeDamage .~ Constants.damageYes
                                       & eMonsterInfo.miCurrentMove .~ Just (currentMove n))
            sfx <- getSfx n
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) Constants.chanVoice sfx 1 Constants.attnNorm 0
    currentMove n
        | n == 0    = chickMoveDeath1
        | otherwise = chickMoveDeath2
    getSfx :: Int16 -> Quake Int
    getSfx n
        | n == 0    = use (mChickGlobals.mChickSoundDeath1)
        | otherwise = use (mChickGlobals.mChickSoundDeath2)

chickDead :: EntThink
chickDead = EntThink "chick_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) 0
                               & eMaxs .~ V3 16 16 16
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

chickMoveStand :: MMoveT
chickMoveStand = MMoveT "chickMoveStand" frameStand101 frameStand130 chickFramesStand Nothing

chickFramesStand :: V.Vector MFrameT
chickFramesStand = V.snoc (V.replicate 29 (MFrameT (Just GameAI.aiStand) 0 Nothing))
                          (MFrameT (Just GameAI.aiStand) 0 (Just chickFidget))

chickMoveWalk :: MMoveT
chickMoveWalk = MMoveT "chickMoveWalk" frameWalk11 frameWalk20 chickFramesWalk Nothing

chickFramesWalk :: V.Vector MFrameT
chickFramesWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dist)
  where
    dist = [ 6, 8, 13, 5, 7, 4, 11, 5, 9, 7 ]

chickMoveStartRun :: MMoveT
chickMoveStartRun = MMoveT "chickMoveStartRun" frameWalk01 frameWalk10 chickFramesStartRun (Just chickRun)

chickFramesStartRun :: V.Vector MFrameT
chickFramesStartRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dist)
  where
    dist = [ 1, 0, 0, -1, -1, 0, 1, 3, 6, 3 ]

chickMoveDuck :: MMoveT
chickMoveDuck = MMoveT "chickMoveDuck" frameDuck01 frameDuck07 chickFramesDuck (Just chickRun)

chickFramesDuck :: V.Vector MFrameT
chickFramesDuck = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove)   0  (Just chickDuckDown)
    , MFrameT (Just GameAI.aiMove)   1  Nothing
    , MFrameT (Just GameAI.aiMove)   4  (Just chickDuckHold)
    , MFrameT (Just GameAI.aiMove) (-4) Nothing
    , MFrameT (Just GameAI.aiMove) (-5) (Just chickDuckUp)
    , MFrameT (Just GameAI.aiMove)   3  Nothing
    , MFrameT (Just GameAI.aiMove)   1  Nothing
    ]

chickMoveStartAttack1 :: MMoveT
chickMoveStartAttack1 = MMoveT "chickMoveStartAttack1" frameAttack101 frameAttack113 chickFramesStartAttack1 Nothing

chickFramesStartAttack1 :: V.Vector MFrameT
chickFramesStartAttack1 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge)   0  (Just chickPreAttack1)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge) (-3) Nothing
    , MFrameT (Just GameAI.aiCharge)   3  Nothing
    , MFrameT (Just GameAI.aiCharge)   5  Nothing
    , MFrameT (Just GameAI.aiCharge)   7  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just chickAttack1)
    ]

chickMoveStartSlash :: MMoveT
chickMoveStartSlash = MMoveT "chickMoveStartSlash" frameAttack201 frameAttack203 chickFramesStartSlash (Just chickStartSlash)


chickFramesStartSlash :: V.Vector MFrameT
chickFramesStartSlash = V.fromList
    [ MFrameT (Just GameAI.aiCharge) 1 Nothing
    , MFrameT (Just GameAI.aiCharge) 8 Nothing
    , MFrameT (Just GameAI.aiCharge) 3 Nothing
    ]

chickMoveRun :: MMoveT
chickMoveRun = MMoveT "chickMoveRun" frameWalk11 frameWalk20 chickFramesRun Nothing

chickFramesRun :: V.Vector MFrameT
chickFramesRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dist)
  where
    dist = [ 6, 8, 13, 5, 7, 4, 11, 5, 9, 7 ]

chickFramesFidget :: V.Vector MFrameT
chickFramesFidget = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 Nothing
    , MFrameT (Just GameAI.aiStand) 0 (Just chickMoan)
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

chickMoveFidget :: MMoveT
chickMoveFidget = MMoveT "chickMoveFidget" frameStand201 frameStand230 chickFramesFidget (Just chickStand)

chickFidget :: EntThink
chickFidget = EntThink "chick_fidget" $ \selfRef -> do
    self <- readRef selfRef
    unless ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0) $ do
        r <- Lib.randomF
        when (r <= 0.3) $ 
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveFidget)
    return True

chickMoan :: EntThink
chickMoan = EntThink "ChickMoan" $ \selfRef -> do
    r <- Lib.randomF
    soundIdle <- getSoundIdle r
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True
  where
    getSoundIdle :: Float -> Quake Int
    getSoundIdle r
        | r < 0.5   = use (mChickGlobals.mChickSoundIdle1)
        | otherwise = use (mChickGlobals.mChickSoundIdle2)

chickDuckDown :: EntThink
chickDuckDown = EntThink "chick_duck_down" $ \selfRef -> do
    self <- readRef selfRef
    unless ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked /= 0) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked)
                                   & eMaxs._z -~ 32
                                   & eTakeDamage .~ Constants.damageYes
                                   & eMonsterInfo.miPauseTime .~ levelTime + 1)
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity selfRef
    return True

chickDuckHold :: EntThink
chickDuckHold = EntThink "chick_duck_hold" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    self <- readRef selfRef
    updateEdict selfRef self levelTime
    return True
  where
    updateEdict selfRef self levelTime
        | levelTime >= (self^.eMonsterInfo.miPauseTime) =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. complement Constants.aiHoldFrame))
        | otherwise =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

chickDuckUp :: EntThink
chickDuckUp = EntThink "chick_duck_up" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. complement Constants.aiDucked)
                               & eMaxs._z +~ 32
                               & eTakeDamage .~ Constants.damageAim)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

chickPreAttack1 :: EntThink
chickPreAttack1 = EntThink "Chick_PreAttack1" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundMissilePrelaunch <- use (mChickGlobals.mChickSoundMissilePrelaunch)
    sound (Just selfRef) Constants.chanVoice soundMissilePrelaunch 1 Constants.attnNorm 0
    return True

chickAttack1 :: EntThink
chickAttack1 = EntThink "chick_attack1" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveAttack1)
    return True

chickStartSlash :: EntThink
chickStartSlash = EntThink "chick_slash" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveSlash)
    return True

chickMoveAttack1 :: MMoveT
chickMoveAttack1 = MMoveT "chickMoveAttack1" frameAttack114 frameAttack127 chickFramesAttack1 Nothing

chickFramesAttack1 :: V.Vector MFrameT
chickFramesAttack1 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge)  19  (Just chickRocket)
    , MFrameT (Just GameAI.aiCharge) (-6) Nothing
    , MFrameT (Just GameAI.aiCharge) (-5) Nothing
    , MFrameT (Just GameAI.aiCharge) (-2) Nothing
    , MFrameT (Just GameAI.aiCharge) (-7) Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)  10  (Just chickReload)
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   5  Nothing
    , MFrameT (Just GameAI.aiCharge)   6  Nothing
    , MFrameT (Just GameAI.aiCharge)   6  Nothing
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   3  (Just chickReRocket)
    ]

chickMoveSlash :: MMoveT
chickMoveSlash = MMoveT "chickMoveSlash" frameAttack204 frameAttack212 chickFramesSlash Nothing

chickFramesSlash :: V.Vector MFrameT
chickFramesSlash = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)   7  (Just chickSlash)
    , MFrameT (Just GameAI.aiCharge) (-7) Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge) (-2) (Just chickReSlash)
    ]

chickRocket :: EntThink
chickRocket = EntThink "ChickRocket" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doChickRocket selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MChick.chickRocket self^.eEnemy is Nothing"
    doChickRocket selfRef self enemyRef = do
        enemy <- readRef enemyRef
        let (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
            start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! Constants.mz2ChickRocket1) forward right
            V3 a b c = enemy^.eEntityState.esOrigin
            vec = V3 a b (c + fromIntegral (enemy^.eViewHeight))
            dir = normalize (vec - start)
        Monster.monsterFireRocket selfRef start dir 50 500 Constants.mz2ChickRocket1

chickReload :: EntThink
chickReload = EntThink "ChickReload" $ \selfRef -> do
    soundMissileReload <- use (mChickGlobals.mChickSoundMissileReload)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice soundMissileReload 1 Constants.attnNorm 0
    return True

chickFramesEndAttack1 :: V.Vector MFrameT
chickFramesEndAttack1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiCharge) v Nothing) dists)
  where
    dists = [-3, 0, -6, -4, -2]

chickMoveEndAttack1 :: MMoveT
chickMoveEndAttack1 = MMoveT "chickMoveEndAttack1" frameAttack128 frameAttack132 chickFramesEndAttack1 (Just chickRun)

chickReRocket :: EntThink
chickReRocket = EntThink "chick_rerocket" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doChickReRocket selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MChick.chickReRocket self^.eEnemy is Nothing"
    doChickReRocket selfRef self enemyRef = do
        enemy <- readRef enemyRef
        done <- checkIfAttack selfRef self enemyRef enemy
        unless done $
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveEndAttack1)
    checkIfAttack selfRef self enemyRef enemy
        | (enemy^.eHealth) > 0 && GameUtil.range self enemy > Constants.rangeMelee = do
            visible <- GameUtil.visible selfRef enemyRef
            r <- Lib.randomF
            checkVisibility selfRef visible r
        | otherwise = return False
    checkVisibility selfRef visible r
        | visible && r <= 0.6 = do
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveAttack1)
            return True
        | otherwise = return False

chickSlash :: EntThink
chickSlash = EntThink "ChickSlash" $ \selfRef -> do
    self <- readRef selfRef
    soundMeleeSwing <- use (mChickGlobals.mChickSoundMeleeSwing)
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanWeapon soundMeleeSwing 1 Constants.attnNorm 0
    r <- Lib.rand
    let aim = V3 (fromIntegral Constants.meleeDistance) (self^.eMins._x) 10
    void (GameWeapon.fireHit selfRef aim (10 + fromIntegral (r `mod` 6)) 100)
    return True

chickFramesEndSlash :: V.Vector MFrameT
chickFramesEndSlash =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiCharge) v Nothing) dists)
  where
    dists = [-6, -1, -6, 0]

chickMoveEndSlash :: MMoveT
chickMoveEndSlash = MMoveT "chickMoveEndSlash" frameAttack213 frameAttack216 chickFramesEndSlash (Just chickRun)

chickReSlash :: EntThink
chickReSlash = EntThink "chick_reslash" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doChickReSlash selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MChick.chickReSlash self^.eEnemy is Nothing"
    doChickReSlash selfRef self enemyRef = do
        enemy <- readRef enemyRef
        done <- checkIfSlash selfRef self enemy
        unless done $
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveEndSlash)
    checkIfSlash selfRef self enemy
        |  (enemy^.eHealth) > 0 && GameUtil.range self enemy == Constants.rangeMelee = do
            r <- Lib.randomF
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove r))
            return True
        | otherwise = return False
    currentMove r
        | r <= 0.9  = chickMoveSlash
        | otherwise = chickMoveEndSlash
