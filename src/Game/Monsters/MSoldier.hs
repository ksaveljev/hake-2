module Game.Monsters.MSoldier
  ( spMonsterSoldier
  , spMonsterSoldierLight
  , spMonsterSoldierSS
  ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~), (+~), (-~))
import           Control.Monad         (void, when, unless)
import           Data.Bits             (complement, (.|.), (.&.))
import qualified Data.ByteString       as B
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as UV
import           Linear                (V3(..), normalize, _z)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import qualified Game.GameMisc         as GameMisc
import           Game.MMoveT
import qualified Game.Monster          as Monster
import           Game.MonsterInfoT
import qualified Game.Monsters.MFlash  as MFlash
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary           (encode)
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

modelScale :: Float
modelScale = 1.20000

frameAttak101 :: Int
frameAttak101 = 0

frameAttak102 :: Int
frameAttak102 = 1

frameAttak110 :: Int
frameAttak110 = 9

frameAttak112 :: Int
frameAttak112 = 11

frameAttak201 :: Int
frameAttak201 = 12

frameAttak204 :: Int
frameAttak204 = 15

frameAttak216 :: Int
frameAttak216 = 27

frameAttak218 :: Int
frameAttak218 = 29

frameAttak301 :: Int
frameAttak301 = 30

frameAttak303 :: Int
frameAttak303 = 32

frameAttak309 :: Int
frameAttak309 = 38

frameAttak401 :: Int
frameAttak401 = 39

frameAttak406 :: Int
frameAttak406 = 44

frameDuck01 :: Int
frameDuck01 = 45

frameDuck05 :: Int
frameDuck05 = 49

framePain101 :: Int
framePain101 = 50

framePain105 :: Int
framePain105 = 54

framePain201 :: Int
framePain201 = 55

framePain207 :: Int
framePain207 = 61

framePain301 :: Int
framePain301 = 62

framePain318 :: Int
framePain318 = 79

framePain401 :: Int
framePain401 = 80

framePain417 :: Int
framePain417  = 96

frameRun01 :: Int
frameRun01 = 97

frameRun02 :: Int
frameRun02 = 98

frameRun03 :: Int
frameRun03 = 99

frameRun08 :: Int
frameRun08 = 104

frameRuns01 :: Int
frameRuns01 = 109

frameRuns03 :: Int
frameRuns03 = 111

frameRuns14 :: Int
frameRuns14 = 122

frameStand101 :: Int
frameStand101 = 146

frameStand130 :: Int
frameStand130 = 175

frameStand301 :: Int
frameStand301 = 176

frameStand322 :: Int
frameStand322 = 197

frameStand339 :: Int
frameStand339 = 214

frameWalk101 :: Int
frameWalk101 = 215

frameWalk133 :: Int
frameWalk133 = 247

frameWalk209 :: Int
frameWalk209 = 256

frameWalk218 :: Int
frameWalk218 = 265

frameDeath101 :: Int
frameDeath101 = 272

frameDeath136 :: Int
frameDeath136 = 307

frameDeath201 :: Int
frameDeath201 = 308

frameDeath235 :: Int
frameDeath235 = 342

frameDeath301 :: Int
frameDeath301 = 343

frameDeath345 :: Int
frameDeath345 = 387

frameDeath401 :: Int
frameDeath401 = 388

frameDeath453 :: Int
frameDeath453 = 440

frameDeath501 :: Int
frameDeath501 = 441

frameDeath524 :: Int
frameDeath524 = 464

frameDeath601 :: Int
frameDeath601 = 465

frameDeath610 :: Int
frameDeath610 = 474

blasterFlash :: UV.Vector Int
blasterFlash = UV.fromList
    [ Constants.mz2SoldierBlaster1
    , Constants.mz2SoldierBlaster2
    , Constants.mz2SoldierBlaster3
    , Constants.mz2SoldierBlaster4
    , Constants.mz2SoldierBlaster5
    , Constants.mz2SoldierBlaster6
    , Constants.mz2SoldierBlaster7
    , Constants.mz2SoldierBlaster8
    ]

shotgunFlash :: UV.Vector Int
shotgunFlash = UV.fromList
    [ Constants.mz2SoldierShotgun1
    , Constants.mz2SoldierShotgun2
    , Constants.mz2SoldierShotgun3
    , Constants.mz2SoldierShotgun4
    , Constants.mz2SoldierShotgun5
    , Constants.mz2SoldierShotgun6
    , Constants.mz2SoldierShotgun7
    , Constants.mz2SoldierShotgun8
    ]

machinegunFlash :: UV.Vector Int
machinegunFlash = UV.fromList
    [ Constants.mz2SoldierMachinegun1
    , Constants.mz2SoldierMachinegun2
    , Constants.mz2SoldierMachinegun3
    , Constants.mz2SoldierMachinegun4
    , Constants.mz2SoldierMachinegun5
    , Constants.mz2SoldierMachinegun6
    , Constants.mz2SoldierMachinegun7
    , Constants.mz2SoldierMachinegun8
    ]

spMonsterSoldier :: EntThink
spMonsterSoldier = EntThink "SP_monster_soldier" $ \selfRef -> do
    self <- readRef selfRef
    Com.dprintf ("Spawning a soldier at " `B.append` encode (self^.eEntityState.esOrigin))
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnMonsterSoldier selfRef deathmatch
    return True
  where
    doSpawnMonsterSoldier selfRef deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            void (entThink spMonsterSoldierX selfRef)
            soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
            soundIndex (Just "soldier/solpain1.wav") >>= (mSoldierGlobals.msSoundPain .=)
            soundIndex (Just "soldier/soldeth1.wav") >>= (mSoldierGlobals.msSoundDeath .=)
            void (soundIndex (Just "soldier/solatck1.wav"))
            modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 2
                                       & eHealth .~ 30
                                       & eGibHealth .~ (-30))

spMonsterSoldierSS :: EntThink
spMonsterSoldierSS = EntThink "SP_monster_soldier_ss" $ \selfRef -> do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnMonsterSoldierSS selfRef deathmatch
    return True
  where
    doSpawnMonsterSoldierSS selfRef deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            void (entThink spMonsterSoldierX selfRef)
            soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
            soundIndex (Just "soldier/solpain3.wav") >>= (mSoldierGlobals.msSoundPainSS .=)
            soundIndex (Just "soldier/soldeth3.wav") >>= (mSoldierGlobals.msSoundDeathSS .=)
            void (soundIndex (Just "soldier/solatck3.wav"))
            modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 4
                                       & eHealth .~ 40
                                       & eGibHealth .~ (-30))

spMonsterSoldierLight :: EntThink
spMonsterSoldierLight = EntThink "SP_monster_soldier_light" $ \selfRef -> do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    monsterSoldierLight selfRef deathmatch
    return True
  where
    monsterSoldierLight selfRef deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            void (entThink spMonsterSoldierX selfRef)
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let soundIndex = gameImport^.giSoundIndex
                modelIndex = gameImport^.giModelIndex
            soundIndex (Just "soldier/solpain2.wav") >>= (mSoldierGlobals.msSoundPainLight .=)
            soundIndex (Just "soldier/soldeth2.wav") >>= (mSoldierGlobals.msSoundDeathLight .=)
            void (modelIndex (Just "models/objects/laser/tris.md2"))
            void (soundIndex (Just "misc/lasfly.wav"))
            void (soundIndex (Just "soldier/solatck2.wav"))
            modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 0
                                       & eHealth .~ 20
                                       & eGibHealth .~ (-30))

spMonsterSoldierX :: EntThink
spMonsterSoldierX = EntThink "SP_monster_soldier_x" $ \selfRef -> do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity
    soundIndex (Just "soldier/solidle1.wav") >>= (mSoldierGlobals.msSoundIdle .=)
    soundIndex (Just "soldier/solsght1.wav") >>= (mSoldierGlobals.msSoundSight1 .=)
    soundIndex (Just "soldier/solsrch1.wav") >>= (mSoldierGlobals.msSoundSight2 .=)
    soundIndex (Just "infantry/infatck3.wav") >>= (mSoldierGlobals.msSoundCock .=)
    modelIdx <- modelIndex (Just "models/monsters/soldier/tris.md2")
    modifyRef selfRef (\v -> v & eEntityState.esModelIndex .~ modelIdx
                               & eMonsterInfo.miScale .~ modelScale
                               & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 32
                               & eMoveType .~ Constants.moveTypeStep
                               & eSolid .~ Constants.solidBbox
                               & eMass .~ 100
                               & ePain .~ Just soldierPain
                               & eDie  .~ Just soldierDie
                               & eMonsterInfo.miStand  .~ Just soldierStand
                               & eMonsterInfo.miWalk   .~ Just soldierWalk
                               & eMonsterInfo.miRun    .~ Just soldierRun
                               & eMonsterInfo.miDodge  .~ Just soldierDodge
                               & eMonsterInfo.miAttack .~ Just soldierAttack
                               & eMonsterInfo.miMelee  .~ Nothing
                               & eMonsterInfo.miSight  .~ Just soldierSight)
    linkEntity selfRef
    void (entThink soldierStand selfRef)
    void (entThink GameAI.walkMonsterStart selfRef)
    return True

soldierFramesPain1 :: V.Vector MFrameT
soldierFramesPain1 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-3, 4, 1, 1, 0]

soldierMovePain1 :: MMoveT
soldierMovePain1 = MMoveT "soldierMovePain1" framePain101 framePain105 soldierFramesPain1 (Just soldierRun)

soldierFramesPain2 :: V.Vector MFrameT
soldierFramesPain2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-13, -1, 2, 4, 2, 3, 2]

soldierMovePain2 :: MMoveT
soldierMovePain2 = MMoveT "soldierMovePain2" framePain201 framePain207 soldierFramesPain2 (Just soldierRun)

soldierFramesPain3 :: V.Vector MFrameT
soldierFramesPain3 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [-8, 10, -4, -1, -3, 0, 3, 0, 0, 0, 0, 1, 0, 1, 2, 4, 3, 2]

soldierMovePain3 :: MMoveT
soldierMovePain3 = MMoveT "soldierMovePain3" framePain301 framePain318 soldierFramesPain3 (Just soldierRun)

soldierFramesPain4 :: V.Vector MFrameT
soldierFramesPain4 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = [0, 0, 0, -10, -6, 8, 4, 1, 0, 2, 5, 2, -1, -1, 3, 2, 0]

soldierMovePain4 :: MMoveT
soldierMovePain4 = MMoveT "soldierMovePain4" framePain401 framePain417 soldierFramesPain4 (Just soldierRun)

soldierPain :: EntPain
soldierPain = EntPain "soldier_pain" $ \selfRef _ _ _ -> do
    self <- readRef selfRef
    when ((self^.eHealth) < ((self^.eMaxHealth) `div` 2)) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum %~ (.|. 1))
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    doSoldierPain selfRef self levelTime
  where
    doSoldierPain selfRef self levelTime
        | levelTime < (self^.ePainDebounceTime) =
            when ((self^.eVelocity._z) > 100) $ do
                setPainMove selfRef (self^.eMonsterInfo.miCurrentMove)
        | otherwise = do
            modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
            let n = (self^.eEntityState.esSkinNum) .|. 1
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            soundPainLight <- use (mSoldierGlobals.msSoundPainLight)
            soundPain <- use (mSoldierGlobals.msSoundPain)
            soundPainSS <- use (mSoldierGlobals.msSoundPainSS)
            let s | n == 1    = soundPainLight
                  | n == 3    = soundPain
                  | otherwise = soundPainSS
            sound (Just selfRef) Constants.chanVoice s 1 Constants.attnNorm 0
            setMove selfRef (self^.eVelocity._z)
    setPainMove _ Nothing = return ()
    setPainMove selfRef (Just move) =
        when (any (== (move^.mmId)) ["soldierMovePain1", "soldierMovePain2", "soldierMovePain3"]) $
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just soldierMovePain4)
    setMove selfRef velocity
        | velocity > 100 =
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just soldierMovePain4)
        | otherwise = do
            skill <- fmap (^.cvValue) skillCVar
            -- no pain anims in nightmare
            unless (skill == 3) $ do
                r <- Lib.randomF
                let nextMove | r < 0.33  = soldierMovePain1
                             | r < 0.66  = soldierMovePain2
                             | otherwise = soldierMovePain3
                modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just nextMove)

soldierFramesDeath1 :: V.Vector MFrameT
soldierFramesDeath1 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove) (-10) Nothing
    , MFrameT (Just GameAI.aiMove) (-10) Nothing
    , MFrameT (Just GameAI.aiMove) (-10) Nothing
    , MFrameT (Just GameAI.aiMove)  (-5) Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  (Just soldierFire6)
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  (Just soldierFire7)
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    ]

soldierMoveDeath1 :: MMoveT
soldierMoveDeath1 = MMoveT "soldierMoveDeath1" frameDeath101 frameDeath136 soldierFramesDeath1 (Just soldierDead)

soldierFramesDeath2 :: V.Vector MFrameT
soldierFramesDeath2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = replicate 3 (-5) ++ replicate 32 0

soldierMoveDeath2 :: MMoveT
soldierMoveDeath2 = MMoveT "soldierMoveDeath2" frameDeath201 frameDeath235 soldierFramesDeath2 (Just soldierDead)

soldierFramesDeath3 :: V.Vector MFrameT
soldierFramesDeath3 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = replicate 3 (-5) ++ replicate 42 0

soldierMoveDeath3 :: MMoveT
soldierMoveDeath3 = MMoveT "soldierMoveDetah3" frameDeath301 frameDeath345 soldierFramesDeath3 (Just soldierDead)

soldierFramesDeath4 :: V.Vector MFrameT
soldierFramesDeath4 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = replicate 53 0

soldierMoveDeath4 :: MMoveT
soldierMoveDeath4 = MMoveT "soldierMoveDeath4" frameDeath401 frameDeath453 soldierFramesDeath4 (Just soldierDead)

soldierFramesDeath5 :: V.Vector MFrameT
soldierFramesDeath5 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = replicate 3 (-5) ++ replicate 21 0

soldierMoveDeath5 :: MMoveT
soldierMoveDeath5 = MMoveT "soldierMoveDeath5" frameDeath501 frameDeath524 soldierFramesDeath5 (Just soldierDead)

soldierFramesDeath6 :: V.Vector MFrameT
soldierFramesDeath6 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiMove) v Nothing) dists)
  where
    dists = replicate 10 0

soldierMoveDeath6 :: MMoveT
soldierMoveDeath6 = MMoveT "soldierMoveDeath6" frameDeath601 frameDeath610 soldierFramesDeath6 (Just soldierDead)

soldierDead :: EntThink
soldierDead = EntThink "soldier_dead" $ \selfRef -> do
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 (-8)
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity selfRef
    return True
        
soldierDie :: EntDie
soldierDie = EntDie "soldier_die" $ \selfRef _ _ damage point -> do
    self <- readRef selfRef
    doSoldierDie selfRef self damage point
  where
    doSoldierDie selfRef self damage point
        | (self^.eHealth) <= (self^.eGibHealth) = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "misc/udeath.wav")
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic -- IMPROVE
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/chest/tris.md2" damage Constants.gibOrganic
            GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        | (self^.eDeadFlag) == Constants.deadDead = return ()
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            -- regular death
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                       & eTakeDamage .~ Constants.damageYes
                                       & eEntityState.esSkinNum %~ (.|. 1))
            updatedSelf <- readRef selfRef
            soundIdx <- getDeathSound updatedSelf
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            setMove selfRef updatedSelf point
    getDeathSound :: EdictT -> Quake Int
    getDeathSound self
        | (self^.eEntityState.esSkinNum) == 1 = use (mSoldierGlobals.msSoundDeathLight)
        | (self^.eEntityState.esSkinNum) == 3 = use (mSoldierGlobals.msSoundDeath)
        | otherwise                           = use (mSoldierGlobals.msSoundDeathSS)
    setMove selfRef self point
        | abs (((self^.eEntityState.esOrigin._z) + fromIntegral (self^.eViewHeight)) - (point^._z)) <= 4 =
            -- head shot
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just soldierMoveDeath3)
        | otherwise = do
            n <- fmap (`mod` 5) Lib.rand
            let currentMove | n == 0    = soldierMoveDeath1
                            | n == 1    = soldierMoveDeath2
                            | n == 2    = soldierMoveDeath4
                            | n == 3    = soldierMoveDeath5
                            | otherwise = soldierMoveDeath6
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

soldierStand :: EntThink
soldierStand = EntThink "soldier_stand" $ \selfRef -> do
    self <- readRef selfRef
    r <- Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (getNextMove r (self^.eMonsterInfo.miCurrentMove)))
    return True
  where
    getNextMove r Nothing
        | r < 0.8   = soldierMoveStand1
        | otherwise = soldierMoveStand3
    getNextMove r (Just move)
        | (move^.mmId) == "soldierMoveStand3" || r < 0.8 = soldierMoveStand1
        | otherwise                                     = soldierMoveStand3

soldierFramesWalk1 :: V.Vector MFrameT
soldierFramesWalk1 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiWalk)   3  Nothing
    , MFrameT (Just GameAI.aiWalk)   6  Nothing
    , MFrameT (Just GameAI.aiWalk)   2  Nothing
    , MFrameT (Just GameAI.aiWalk)   2  Nothing
    , MFrameT (Just GameAI.aiWalk)   2  Nothing
    , MFrameT (Just GameAI.aiWalk)   1  Nothing
    , MFrameT (Just GameAI.aiWalk)   6  Nothing
    , MFrameT (Just GameAI.aiWalk)   5  Nothing
    , MFrameT (Just GameAI.aiWalk)   3  Nothing
    , MFrameT (Just GameAI.aiWalk) (-1) (Just soldierWalk1Random)
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    , MFrameT (Just GameAI.aiWalk)   0  Nothing
    ]

soldierMoveWalk1 :: MMoveT
soldierMoveWalk1 = MMoveT "soldierMoveWalk1" frameWalk101 frameWalk133 soldierFramesWalk1 Nothing

soldierFramesWalk2 :: V.Vector MFrameT
soldierFramesWalk2 =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dists)
  where
    dists = [4, 4, 9, 8, 5, 1, 3, 7, 6, 7]

soldierMoveWalk2 :: MMoveT
soldierMoveWalk2 = MMoveT "soldierMoveWalk2" frameWalk209 frameWalk218 soldierFramesWalk2 Nothing

soldierWalk1Random :: EntThink
soldierWalk1Random = EntThink "soldier_walk1_random" $ \selfRef -> do
    r <- Lib.randomF
    when (r > 0.1) $
        modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameWalk101)
    return True

soldierWalk :: EntThink
soldierWalk = EntThink "soldier_walk" $ \selfRef -> do
    r <- Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (nextMove r))
    return True
  where
    nextMove r
        | r < 0.5   = soldierMoveWalk1
        | otherwise = soldierMoveWalk2

soldierFramesRun :: V.Vector MFrameT
soldierFramesRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dists)
  where
    dists = [10, 11, 11, 16, 10, 15]

soldierFramesStartRun :: V.Vector MFrameT
soldierFramesStartRun = V.fromList
    [ MFrameT (Just GameAI.aiRun) 7 Nothing
    , MFrameT (Just GameAI.aiRun) 5 Nothing
    ]

soldierMoveStartRun :: MMoveT
soldierMoveStartRun = MMoveT "soldierMoveStartRun" frameRun01 frameRun02 soldierFramesStartRun (Just soldierRun)

soldierMoveRun :: MMoveT
soldierMoveRun = MMoveT "soldierMoveRun" frameRun03 frameRun08 soldierFramesRun Nothing

soldierRun :: EntThink
soldierRun = EntThink "soldier_run" $ \selfRef -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (nextMove self))
    return True
  where
    nextMove self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 = soldierMoveStand1
        | otherwise = getNextMove (self^.eMonsterInfo.miCurrentMove)
    getNextMove Nothing = soldierMoveStartRun
    getNextMove (Just move)
        | any (== (move^.mmId)) ["soldierMoveWalk1", "soldierMoveWalk2", "soldierMoveStartRun"] = soldierMoveRun
        | otherwise = soldierMoveStartRun

soldierFramesDuck :: V.Vector MFrameT
soldierFramesDuck = V.fromList
    [ MFrameT (Just GameAI.aiMove)   5  (Just soldierDuckDown)
    , MFrameT (Just GameAI.aiMove) (-1) (Just soldierDuckHold)
    , MFrameT (Just GameAI.aiMove)   1  Nothing
    , MFrameT (Just GameAI.aiMove)   0  (Just soldierDuckUp)
    , MFrameT (Just GameAI.aiMove)   5  Nothing
    ]

soldierMoveDuck :: MMoveT
soldierMoveDuck = MMoveT "soldierMoveDuck" frameDuck01 frameDuck05 soldierFramesDuck (Just soldierRun)

soldierDodge :: EntDodge
soldierDodge = EntDodge "soldier_dodge" $ \selfRef attackerRef eta -> do
    r <- Lib.randomF
    when (r > 0.25) $ do
        self <- readRef selfRef
        when (isNothing (self^.eEnemy)) $
            modifyRef selfRef (\v -> v & eEnemy .~ Just attackerRef)
        skill <- fmap (^.cvValue) skillCVar
        nextMove <- getNextMove selfRef skill eta
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just nextMove)
  where
    getNextMove selfRef skill eta
        | skill == 0 = return soldierMoveDuck
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + eta + 0.3)
            r <- Lib.randomF
            return (doGetNextMove skill r)
    doGetNextMove skill r
        | skill == 1 = if r > 0.33 then soldierMoveDuck else soldierMoveAttack3
        | skill >= 2 = if r > 0.66 then soldierMoveDuck else soldierMoveAttack3
        | otherwise  = soldierMoveAttack3

soldierAttack :: EntThink
soldierAttack = EntThink "soldier_attack" $ \selfRef -> do
    self <- readRef selfRef
    nextMove <- getNextMove (self^.eEntityState.esSkinNum)
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just nextMove)
    return True
  where
    getNextMove skinNum
        | skinNum < 4 = do
            v <- Lib.randomF
            return (if v < 0.5 then soldierMoveAttack1 else soldierMoveAttack2)
        | otherwise =
            return soldierMoveAttack4

soldierSight :: EntInteract
soldierSight = EntInteract "soldier_sight" $ \selfRef _ -> do
    self <- readRef selfRef
    maybe enemyError (doSoldierSight selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MSoldier.soldierSight self^.eEnemy is Nothing"
    doSoldierSight selfRef self enemyRef = do
        enemy <- readRef enemyRef
        r <- Lib.randomF
        skill <- fmap (^.cvValue) skillCVar
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        soundSight1 <- use (mSoldierGlobals.msSoundSight1)
        soundSight2 <- use (mSoldierGlobals.msSoundSight2)
        sound (Just selfRef) Constants.chanVoice (soundSight soundSight1 soundSight2 r) 1 Constants.attnNorm 0
        when (skill > 0 && GameUtil.range self enemy >= Constants.rangeMid && r > 0.5) $
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just soldierMoveAttack6)
    soundSight soundSight1 soundSight2 r
        | r < 0.5   = soundSight1
        | otherwise = soundSight2

soldierIdle :: EntThink
soldierIdle = EntThink "soldier_idle" $ \selfRef -> do
    r <- Lib.randomF
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle <- use (mSoldierGlobals.msSoundIdle)
    when (r > 0.8) $
        sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

soldierCock :: EntThink
soldierCock = EntThink "soldier_cock" $ \selfRef -> do
    self <- readRef selfRef
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundCock <- use (mSoldierGlobals.msSoundCock)
    sound (Just selfRef) Constants.chanWeapon soundCock 1 (getAttn (self^.eEntityState.esFrame)) 0
    return True
  where
    getAttn frame
        | frame == frameStand322 = Constants.attnIdle
        | otherwise              = Constants.attnNorm

soldierFramesStand1 :: V.Vector MFrameT
soldierFramesStand1 = V.fromList $
    [ MFrameT (Just GameAI.aiStand) 0 (Just soldierIdle) ]
    ++ replicate 29 (MFrameT (Just GameAI.aiStand) 0 Nothing)

soldierMoveStand1 :: MMoveT
soldierMoveStand1 = MMoveT "soldierMoveStand1" frameStand101 frameStand130 soldierFramesStand1 (Just soldierStand)

soldierFramesStand3 :: V.Vector MFrameT
soldierFramesStand3 = V.fromList $
    replicate 21 (MFrameT (Just GameAI.aiStand) 0 Nothing)
    ++ [ MFrameT (Just GameAI.aiStand) 0 (Just soldierCock) ]
    ++ replicate 17 (MFrameT (Just GameAI.aiStand) 0 Nothing)

soldierMoveStand3 :: MMoveT
soldierMoveStand3 = MMoveT "soldierMoveStand3" frameStand301 frameStand339 soldierFramesStand3 (Just soldierStand)

soldierFire1 :: EntThink
soldierFire1 = EntThink "soldier_fire1" $ \selfRef -> do
    soldierFire selfRef 0
    return True

soldierFire2 :: EntThink
soldierFire2 = EntThink "soldier_fire2" $ \selfRef -> do
    soldierFire selfRef 1
    return True

soldierFire3 :: EntThink
soldierFire3 = EntThink "soldier_fire3" $ \selfRef -> do
    void (entThink soldierDuckDown selfRef)
    soldierFire selfRef 2
    return True

soldierFire4 :: EntThink
soldierFire4 = EntThink "soldier_fire4" $ \selfRef -> do
    soldierFire selfRef 3
    return True

soldierFire6 :: EntThink
soldierFire6 = EntThink "soldier_fire6" $ \selfRef -> do
    soldierFire selfRef 5
    return True

soldierFire7 :: EntThink
soldierFire7 = EntThink "soldier_fire7" $ \selfRef -> do
    soldierFire selfRef 6
    return True

soldierFire8 :: EntThink
soldierFire8 = EntThink "soldier_fire8" $ \selfRef -> do
    soldierFire selfRef 7
    return True

soldierFire :: Ref EdictT -> Int -> Quake ()
soldierFire selfRef flashNumber = do
    self <- readRef selfRef
    let flashIndex | (self^.eEntityState.esSkinNum) < 2 = blasterFlash UV.! flashNumber
                   | (self^.eEntityState.esSkinNum) < 4 = shotgunFlash UV.! flashNumber
                   | otherwise                          = machinegunFlash UV.! flashNumber
        (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashIndex) forward right
    aim <- getAim (self^.eEnemy) forward start
    fireWeapon self start aim flashIndex
  where
    getAim Nothing forward _
        | flashNumber == 5 || flashNumber == 6 = return forward
        | otherwise = do
            Com.fatalError "MSoldier.soldierFire#getAim self^.eEnemy is Nothing"
            return forward
    getAim (Just enemyRef) forward start
        | flashNumber == 5 || flashNumber == 6 = return forward
        | otherwise = do
            enemy <- readRef enemyRef
            let V3 a b c = enemy^.eEntityState.esOrigin
                end = V3 a b (c + fromIntegral (enemy^.eViewHeight))
                aim = end - start
                dir = Math3D.vectorAngles aim
                (forward', right', up') = Math3D.angleVectors dir True True True
            r <- fmap (* 1000) Lib.crandom
            u <- fmap (* 500) Lib.crandom
            let end' = start + fmap (* 8192) forward'
                             + fmap (* r) right'
                             + fmap (* u) up'
                aim' = end' - start
            return (normalize aim')
    fireWeapon self start aim flashIndex
        | (self^.eEntityState.esSkinNum) <= 1 =
            Monster.monsterFireBlaster selfRef start aim 5 600 flashIndex Constants.efBlaster
        | (self^.eEntityState.esSkinNum) <= 3 =
            Monster.monsterFireShotgun selfRef start aim 2 1 Constants.defaultShotgunHspread Constants.defaultShotgunVspread Constants.defaultShotgunCount flashIndex
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            when ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiHoldFrame == 0) $ do
                r <- Lib.rand
                modifyRef selfRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + (3 + fromIntegral (r `mod` 8)) * Constants.frameTime)
            Monster.monsterFireBullet selfRef start aim 2 4 Constants.defaultBulletHspread Constants.defaultBulletVspread flashIndex
            pauseTime <- fmap (^.eMonsterInfo.miPauseTime) (readRef selfRef)
            if levelTime >= pauseTime
                then modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiHoldFrame)))
                else modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

soldierDuckUp :: EntThink
soldierDuckUp = EntThink "soldier_duck_up" $ \selfRef -> do
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiDucked))
                               & eMaxs._z +~ 32
                               & eTakeDamage .~ Constants.damageAim)
    linkEntity selfRef
    return True

soldierDuckDown :: EntThink
soldierDuckDown = EntThink "soldier_duck_down" $ \selfRef -> do
    self <- readRef selfRef
    when ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked == 0) $ do
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiDucked)
                                   & eMaxs._z -~ 32
                                   & eTakeDamage .~ Constants.damageYes
                                   & eMonsterInfo.miPauseTime .~ levelTime + 1)
        linkEntity selfRef
    return True

soldierDuckHold :: EntThink
soldierDuckHold = EntThink "soldier_duck_hold" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (updateMI self levelTime))
    return True
  where
    updateMI self levelTime
        | levelTime >= self^.eMonsterInfo.miPauseTime = (.&. (complement Constants.aiHoldFrame))
        | otherwise = (.|. Constants.aiHoldFrame)

soldierFramesAttack1 :: V.Vector MFrameT
soldierFramesAttack1 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire1)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack1Refire1)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierCock)
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack1Refire2)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    ]

soldierMoveAttack1 :: MMoveT
soldierMoveAttack1 = MMoveT "soldierMoveAttack1" frameAttak101 frameAttak112 soldierFramesAttack1 (Just soldierRun)

soldierFramesAttack2 :: V.Vector MFrameT
soldierFramesAttack2 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire2)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack2Refire1)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierCock)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack2Refire2)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    ]

soldierMoveAttack2 :: MMoveT
soldierMoveAttack2 = MMoveT "soldierMoveAttack2" frameAttak201 frameAttak218 soldierFramesAttack2 (Just soldierRun)

soldierFramesAttack3 :: V.Vector MFrameT
soldierFramesAttack3 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire3)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack3Refire)
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierDuckUp)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    ]

soldierMoveAttack3 :: MMoveT
soldierMoveAttack3 = MMoveT "soldierMoveAttack3" frameAttak301 frameAttak309 soldierFramesAttack3 (Just soldierRun)

soldierFramesAttack4 :: V.Vector MFrameT
soldierFramesAttack4 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire4)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    ]

soldierMoveAttack4 :: MMoveT
soldierMoveAttack4 = MMoveT "soldierMoveAttack4" frameAttak401 frameAttak406 soldierFramesAttack4 (Just soldierRun)

soldierFramesAttack6 :: V.Vector MFrameT
soldierFramesAttack6 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 10 Nothing
    , MFrameT (Just GameAI.aiCharge)  4 Nothing
    , MFrameT (Just GameAI.aiCharge) 12 Nothing
    , MFrameT (Just GameAI.aiCharge) 11 (Just soldierFire8)
    , MFrameT (Just GameAI.aiCharge) 13 Nothing
    , MFrameT (Just GameAI.aiCharge) 18 Nothing
    , MFrameT (Just GameAI.aiCharge) 15 Nothing
    , MFrameT (Just GameAI.aiCharge) 14 Nothing
    , MFrameT (Just GameAI.aiCharge) 11 Nothing
    , MFrameT (Just GameAI.aiCharge)  8 Nothing
    , MFrameT (Just GameAI.aiCharge) 11 Nothing
    , MFrameT (Just GameAI.aiCharge) 12 Nothing
    , MFrameT (Just GameAI.aiCharge) 12 Nothing
    , MFrameT (Just GameAI.aiCharge) 17 (Just soldierAttack6Refire)
    ]

soldierMoveAttack6 :: MMoveT
soldierMoveAttack6 = MMoveT "soldierMoveAttack6" frameRuns01 frameRuns14 soldierFramesAttack6 (Just soldierRun)

soldierAttack1Refire1 :: EntThink
soldierAttack1Refire1 = EntThink "soldier_attack1_refire1" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doAttack1Refire1 selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MSoldier.soldierAttack1Refire1 self^.eEnemy is Nothing"
    doAttack1Refire1 selfRef self enemyRef = do
        enemy <- readRef enemyRef
        unless (self^.eEntityState.esSkinNum > 1 || enemy^.eHealth <= 0) $ do
            skill <- fmap (^.cvValue) skillCVar
            r <- Lib.randomF
            modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ (nextFrame self enemy skill r))
    nextFrame self enemy skill r
        | (skill == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee = frameAttak102
        | otherwise = frameAttak110

soldierAttack1Refire2 :: EntThink
soldierAttack1Refire2 = EntThink "soldier_attack1_refire2" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doAttack1Refire2 selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MSoldier.soldierAttack1Refire2 self^.eEnemy is Nothing"
    doAttack1Refire2 selfRef self enemyRef = do
        enemy <- readRef enemyRef
        unless (self^.eEntityState.esSkinNum < 2 || enemy^.eHealth <= 0) $ do
            skill <- fmap (^.cvValue) skillCVar
            r <- Lib.randomF
            when ((skill == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee) $
                modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameAttak102)

soldierAttack2Refire1 :: EntThink
soldierAttack2Refire1 = EntThink "soldier_attack2_refire1" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doAttack2Refire1 selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MSoldier.soldierAttack2Refire1 self^.eEnemy is Nothing"
    doAttack2Refire1 selfRef self enemyRef = do
        enemy <- readRef enemyRef
        unless (self^.eEntityState.esSkinNum > 1 || enemy^.eHealth <= 0) $ do
            skill <- fmap (^.cvValue) skillCVar
            r <- Lib.randomF
            modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ (nextFrame self enemy skill r))
    nextFrame self enemy skill r
        | (skill == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee = frameAttak204
        | otherwise = frameAttak216

soldierAttack2Refire2 :: EntThink
soldierAttack2Refire2 = EntThink "soldier_attack2_refire2" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doAttack2Refire2 selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MSoldier.soldierAttack2Refire2 self^.eEnemy is Nothing"
    doAttack2Refire2 selfRef self enemyRef = do
        enemy <- readRef enemyRef
        unless (self^.eEntityState.esSkinNum < 2 || enemy^.eHealth <= 0) $ do
            skill <- fmap (^.cvValue) skillCVar
            r <- Lib.randomF
            when ((skill == 3 && r < 0.5) || GameUtil.range self enemy == Constants.rangeMelee) $
                modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameAttak204)

soldierAttack3Refire :: EntThink
soldierAttack3Refire = EntThink "soldier_attack3_refire" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when ((levelTime + 0.4) < (self^.eMonsterInfo.miPauseTime)) $
        modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameAttak303)
    return True

soldierAttack6Refire :: EntThink
soldierAttack6Refire = EntThink "soldier_attack6_refire" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doAttack6Refire selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MSoldier.soldierAttack2Refire2 self^.eEnemy is Nothing"
    doAttack6Refire selfRef self enemyRef = do
        enemy <- readRef enemyRef
        unless (enemy^.eHealth <= 0 || GameUtil.range self enemy < Constants.rangeMid) $ do
            skill <- fmap (^.cvValue) skillCVar
            when (skill == 3) $
                modifyRef selfRef (\v -> v & eMonsterInfo.miNextFrame .~ frameRuns03)