module Game.Monsters.MBoss2
    ( spMonsterBoss2
    ) where

import           Control.Lens             (use, (^.), (.=), (.~), (&), (%~))
import           Control.Monad            (void)
import           Data.Bits                ((.&.), (.|.))
import qualified Data.Vector              as V
import           Linear                   (V3(..), norm)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI              as GameAI
import qualified Game.GameUtil            as GameUtil
import           Game.MonsterInfoT
import qualified Game.Monsters.MSuperTank as MSuperTank
import qualified QCommon.Com              as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib                 as Lib

modelScale :: Float
modelScale = 1.0

frameStand30 :: Int
frameStand30 = 0

frameStand50 :: Int
frameStand50 = 20

frameStand1 :: Int
frameStand1 = 21

frameWalk1 :: Int
frameWalk1 = 50

frameWalk20 :: Int
frameWalk20 = 69

frameAttack1 :: Int
frameAttack1 = 70

frameAttack9 :: Int
frameAttack9 = 78

frameAttack10 :: Int
frameAttack10 = 79

frameAttack15 :: Int
frameAttack15 = 84

frameAttack16 :: Int
frameAttack16 = 85

frameAttack19 :: Int
frameAttack19 = 88

frameAttack20 :: Int
frameAttack20 = 89

frameAttack40 :: Int
frameAttack40 = 109

framePain2 :: Int
framePain2 = 110

framePain19 :: Int
framePain19 = 127

framePain20 :: Int
framePain20 = 128

framePain23 :: Int
framePain23 = 131

frameDeath2 :: Int
frameDeath2 = 132

frameDeath50 :: Int
frameDeath50 = 180

spMonsterBoss2 :: Ref EdictT -> Quake ()
spMonsterBoss2 selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterBoss2 selfRef gameImport deathmatch

proceedSpawnMonsterBoss2 :: Ref EdictT -> GameImportT -> Float -> Quake ()
proceedSpawnMonsterBoss2 selfRef gameImport deathmatch
    | deathmatch /= 0 = GameUtil.freeEdict selfRef
    | otherwise = do
        soundIndex (Just "bosshovr/bhvpain1.wav") >>= (mBoss2Globals.mb2SoundPain1   .=)
        soundIndex (Just "bosshovr/bhvpain2.wav") >>= (mBoss2Globals.mb2SoundPain2   .=)
        soundIndex (Just "bosshovr/bhvpain3.wav") >>= (mBoss2Globals.mb2SoundPain3   .=)
        soundIndex (Just "bosshovr/bhvdeth1.wav") >>= (mBoss2Globals.mb2SoundDeath   .=)
        soundIndex (Just "bosshovr/bhvunqv1.wav") >>= (mBoss2Globals.mb2SoundSearch1 .=)
        soundIdx <- soundIndex (Just "bosshovr/bhvengn1.wav")
        modelIdx <- modelIndex (Just "models/monsters/boss2/tris.md2")
        modifyRef selfRef (\v -> v & eEntityState.esSound .~ soundIdx
                                   & eMoveType .~ Constants.moveTypeStep
                                   & eSolid .~ Constants.solidBbox
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eMins .~ V3 (-56) (-56) 0
                                   & eMaxs .~ V3 56 56 80
                                   & eHealth .~ 2000
                                   & eGibHealth .~ (-200)
                                   & eMass .~ 1000
                                   & eFlags %~ (.|. Constants.flImmuneLaser)
                                   & ePain .~ Just boss2Pain
                                   & eDie .~ Just boss2Die
                                   & eMonsterInfo.miStand .~ Just boss2Stand
                                   & eMonsterInfo.miWalk .~ Just boss2Walk
                                   & eMonsterInfo.miRun .~ Just boss2Run
                                   & eMonsterInfo.miAttack .~ Just boss2Attack
                                   & eMonsterInfo.miSearch .~ Just boss2Search
                                   & eMonsterInfo.miCheckAttack .~ Just boss2CheckAttack)
        linkEntity selfRef
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveStand
                                   & eMonsterInfo.miScale .~ modelScale)
        void (entThink GameAI.flyMonsterStart selfRef)
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity

boss2Stand :: EntThink
boss2Stand = EntThink "boss2_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveStand)
    return True

boss2Walk :: EntThink
boss2Walk = EntThink "boss2_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveWalk)
    return True

boss2Run :: EntThink
boss2Run = EntThink "boss2_run" $ \selfRef -> do
    action <- pickAction <$> readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    pickAction self
        | shouldStandGround self = boss2MoveStand
        | otherwise = boss2MoveRun
    shouldStandGround self =
        (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0

boss2Attack :: EntThink
boss2Attack = EntThink "boss2_attack" $ \selfRef -> do
    self <- readRef selfRef
    enemy <- getEnemy (self^.eEnemy)
    action <- pickAction self enemy <$> Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    getEnemy Nothing = do
        Com.fatalError "MBoss2.boss2Attack self^.eEnemy is Nothing"
        return (newEdictT (-1))
    getEnemy (Just enemyRef) = readRef enemyRef
    pickAction self enemy r
        | norm ((enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)) <= 125 = boss2MoveAttackPreMg
        | r <= 0.6 = boss2MoveAttackPreMg
        | otherwise = boss2MoveAttackRocket

boss2Search :: EntThink
boss2Search = EntThink "boss2_search" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSearch1 <- use (mBoss2Globals.mb2SoundSearch1)
    sound (Just selfRef) Constants.chanVoice soundSearch1 1 Constants.attnNone 0
    return True

boss2CheckAttack :: EntThink
boss2CheckAttack = EntThink "boss2_checkattack" $ \selfRef -> do
    undefined -- TODO

boss2MoveStand :: MMoveT
boss2MoveStand = MMoveT "boss2MoveStand" frameStand30 frameStand50 boss2FramesStand Nothing

boss2FramesStand :: V.Vector MFrameT
boss2FramesStand = V.replicate 21 (MFrameT (Just GameAI.aiStand) 0 Nothing)

boss2MoveWalk :: MMoveT
boss2MoveWalk = MMoveT "boss2MoveWalk" frameWalk1 frameWalk20 boss2FramesWalk Nothing

boss2FramesWalk :: V.Vector MFrameT
boss2FramesWalk = V.replicate 20 (MFrameT (Just GameAI.aiWalk) 8 Nothing)

boss2Pain :: EntPain
boss2Pain = undefined -- TODO

boss2Die :: EntDie
boss2Die = EntDie "boss2_die" $ \selfRef _ _ _ _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundDeath <- use (mBoss2Globals.mb2SoundDeath)
    sound (Just selfRef) Constants.chanVoice soundDeath 1 Constants.attnNone 0
    modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                               & eTakeDamage .~ Constants.damageNo
                               & eCount .~ 0
                               & eMonsterInfo.miCurrentMove .~ Just boss2MoveDeath)

boss2MoveRun :: MMoveT
boss2MoveRun = MMoveT "boss2MoveRun" frameWalk1 frameWalk20 boss2FramesRun Nothing

boss2FramesRun :: V.Vector MFrameT
boss2FramesRun = V.replicate 20 (MFrameT (Just GameAI.aiRun) 8 Nothing)

boss2MoveDeath :: MMoveT
boss2MoveDeath = MMoveT "boss2MoveDeath" frameDeath2 frameDeath50 boss2FramesDeath (Just boss2Dead)

boss2FramesDeath :: V.Vector MFrameT
boss2FramesDeath =
    V.snoc (V.replicate 48 (MFrameT (Just GameAI.aiMove) 0 Nothing))
           (MFrameT (Just GameAI.aiMove) 0 (Just MSuperTank.bossExplode))

boss2MoveAttackPreMg :: MMoveT
boss2MoveAttackPreMg = MMoveT "boss2MoveAttackPreMg" frameAttack1 frameAttack9 boss2FramesAttackPreMg Nothing

boss2FramesAttackPreMg :: V.Vector MFrameT
boss2FramesAttackPreMg =
    V.snoc (V.replicate 8 (MFrameT (Just GameAI.aiCharge) 1 Nothing))
           (MFrameT (Just GameAI.aiCharge) 1 (Just boss2AttackMg))

boss2MoveAttackRocket :: MMoveT
boss2MoveAttackRocket = MMoveT "boss2MoveAttackRocket" frameAttack20 frameAttack40 boss2FramesAttackRocket (Just boss2Run)

boss2FramesAttackRocket :: V.Vector MFrameT
boss2FramesAttackRocket =
    V.concat [a, b]
  where
    a = V.snoc (V.replicate 12 (MFrameT (Just GameAI.aiCharge) 1 Nothing))
               (MFrameT (Just GameAI.aiMove)   (-20) (Just boss2Rocket))
    b = V.replicate 8 (MFrameT (Just GameAI.aiCharge) 1 Nothing)

boss2Dead :: EntThink
boss2Dead = EntThink "boss2_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-56) (-56) 0
                               & eMaxs .~ V3 56 56 80
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

boss2AttackMg :: EntThink
boss2AttackMg = EntThink "boss2_attack_mg" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just boss2MoveAttackMg)
    return True

boss2Rocket :: EntThink
boss2Rocket = EntThink "Boss2Rocket" $ \selfRef -> do
    undefined -- TODO

boss2MoveAttackMg :: MMoveT
boss2MoveAttackMg = MMoveT "boss2MoveAttackMg" frameAttack10 frameAttack15 boss2FramesAttackMg Nothing

boss2FramesAttackMg :: V.Vector MFrameT
boss2FramesAttackMg = V.fromList
    [ MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
    , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
    , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
    , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
    , MFrameT (Just GameAI.aiCharge) 1 (Just boss2MachineGun)
    , MFrameT (Just GameAI.aiCharge) 1 (Just boss2ReAttackMg)
    ]

boss2ReAttackMg :: EntThink
boss2ReAttackMg = EntThink "boss2_reattack_mg" $ \selfRef -> do
    self <- readRef selfRef
    enemy <- getEnemy (self^.eEnemy)
    action <- pickAction self enemy <$> Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    getEnemy Nothing = do
        Com.fatalError "MBoss2.boss2ReAttackMg self^.eEnemy is Nothing"
        return (newEdictT (-1))
    getEnemy (Just enemyRef) = readRef enemyRef
    pickAction self enemy r
        | GameUtil.inFront self enemy && r <= 0.7 = boss2MoveAttackPostMg
        | otherwise = boss2MoveAttackPostMg

boss2MoveAttackPostMg :: MMoveT
boss2MoveAttackPostMg = MMoveT "boss2MoveAttackPostMg" frameAttack16 frameAttack19 boss2FramesAttackPostMg (Just boss2Run)

boss2FramesAttackPostMg :: V.Vector MFrameT
boss2FramesAttackPostMg = V.replicate 4 (MFrameT (Just GameAI.aiCharge) 1 Nothing)

boss2MachineGun :: EntThink
boss2MachineGun = EntThink "Boss2MachineGun" $ \selfRef -> do
    void (entThink boss2FireBulletLeft selfRef)
    void (entThink boss2FireBulletRight selfRef)
    return True

boss2FireBulletRight :: EntThink
boss2FireBulletRight = EntThink "boss2_firebullet_right" $ \selfRef -> do
    undefined -- TODO

boss2FireBulletLeft :: EntThink
boss2FireBulletLeft = EntThink "boss2_firebullet_left" $ \_ -> do
    undefined -- TODO