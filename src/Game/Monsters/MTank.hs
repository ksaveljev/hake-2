module Game.Monsters.MTank
    ( spMonsterTank
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (+~), (%~))
import           Control.Monad         (void, when, unless)
import           Data.Bits             ((.&.), (.|.), complement)
import qualified Data.Vector           as V
import           Linear                (V3(..), norm, normalize, _x, _y, _z)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameMisc         as GameMisc
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
modelScale = 1.20000

frameStand01 :: Int
frameStand01 = 0

frameStand30 :: Int
frameStand30 = 29

frameWalk01 :: Int
frameWalk01 = 30

frameWalk04 :: Int
frameWalk04 = 33

frameWalk05 :: Int
frameWalk05 = 34

frameWalk20 :: Int
frameWalk20 = 49

frameWalk21 :: Int
frameWalk21 = 50

frameWalk25 :: Int
frameWalk25 = 54

frameAttack101 :: Int
frameAttack101 = 55

frameAttack110 :: Int
frameAttack110 = 64

frameAttack111 :: Int
frameAttack111 = 65

frameAttack113 :: Int
frameAttack113 = 67

frameAttack116 :: Int
frameAttack116 = 70

frameAttack117 :: Int
frameAttack117 = 71

frameAttack122 :: Int
frameAttack122 = 76

frameAttack201 :: Int
frameAttack201 = 77

frameAttack238 :: Int
frameAttack238 = 114

frameAttack301 :: Int
frameAttack301 = 115

frameAttack321 :: Int
frameAttack321 = 135

frameAttack322 :: Int
frameAttack322 = 136

frameAttack324 :: Int
frameAttack324 = 138

frameAttack327 :: Int
frameAttack327 = 141

frameAttack330 :: Int
frameAttack330 = 144

frameAttack331 :: Int
frameAttack331 = 145

frameAttack353 :: Int
frameAttack353 = 167

frameAttack401 :: Int
frameAttack401 = 168

frameAttack406 :: Int
frameAttack406 = 173

frameAttack411 :: Int
frameAttack411 = 178

frameAttack415 :: Int
frameAttack415 = 182

frameAttack419 :: Int
frameAttack419 = 186

frameAttack429 :: Int
frameAttack429 = 196

framePain101 :: Int
framePain101 = 197

framePain104 :: Int
framePain104 = 200

framePain201 :: Int
framePain201 = 201

framePain205 :: Int
framePain205 = 205

framePain301 :: Int
framePain301 = 206

framePain316 :: Int
framePain316 = 221

frameDeath101 :: Int
frameDeath101 = 222

frameDeath132 :: Int
frameDeath132 = 253

tankSight :: EntInteract
tankSight = EntInteract "tank_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mTankGlobals.mTankSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

tankFootStep :: EntThink
tankFootStep = EntThink "tank_footstep" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundStep <- use (mTankGlobals.mTankSoundStep)
    sound (Just selfRef) Constants.chanBody soundStep 1 Constants.attnNorm 0
    return True

tankThud :: EntThink
tankThud = EntThink "tank_thud" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundThud <- use (mTankGlobals.mTankSoundThud)
    sound (Just selfRef) Constants.chanBody soundThud 1 Constants.attnNorm 0
    return True

tankWindUp :: EntThink
tankWindUp = EntThink "tank_windup" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundWindUp <- use (mTankGlobals.mTankSoundWindUp)
    sound (Just selfRef) Constants.chanWeapon soundWindUp 1 Constants.attnNorm 0
    return True

tankIdle :: EntThink
tankIdle = EntThink "tank_idle" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle <- use (mTankGlobals.mTankSoundIdle)
    sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

tankFramesStand :: V.Vector MFrameT
tankFramesStand = V.replicate 30 (MFrameT (Just GameAI.aiStand) 0 Nothing)

tankMoveStand :: MMoveT
tankMoveStand = MMoveT "tankMoveStand" frameStand01 frameStand30 tankFramesStand Nothing

tankStand :: EntThink
tankStand = EntThink "tank_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just tankMoveStand)
    return True

tankRun :: EntThink
tankRun = EntThink "tank_run" $ \selfRef -> do
    enemy <- fmap (^.eEnemy) (readRef selfRef)
    maybe (noEnemy selfRef) (hasEnemy selfRef) enemy
    self <- readRef selfRef
    setCurrentMove selfRef self
    return True
  where
    noEnemy selfRef = modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiBrutal)))
    hasEnemy selfRef enemyRef = do
        enemy <- readRef enemyRef
        maybe (noClient selfRef) (hasClient selfRef) (enemy^.eClient)
    noClient selfRef = modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiBrutal)))
    hasClient selfRef _ = modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiBrutal))
    setCurrentMove selfRef self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 =
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just tankMoveStand)
        | otherwise =
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove (self^.eMonsterInfo.miCurrentMove)))
    currentMove Nothing = tankMoveStartRun
    currentMove (Just move)
        | (move^.mmId) == "tankMoveWalk" || (move^.mmId) == "tankMoveStartRun" = tankMoveRun
        | otherwise = tankMoveStartRun

tankWalk :: EntThink
tankWalk = EntThink "tank_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just tankMoveWalk)
    return True

tankFramesStartWalk :: V.Vector MFrameT
tankFramesStartWalk = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiWalk)  0 Nothing
    , MFrameT (Just GameAI.aiWalk)  6 Nothing
    , MFrameT (Just GameAI.aiWalk)  6 Nothing
    , MFrameT (Just GameAI.aiWalk) 11 (Just tankFootStep)
    ]

tankMoveStartWalk :: MMoveT
tankMoveStartWalk = MMoveT "tankMoveStartWalk" frameWalk01 frameWalk04 tankFramesStartWalk (Just tankWalk)

tankFramesWalk :: V.Vector MFrameT
tankFramesWalk = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiWalk) 4 Nothing
    , MFrameT (Just GameAI.aiWalk) 5 Nothing
    , MFrameT (Just GameAI.aiWalk) 3 Nothing
    , MFrameT (Just GameAI.aiWalk) 2 Nothing
    , MFrameT (Just GameAI.aiWalk) 5 Nothing
    , MFrameT (Just GameAI.aiWalk) 5 Nothing
    , MFrameT (Just GameAI.aiWalk) 4 Nothing
    , MFrameT (Just GameAI.aiWalk) 4 (Just tankFootStep)
    , MFrameT (Just GameAI.aiWalk) 3 Nothing
    , MFrameT (Just GameAI.aiWalk) 5 Nothing
    , MFrameT (Just GameAI.aiWalk) 4 Nothing
    , MFrameT (Just GameAI.aiWalk) 5 Nothing
    , MFrameT (Just GameAI.aiWalk) 7 Nothing
    , MFrameT (Just GameAI.aiWalk) 7 Nothing
    , MFrameT (Just GameAI.aiWalk) 6 Nothing
    , MFrameT (Just GameAI.aiWalk) 6 (Just tankFootStep)
    ]

tankMoveWalk :: MMoveT
tankMoveWalk = MMoveT "tankMoveWalk" frameWalk05 frameWalk20 tankFramesWalk Nothing

tankFramesStopWalk :: V.Vector MFrameT
tankFramesStopWalk = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiWalk) 3 Nothing
    , MFrameT (Just GameAI.aiWalk) 3 Nothing
    , MFrameT (Just GameAI.aiWalk) 2 Nothing
    , MFrameT (Just GameAI.aiWalk) 2 Nothing
    , MFrameT (Just GameAI.aiWalk) 4 (Just tankFootStep) 
    ]

tankMoveStopWalk :: MMoveT
tankMoveStopWalk = MMoveT "tankMoveStopWalk" frameWalk21 frameWalk25 tankFramesStopWalk (Just tankStand)

tankFramesStartRun :: V.Vector MFrameT
tankFramesStartRun = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiRun)  0 Nothing
    , MFrameT (Just GameAI.aiRun)  6 Nothing
    , MFrameT (Just GameAI.aiRun)  6 Nothing
    , MFrameT (Just GameAI.aiRun) 11 (Just tankFootStep)
    ]

tankMoveStartRun :: MMoveT
tankMoveStartRun = MMoveT "tankMoveStartRun" frameWalk01 frameWalk04 tankFramesStartRun (Just tankRun)

tankFramesRun :: V.Vector MFrameT
tankFramesRun = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiRun) 4 Nothing
    , MFrameT (Just GameAI.aiRun) 5 Nothing
    , MFrameT (Just GameAI.aiRun) 3 Nothing
    , MFrameT (Just GameAI.aiRun) 2 Nothing
    , MFrameT (Just GameAI.aiRun) 5 Nothing
    , MFrameT (Just GameAI.aiRun) 5 Nothing
    , MFrameT (Just GameAI.aiRun) 4 Nothing
    , MFrameT (Just GameAI.aiRun) 4 (Just tankFootStep)
    , MFrameT (Just GameAI.aiRun) 3 Nothing
    , MFrameT (Just GameAI.aiRun) 5 Nothing
    , MFrameT (Just GameAI.aiRun) 4 Nothing
    , MFrameT (Just GameAI.aiRun) 5 Nothing
    , MFrameT (Just GameAI.aiRun) 7 Nothing
    , MFrameT (Just GameAI.aiRun) 7 Nothing
    , MFrameT (Just GameAI.aiRun) 6 Nothing
    , MFrameT (Just GameAI.aiRun) 6 (Just tankFootStep)
    ]

tankMoveRun :: MMoveT
tankMoveRun = MMoveT "tankMoveRun" frameWalk05 frameWalk20 tankFramesRun Nothing

tankFramesStopRun :: V.Vector MFrameT
tankFramesStopRun = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiRun) 3 Nothing
    , MFrameT (Just GameAI.aiRun) 3 Nothing
    , MFrameT (Just GameAI.aiRun) 2 Nothing
    , MFrameT (Just GameAI.aiRun) 2 Nothing
    , MFrameT (Just GameAI.aiRun) 4 (Just tankFootStep)
    ]

tankMoveStopRun :: MMoveT
tankMoveStopRun = MMoveT "tankMoveStopRun" frameWalk21 frameWalk25 tankFramesStopRun (Just tankWalk)

tankFramesPain1 :: V.Vector MFrameT
tankFramesPain1 = V.replicate 4 (MFrameT (Just GameAI.aiMove) 0 Nothing)

tankMovePain1 :: MMoveT
tankMovePain1 = MMoveT "tankMovePain1" framePain101 framePain104 tankFramesPain1 (Just tankRun)

tankFramesPain2 :: V.Vector MFrameT
tankFramesPain2 = V.replicate 5 (MFrameT (Just GameAI.aiMove) 0 Nothing)

tankMovePain2 :: MMoveT
tankMovePain2 = MMoveT "tankMovePain2" framePain201 framePain205 tankFramesPain2 (Just tankRun)

tankFramesPain3 :: V.Vector MFrameT
tankFramesPain3 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove) (-7) Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   2  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   3  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   2  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   0  (Just tankFootStep) 
    ]

tankMovePain3 :: MMoveT
tankMovePain3 = MMoveT "tankMovePain3" framePain301 framePain316 tankFramesPain3 (Just tankRun)

tankPain :: EntPain
tankPain = EntPain "tank_pain" $ \selfRef _ _ damage -> do
    self <- readRef selfRef
    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
        modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    r <- Lib.randomF
    unless (done self levelTime damage r) $ do
        -- If hard or nightmare, don't go into pain while attacking
        skill <- fmap (^.cvValue) skillCVar
        unless (skip skill (self^.eEntityState.esFrame)) $ do
            modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)
            soundPain <- use (mTankGlobals.mTankSoundPain)
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) Constants.chanVoice soundPain 1 Constants.attnNorm 0
            unless (skill == 3) $ -- no pain anims in nightmare
                modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove damage))
  where
    done self levelTime damage r = damage <= 10 || levelTime < (self^.ePainDebounceTime) || damage <= 30 && r > 0.2
    skip skill frame = skill >= 2 && (frame >= frameAttack301 && frame <= frameAttack330 || frame >= frameAttack101 && frame <= frameAttack116)
    currentMove damage
        | damage <= 30 = tankMovePain1
        | damage <= 60 = tankMovePain2
        | otherwise    = tankMovePain3

tankBlaster :: EntThink
tankBlaster = EntThink "TankBlaster" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doTankBlaster selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MTank.tankBlaster self^.eEnemy is Nothing"
    doTankBlaster selfRef self enemyRef = do
        enemy <- readRef enemyRef
        let flashNumber | (self^.eEntityState.esFrame) == frameAttack110 = Constants.mz2TankBlaster1
                        | (self^.eEntityState.esFrame) == frameAttack113 = Constants.mz2TankBlaster2
                        | otherwise                                      = Constants.mz2TankBlaster3
            (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
            start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
            V3 a b c = enemy^.eEntityState.esOrigin
            end = V3 a b (c + fromIntegral (enemy^.eViewHeight))
            dir = end - start
        Monster.monsterFireBlaster selfRef start dir 30 800 flashNumber Constants.efBlaster

tankStrike :: EntThink
tankStrike = EntThink "TankStrike" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundStrike <- use (mTankGlobals.mTankSoundStrike)
    sound (Just selfRef) Constants.chanWeapon soundStrike 1 Constants.attnNorm 0
    return True

tankRocket :: EntThink
tankRocket = EntThink "TankRocket" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (doTankRocket selfRef self) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MTank.tankRocket self^.eEnemy is Nothing"
    doTankRocket selfRef self enemyRef = do
        enemy <- readRef enemyRef
        let flashNumber | (self^.eEntityState.esFrame) == frameAttack324 = Constants.mz2TankRocket1
                        | (self^.eEntityState.esFrame) == frameAttack327 = Constants.mz2TankRocket2
                        | otherwise                                      = Constants.mz2TankRocket3
            (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
            start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
            V3 a b c = enemy^.eEntityState.esOrigin
            vec = V3 a b (c + fromIntegral (enemy^.eViewHeight))
            dir = normalize (vec - start)
        Monster.monsterFireRocket selfRef start dir 50 550 flashNumber

tankMachineGun :: EntThink
tankMachineGun = EntThink "TankMachineGun" $ \selfRef -> do
    self <- readRef selfRef
    let flashNumber = Constants.mz2TankMachinegun1 + (self^.eEntityState.esFrame) - frameAttack406
        (forward, right, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True True False
        start = Math3D.projectSource (self^.eEntityState.esOrigin) (MFlash.monsterFlashOffset V.! flashNumber) forward right
    dir <- calcDir self start (self^.eEnemy)
    let (forward', _, _) = Math3D.angleVectors dir True False False
    Monster.monsterFireBullet selfRef start forward' 20 4 Constants.defaultBulletHspread Constants.defaultBulletVspread flashNumber
    return True
  where
    calcDir self _ Nothing = do
        return (V3 0 (dirY self) 0)
    calcDir self start (Just enemyRef) = do
        enemy <- readRef enemyRef
        return (V3 (dirX enemy start) (dirY self) 0)
    dirY self
        | (self^.eEntityState.esFrame) <= frameAttack415 =
            (self^.eEntityState.esAngles._y) - 8 * fromIntegral ((self^.eEntityState.esFrame) - frameAttack411)
        | otherwise =
            (self^.eEntityState.esAngles._y) + 8 * fromIntegral ((self^.eEntityState.esFrame) - frameAttack419)
    dirX enemy start = (Math3D.vectorAngles (((enemy^.eEntityState.esOrigin) & _z +~ fromIntegral (enemy^.eViewHeight)) - start))^._x

tankReAttackBlaster :: EntThink
tankReAttackBlaster = EntThink "tank_reattack_blaster" $ \selfRef -> do
    self <- readRef selfRef
    skill <- fmap (^.cvValue) skillCVar
    maybe enemyError (doTankReAttackBlaster selfRef skill) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MTank.tankReAttackBlaster self^.eEnemy is Nothing"
    doTankReAttackBlaster selfRef skill enemyRef = do
        enemy <- readRef enemyRef
        visible <- GameUtil.visible selfRef enemyRef
        r <- Lib.randomF
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove skill visible (enemy^.eHealth) r))
    currentMove skill visible enemyHealth r
        | skill >= 2 && visible && enemyHealth > 0 && r <= 0.6 = tankMoveReAttackBlast
        | otherwise                                            = tankMoveAttackPostBlast

tankFramesAttackBlast :: V.Vector MFrameT
tankFramesAttackBlast = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge) (-2) Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just tankBlaster) -- 10
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just tankBlaster)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just tankBlaster) -- 16
    ]

tankMoveAttackBlast :: MMoveT
tankMoveAttackBlast = MMoveT "tankMoveAttackBlast" frameAttack101 frameAttack116 tankFramesAttackBlast (Just tankReAttackBlaster)

tankFramesReAttackBlast :: V.Vector MFrameT
tankFramesReAttackBlast = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just tankBlaster)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 (Just tankBlaster) -- 16
    ]

tankMoveReAttackBlast :: MMoveT
tankMoveReAttackBlast = MMoveT "tankMoveReAttackBlast" frameAttack111 frameAttack116 tankFramesReAttackBlast (Just tankReAttackBlaster)

tankFramesAttackPostBlast :: V.Vector MFrameT
tankFramesAttackPostBlast = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove)   0  Nothing -- 17
    , MFrameT (Just GameAI.aiMove)   0  Nothing
    , MFrameT (Just GameAI.aiMove)   2  Nothing
    , MFrameT (Just GameAI.aiMove)   3  Nothing
    , MFrameT (Just GameAI.aiMove)   2  Nothing
    , MFrameT (Just GameAI.aiMove) (-2) (Just tankFootStep) -- 22
    ]

tankMoveAttackPostBlast :: MMoveT
tankMoveAttackPostBlast = MMoveT "tankMoveAttackPostBlast" frameAttack117 frameAttack122 tankFramesAttackPostBlast (Just tankRun)

tankPostStrike :: EntThink
tankPostStrike = EntThink "tank_poststrike" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eEnemy .~ Nothing)
    void (entThink tankRun selfRef)
    return True

tankDoAttackRocket :: EntThink
tankDoAttackRocket = EntThink "tank_doattack_rocket" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just tankMoveAttackFireRocket)
    return True

tankReFireRocket :: EntThink
tankReFireRocket = error "MTank.tankReFireRocket" -- TODO

tankFramesAttackStrike :: V.Vector MFrameT
tankFramesAttackStrike = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove)    3  Nothing
    , MFrameT (Just GameAI.aiMove)    2  Nothing
    , MFrameT (Just GameAI.aiMove)    2  Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    6  Nothing
    , MFrameT (Just GameAI.aiMove)    7  Nothing
    , MFrameT (Just GameAI.aiMove)    9  (Just tankFootStep)
    , MFrameT (Just GameAI.aiMove)    2  Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    2  Nothing
    , MFrameT (Just GameAI.aiMove)    2  (Just tankFootStep)
    , MFrameT (Just GameAI.aiMove)    2  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) Nothing
    , MFrameT (Just GameAI.aiMove)    0  (Just tankWindUp)
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  (Just tankStrike)
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)  (-1) Nothing
    , MFrameT (Just GameAI.aiMove)  (-1) Nothing
    , MFrameT (Just GameAI.aiMove)  (-1) Nothing
    , MFrameT (Just GameAI.aiMove)  (-1) Nothing
    , MFrameT (Just GameAI.aiMove)  (-1) Nothing
    , MFrameT (Just GameAI.aiMove)  (-3) Nothing
    , MFrameT (Just GameAI.aiMove) (-10) Nothing
    , MFrameT (Just GameAI.aiMove) (-10) Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) Nothing
    , MFrameT (Just GameAI.aiMove)  (-3) Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) (Just tankFootStep)
    ]

tankMoveAttackStrike :: MMoveT
tankMoveAttackStrike = MMoveT "tankMoveAttackStrike" frameAttack201 frameAttack238 tankFramesAttackStrike (Just tankPostStrike)

tankFramesAttackPreRocket :: V.Vector MFrameT
tankFramesAttackPreRocket = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 10
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)   2  Nothing
    , MFrameT (Just GameAI.aiCharge)   7  Nothing
    , MFrameT (Just GameAI.aiCharge)   7  Nothing
    , MFrameT (Just GameAI.aiCharge)   7  (Just tankFootStep)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 20)
    , MFrameT (Just GameAI.aiCharge) (-3) Nothing
    ]

tankMoveAttackPreRocket :: MMoveT
tankMoveAttackPreRocket = MMoveT "tankMoveAttackPreRocket" frameAttack301 frameAttack321 tankFramesAttackPreRocket (Just tankDoAttackRocket)

tankFramesAttackFireRocket :: V.Vector MFrameT
tankFramesAttackFireRocket = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) (-3) Nothing -- Loop Start 22 
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just tankRocket) -- 24
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just tankRocket)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) (Just tankRocket) -- 30 Loop End
    ]

tankMoveAttackFireRocket :: MMoveT
tankMoveAttackFireRocket = MMoveT "tankMoveAttackFireRocket" frameAttack322 frameAttack330 tankFramesAttackFireRocket (Just tankReFireRocket)

tankFramesAttackPostRocket :: V.Vector MFrameT
tankFramesAttackPostRocket = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge)   0  Nothing -- 31
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   2  Nothing
    , MFrameT (Just GameAI.aiCharge)   3  Nothing
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   2  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 40
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge) (-9) Nothing
    , MFrameT (Just GameAI.aiCharge) (-8) Nothing
    , MFrameT (Just GameAI.aiCharge) (-7) Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) (Just tankFootStep)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing -- 50
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    ]

tankMoveAttackPostRocket :: MMoveT
tankMoveAttackPostRocket = MMoveT "tankMoveAttackPostRocket" frameAttack331 frameAttack353 tankFramesAttackPostRocket (Just tankRun)

tankFramesAttackChain :: V.Vector MFrameT
tankFramesAttackChain = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT                Nothing 0 (Just tankMachineGun)
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    , MFrameT (Just GameAI.aiCharge) 0 Nothing
    ]

tankMoveAttackChain :: MMoveT
tankMoveAttackChain = MMoveT "tankMoveAttackChain" frameAttack401 frameAttack429 tankFramesAttackChain (Just tankRun)

tankAttack :: EntThink
tankAttack = EntThink "tank_attack" $ \selfRef -> do
    self <- readRef selfRef
    maybe enemyError (\enemyRef -> doTankAttack selfRef self =<< readRef enemyRef) (self^.eEnemy)
    return True
  where
    enemyError = Com.fatalError "MTank.tankAttack self^.eEnemy is Nothing"
    doTankAttack selfRef self enemy
        | (enemy^.eHealth) < 0 =
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just tankMoveAttackStrike
                                       & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiBrutal)))
        | otherwise = do
            let vec = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
                range = norm vec
            r <- Lib.randomF
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (currentMove range r))
            when (range > 250 && r >= 0.33 && r < 0.66) $ do
                levelTime <- use (gameBaseGlobals.gbLevel.llTime)
                modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 5.0) -- no pain for a while
    currentMove range r
        | range <= 125 && r < 0.4 = tankMoveAttackChain
        | range <= 125            = tankMoveAttackBlast
        | range <= 250 && r < 0.5 = tankMoveAttackChain
        | range <= 250            = tankMoveAttackBlast
        | r < 0.33                = tankMoveAttackChain
        | r < 0.66                = tankMoveAttackPreRocket
        | otherwise               = tankMoveAttackBlast

tankDead :: EntThink
tankDead = EntThink "tank_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-16)
                               & eMaxs .~ V3 16 16 0
                               & eMoveType .~ Constants.moveTypeToss
                               & eSvFlags %~ (.|. Constants.svfDeadMonster)
                               & eNextThink .~ 0)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

tankFramesDeath1 :: V.Vector MFrameT
tankFramesDeath1 = V.fromList -- IMPROVE
    [ MFrameT (Just GameAI.aiMove)  (-7) Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    3  Nothing
    , MFrameT (Just GameAI.aiMove)    6  Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    1  Nothing
    , MFrameT (Just GameAI.aiMove)    2  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)  (-2) Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)  (-3) Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)  (-4) Nothing
    , MFrameT (Just GameAI.aiMove)  (-6) Nothing
    , MFrameT (Just GameAI.aiMove)  (-4) Nothing
    , MFrameT (Just GameAI.aiMove)  (-5) Nothing
    , MFrameT (Just GameAI.aiMove)  (-7) Nothing
    , MFrameT (Just GameAI.aiMove) (-15) (Just tankThud)
    , MFrameT (Just GameAI.aiMove)  (-5) Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    , MFrameT (Just GameAI.aiMove)    0  Nothing
    ]

tankMoveDeath :: MMoveT
tankMoveDeath = MMoveT "tankMoveDeath" frameDeath101 frameDeath132 tankFramesDeath1 (Just tankDead)

tankDie :: EntDie
tankDie = EntDie "tank_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    doTankDie selfRef self damage
  where
    doTankDie selfRef self damage
        | (self^.eHealth) < (self^.eGibHealth) = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            udeath <- (gameImport^.giSoundIndex) (Just "misc/udeath.wav")
            (gameImport^.giSound) (Just selfRef) Constants.chanVoice udeath 1 Constants.attnNorm 0
            -- IMPROVE
            GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
            GameMisc.throwGib selfRef "models/objects/gibs/sm_metal/tris.md2" damage Constants.gibMetallic
            GameMisc.throwGib selfRef "models/objects/gibs/chest/tris.md2" damage Constants.gibOrganic
            GameMisc.throwGib selfRef "models/objects/gibs/gear/tris.md2" damage Constants.gibMetallic
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)
        | (self^.eDeadFlag) == Constants.deadDead = return ()
        | otherwise = do -- regular death
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            soundDie <- use (mTankGlobals.mTankSoundDie)
            sound (Just selfRef) Constants.chanVoice soundDie 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                       & eTakeDamage .~ Constants.damageYes
                                       & eMonsterInfo.miCurrentMove .~ Just tankMoveDeath)

spMonsterTank :: EntThink
spMonsterTank = EntThink "SP_monster_tank" $ \selfRef -> do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnMonsterTank selfRef deathmatch
    return True
  where
    doSpawnMonsterTank selfRef deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            self <- readRef selfRef
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let soundIndex = gameImport^.giSoundIndex
                modelIndex = gameImport^.giModelIndex
                linkEntity = gameImport^.giLinkEntity
            modelIdx <- modelIndex (Just "models/monsters/tank/tris.md2")
            soundIndex(Just ("tank/tnkpain2.wav")) >>= (mTankGlobals.mTankSoundPain   .=)
            soundIndex(Just ("tank/tnkdeth2.wav")) >>= (mTankGlobals.mTankSoundThud   .=)
            soundIndex(Just ("tank/tnkidle1.wav")) >>= (mTankGlobals.mTankSoundIdle   .=)
            soundIndex(Just ("tank/death.wav"))    >>= (mTankGlobals.mTankSoundDie    .=)
            soundIndex(Just ("tank/step.wav"))     >>= (mTankGlobals.mTankSoundStep   .=)
            soundIndex(Just ("tank/tnkatck4.wav")) >>= (mTankGlobals.mTankSoundWindUp .=)
            soundIndex(Just ("tank/tnkatck5.wav")) >>= (mTankGlobals.mTankSoundStrike .=)
            soundIndex(Just ("tank/sight1.wav"))   >>= (mTankGlobals.mTankSoundSight  .=)
            void (soundIndex(Just ("tank/tnkatck1.wav")))
            void (soundIndex(Just ("tank/tnkatk2a.wav")))
            void (soundIndex(Just ("tank/tnkatk2b.wav")))
            void (soundIndex(Just ("tank/tnkatk2c.wav")))
            void (soundIndex(Just ("tank/tnkatk2d.wav")))
            void (soundIndex(Just ("tank/tnkatk2e.wav")))
            void (soundIndex(Just ("tank/tnkatck3.wav")))
            modifyRef selfRef (\v -> v & eEntityState.esModelIndex .~ modelIdx
                                       & eMins .~ V3 (-32) (-32) (-16)
                                       & eMaxs .~ V3 32 32 72
                                       & eMoveType .~ Constants.moveTypeStep
                                       & eSolid .~ Constants.solidBbox
                                       & eMass .~ 500
                                       & ePain .~ Just tankPain
                                       & eDie .~ Just tankDie
                                       & eMonsterInfo.miStand .~ Just tankStand
                                       & eMonsterInfo.miWalk .~ Just tankWalk
                                       & eMonsterInfo.miRun .~ Just tankRun
                                       & eMonsterInfo.miDodge .~ Nothing
                                       & eMonsterInfo.miAttack .~ Just tankAttack
                                       & eMonsterInfo.miMelee .~ Nothing
                                       & eMonsterInfo.miSight .~ Just tankSight
                                       & eMonsterInfo.miIdle .~ Just tankIdle)
            if (self^.eClassName) == "monster_tank_commander"
                then modifyRef selfRef (\v -> v & eHealth .~ 1000
                                                & eGibHealth .~ (-225))
                else modifyRef selfRef (\v -> v & eHealth .~ 750
                                                & eGibHealth .~ (-200))
            linkEntity selfRef
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just tankMoveStand
                                       & eMonsterInfo.miScale .~ modelScale)
            void (entThink GameAI.walkMonsterStart selfRef)
            when ((self^.eClassName) == "monster_tank_commander") $
                modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 2)
