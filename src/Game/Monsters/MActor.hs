module Game.Monsters.MActor
    ( spMiscActor
    , spTargetActor
    ) where

import           Control.Lens          (use, (^.), (%=), (&), (.~), (%~))
import           Control.Monad         (void)
import           Data.Bits             (complement, (.&.), (.|.))
import qualified Data.ByteString       as B
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V
import           Linear                (V3(..), _y, _z)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameBase         as GameBase
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import           Game.MMoveT
import           Game.MonsterInfoT
import           Game.SpawnTempT
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib

modelScale :: Float
modelScale = 1.0

maxActorNames :: Int
maxActorNames = 8

frameAttack01 :: Int
frameAttack01 = 0

frameAttack04 :: Int
frameAttack04 = 3

frameDeath101 :: Int
frameDeath101 = 4

frameDeath107 :: Int
frameDeath107 = 10

frameDeath201 :: Int
frameDeath201 = 11

frameDeath213 :: Int
frameDeath213 = 23

frameFlip01 :: Int
frameFlip01 = 39

frameFlip14 :: Int
frameFlip14 = 52

framePain101 :: Int
framePain101 = 74

framePain103 :: Int
framePain103 = 76

framePain201 :: Int
framePain201 = 77

framePain203 :: Int
framePain203 = 79

framePain301 :: Int
framePain301 = 80

framePain303 :: Int
framePain303 = 82

frameRun02 :: Int
frameRun02 = 93

frameRun07 :: Int
frameRun07 = 98

frameStand101 :: Int
frameStand101 = 128

frameStand140 :: Int
frameStand140 = 167

frameTaunt01 :: Int
frameTaunt01 = 234

frameTaunt17 :: Int
frameTaunt17 = 250

frameWalk01 :: Int
frameWalk01 = 251

frameWalk08 :: Int
frameWalk08 = 258

actorNames :: V.Vector B.ByteString
actorNames = V.fromList
    [ "Hellrot", "Tokay", "Killme", "Disruptor"
    , "Adrianator", "Rambear", "Titus", "Bitterman"
    ]

spMiscActor :: Ref' EdictT -> Quake ()
spMiscActor selfRef = do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMiscActor self (self^.eTarget) (self^.eTargetName) gameImport =<< deathmatchCVar
  where
    proceedSpawnMiscActor self Nothing _ gameImport _ = do
        (gameImport^.giDprintf) (B.concat [self^.eClassName, " with no target at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict selfRef
    proceedSpawnMiscActor self _ Nothing gameImport _ = do
        (gameImport^.giDprintf) (B.concat ["untargeted ", self^.eClassName, " at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict selfRef
    proceedSpawnMiscActor _ _ _ gameImport deathmatch
        | (deathmatch^.cvValue) /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            modelIdx <- (gameImport^.giModelIndex) (Just "players/male/tris.md2")
            modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                       & eSolid .~ Constants.solidBbox
                                       & eEntityState.esModelIndex .~ modelIdx
                                       & eMins .~ V3 (-16) (-16) (-24)
                                       & eMaxs .~ V3 16 16 32
                                       & eHealth %~ (\h -> if h == 0 then 100 else h)
                                       & eMass .~ 200
                                       & ePain .~ Just actorPain
                                       & eDie .~ Just actorDie
                                       & eMonsterInfo.miStand .~ Just actorStand
                                       & eMonsterInfo.miWalk .~ Just actorWalk
                                       & eMonsterInfo.miRun .~ Just actorRun
                                       & eMonsterInfo.miAttack .~ Just actorAttack
                                       & eMonsterInfo.miMelee .~ Nothing
                                       & eMonsterInfo.miSight .~ Nothing
                                       & eMonsterInfo.miAIFlags %~ (.|. Constants.aiGoodGuy))
            (gameImport^.giLinkEntity) selfRef
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveStand
                                       & eMonsterInfo.miScale .~ modelScale)
            void (entThink GameAI.walkMonsterStart selfRef)
            modifyRef selfRef (\v -> v & eUse .~ Just actorUse)

spTargetActor :: Ref' EdictT -> Quake ()
spTargetActor selfRef = do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    checkTargetName self (self^.eTargetName) gameImport
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidTrigger
                               & eTouch .~ Just targetActorTouch
                               & eMins .~ V3 (-8) (-8) (-8)
                               & eMaxs .~ V3 8 8 8
                               & eSvFlags .~ Constants.svfNoClient)
    checkTargetSpawnFlags selfRef self
    (gameImport^.giLinkEntity) selfRef
  where
    checkTargetName self Nothing gameImport =
        (gameImport^.giDprintf) (B.concat [self^.eClassName, " with no targetname at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
    checkTargetName _ _ _ = return ()

checkTargetSpawnFlags :: Ref' EdictT -> EdictT -> Quake ()
checkTargetSpawnFlags selfRef self
    | (self^.eSpawnFlags) .&. 1 /= 0 = do
        gameBaseGlobals.gbSpawnTemp.stHeight %= (\v -> if v == 0 then 200 else v)
        modifyRef selfRef (\v -> v & eSpeed .~ speed
                                   & eEntityState.esAngles._y .~ angle)
        GameBase.setMoveDir selfRef =<< readRef selfRef
        height <- use (gameBaseGlobals.gbSpawnTemp.stHeight)
        modifyRef selfRef (\v -> v & eMoveDir._z .~ fromIntegral height)
    | otherwise = return ()
  where
    speed | (self^.eSpeed) == 0 = 200
          | otherwise = self^.eSpeed
    angle | (self^.eEntityState.esAngles._y) == 0 = 360
          | otherwise = self^.eEntityState.esAngles._y

targetActorTouch :: EntTouch
targetActorTouch = error "MActor.targetActorTouch" -- TODO

actorPain :: EntPain
actorPain = error "MActor.actorPain" -- TODO

actorDie :: EntDie
actorDie = error "MActor.actorDie" -- TODO

actorStand :: EntThink
actorStand = EntThink "MActor.actorStand" doActorStand

doActorStand :: Ref' EdictT -> Quake Bool
doActorStand selfRef = do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveStand)
    randomizeOnStartup =<< use (gameBaseGlobals.gbLevel.llTime)
    return True
  where
    randomizeOnStartup levelTime
        | levelTime < 1.0 = do
            r <- Lib.rand
            modifyRef selfRef (\v -> v & eEntityState.esFrame .~ (actorMoveStand^.mmFirstFrame) + (fromIntegral r `mod` ((actorMoveStand^.mmLastFrame) - (actorMoveStand^.mmFirstFrame) + 1)))
        | otherwise = return ()

actorWalk :: EntThink
actorWalk = EntThink "actor_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveWalk)
    return True

actorMoveWalk :: MMoveT
actorMoveWalk = MMoveT "actorMoveWalk" frameWalk01 frameWalk08 actorFramesWalk Nothing

actorFramesWalk :: V.Vector MFrameT
actorFramesWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dists)
  where
    dists = [0, 6, 10, 3, 2, 7, 10, 1, 4, 0, 0]

actorUse :: EntUse
actorUse = error "MActor.actorUse" -- TODO

actorMoveStand :: MMoveT
actorMoveStand = MMoveT "actorMoveStand" frameStand101 frameStand140 actorFramesStand Nothing

actorFramesStand :: V.Vector MFrameT
actorFramesStand = V.replicate 40 (MFrameT (Just GameAI.aiStand) 0 Nothing)

actorRun :: EntThink
actorRun = EntThink "MActor.actorRun" doActorRun

doActorRun :: Ref' EdictT -> Quake Bool
doActorRun selfRef = do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    runBasedOnTarget self levelTime
    return True
  where
    runBasedOnTarget self levelTime
        | levelTime < (self^.ePainDebounceTime) && isNothing (self^.eEnemy) =
            maybe justStand (const justWalk) (self^.eMoveTarget)
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 =
            justStand
        | otherwise =
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveRun)
    justStand = void (entThink actorStand selfRef)
    justWalk = void (entThink actorWalk selfRef)

actorAttack :: EntThink
actorAttack = EntThink "MActor.actorAttack" doActorAttack

doActorAttack :: Ref' EdictT -> Quake Bool
doActorAttack selfRef = do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    r <- Lib.rand
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveAttack
                               & eMonsterInfo.miPauseTime .~ levelTime + fromIntegral ((r .&. 15) + 3 + 7) * Constants.frameTime)
    return True

actorMoveRun :: MMoveT
actorMoveRun = MMoveT "actorMoveRun" frameRun02 frameRun07 actorFramesRun Nothing

actorFramesRun :: V.Vector MFrameT
actorFramesRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dists)
  where
    dists = [4, 15, 15, 8, 20, 15, 8, 17, 12, -2, -2, -1]

actorMoveAttack :: MMoveT
actorMoveAttack = MMoveT "actorMoveAttack" frameAttack01 frameAttack04 actorFramesAttack (Just actorRun)

actorFramesAttack :: V.Vector MFrameT
actorFramesAttack = V.fromList
    [ MFrameT (Just GameAI.aiCharge) (-2) (Just actorFire)
    , MFrameT (Just GameAI.aiCharge) (-2) Nothing
    , MFrameT (Just GameAI.aiCharge)   3  Nothing
    , MFrameT (Just GameAI.aiCharge)   2  Nothing
    ]

actorFire :: EntThink
actorFire = EntThink "MActor.actorFire" doActorFire

doActorFire :: Ref' EdictT -> Quake Bool
doActorFire selfRef = do
    actorMachineGun selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    self <- readRef selfRef
    checkPause self levelTime
    return True
  where
    checkPause self levelTime
        | levelTime >= (self^.eMonsterInfo.miPauseTime) =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. complement Constants.aiHoldFrame))
        | otherwise =
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

actorMachineGun :: Ref' EdictT -> Quake ()
actorMachineGun = error "MActor.actorMachineGun" -- TODO