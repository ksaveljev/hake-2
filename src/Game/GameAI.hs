module Game.GameAI
    ( aiCharge
    , aiMove
    , aiRun
    , aiSetSightClient
    , aiStand
    , aiWalk
    , flyMonsterStart
    , walkMonsterStart
    ) where

import           Control.Lens      (use, (^.), (.=), (&), (.~), (%~))
import           Control.Monad     (void, when, unless)
import           Data.Bits         (complement, (.&.), (.|.))
import qualified Data.ByteString   as B
import           Data.Maybe        (isJust)
import           Linear            (_y)

import qualified Client.M          as M
import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import           Game.GameLocalsT
import qualified Game.GameUtil     as GameUtil
import           Game.LevelLocalsT
import qualified Game.Monster      as Monster
import           Game.MonsterInfoT
import qualified QCommon.Com       as Com
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib          as Lib
import qualified Util.Math3D       as Math3D

aiCharge :: AI
aiCharge = AI "ai_charge" $ \selfRef dist -> do
    self <- readRef selfRef
    enemyRef <- getEnemyRef (self^.eEnemy)
    enemy <- readRef enemyRef
    modifyRef selfRef (\v -> v & eIdealYaw .~ Math3D.vectorYaw ((enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)))
    M.changeYaw selfRef
    when (dist /= 0) $ do
        updatedSelf <- readRef selfRef
        void (M.walkMove selfRef (updatedSelf^.eEntityState.esAngles._y) dist)
  where
    getEnemyRef Nothing = do
        Com.fatalError "GameAI.aiCharge self^.eEnemy is Nothing"
        return (Ref (-1))
    getEnemyRef (Just enemyRef) = return enemyRef

aiRun :: AI
aiRun = error "GameAI.aiRun" -- TODO

aiSetSightClient :: Quake ()
aiSetSightClient = do
    maxClients <- use (gameBaseGlobals.gbGame.glMaxClients)
    start <- calcStart <$> use (gameBaseGlobals.gbLevel.llSightClient)
    lookThroughClients maxClients start start
  where
    calcStart Nothing = 1
    calcStart (Just (Ref idx)) = idx

lookThroughClients :: Int -> Int -> Int -> Quake ()
lookThroughClients maxClients start check = do
    edict <- readRef (Ref check')
    findSightClient edict
  where
    check' | check + 1 > maxClients = 1
           | otherwise = check + 1
    findSightClient edict
        | (edict^.eInUse) && (edict^.eHealth) > 0 && (edict^.eFlags) .&. Constants.flNoTarget == 0 =
            gameBaseGlobals.gbLevel.llSightClient .= Just (Ref check')
        | check' == start =
            gameBaseGlobals.gbLevel.llSightClient .= Nothing
        | otherwise = lookThroughClients maxClients start check'

aiStand :: AI
aiStand = AI "ai_stand" $ \selfRef dist -> do
    when (dist /= 0) $ do
        self <- readRef selfRef
        void (M.walkMove selfRef (self^.eEntityState.esAngles._y) dist)
    checkAIStandGround selfRef
        >>= checkFindTarget selfRef
        >>= checkPauseTime selfRef
        >>= tryToIdle selfRef
  where
    checkAIStandGround selfRef = do
        self <- readRef selfRef
        doCheckAIStandGround selfRef self
    doCheckAIStandGround selfRef self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 = do
              maybe (void (GameUtil.findTarget selfRef)) (hasEnemy selfRef self) (self^.eEnemy)
              return True
        | otherwise = return False
    hasEnemy selfRef self enemyRef = do
        enemy <- readRef enemyRef
        let idealYaw = Math3D.vectorYaw ((enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin))
        modifyRef selfRef (\v -> v & eIdealYaw .~ idealYaw)
        when ((self^.eEntityState.esAngles._y) /= idealYaw && (self^.eMonsterInfo.miAIFlags) .&. Constants.aiTempStandGround /= 0) $ do
            modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement (Constants.aiStandGround .|. Constants.aiTempStandGround))))
            maybe runError (\runF -> void (entThink runF selfRef)) (self^.eMonsterInfo.miRun)
        M.changeYaw selfRef
        void (aiCheckAttack selfRef 0)
    checkFindTarget _ True = return True
    checkFindTarget selfRef _ = GameUtil.findTarget selfRef
    checkPauseTime _ True = return True
    checkPauseTime selfRef _ = do
        self <- readRef selfRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        doCheckPauseTime selfRef self levelTime
    doCheckPauseTime selfRef self levelTime
        | levelTime > (self^.eMonsterInfo.miPauseTime) = do
            maybe walkError (\walkF -> void (entThink walkF selfRef)) (self^.eMonsterInfo.miWalk)
            return True
        | otherwise = return False
    tryToIdle _ True = return ()
    tryToIdle selfRef _ = do
      self <- readRef selfRef
      levelTime <- use (gameBaseGlobals.gbLevel.llTime)
      doTryToIdle selfRef self levelTime (self^.eMonsterInfo.miIdle)
    doTryToIdle _ _ _ Nothing = return ()
    doTryToIdle selfRef self levelTime (Just idleF) =
        when ((self^.eSpawnFlags) .&. 1 == 0 && levelTime > (self^.eMonsterInfo.miIdleTime)) $ do
            rf <- Lib.randomF
            if (self^.eMonsterInfo.miIdleTime) /= 0
                then do
                    void (entThink idleF selfRef)
                    modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + 15 + rf * 15)
                else do
                    modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + rf * 15)
    runError = Com.fatalError "GameAI.aiStand#hasEnemy self^.eMonsterInfo.miRun is Nothing"
    walkError = Com.fatalError "GameAI.aiStand#doCheckPauseTime self^.eMonsterInfo.miWalk is Nothing"

aiWalk :: AI
aiWalk = AI "ai_walk" $ \selfRef dist -> do
    M.moveToGoal selfRef dist
    found <- GameUtil.findTarget selfRef
    unless found $ do
        self <- readRef selfRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        doWalk selfRef self levelTime (self^.eMonsterInfo.miSearch)
  where
    doWalk _ _ _ Nothing = return ()
    doWalk selfRef self levelTime (Just searchF)
        | levelTime > (self^.eMonsterInfo.miIdleTime) = do
          r <- Lib.randomF
          if (self^.eMonsterInfo.miIdleTime) /= 0
              then do
                  void (entThink searchF selfRef)
                  modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + 15 + r * 15)
              else
                  modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + r * 15)
        | otherwise = return ()

aiCheckAttack :: Ref EdictT -> Float -> Quake Bool
aiCheckAttack = error "GameAI.aiCheckAttack" -- TODO

walkMonsterStart :: EntThink
walkMonsterStart = EntThink "walkmonster_start" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eThink .~ Just walkMonsterStartGo)
    void (Monster.monsterStart edictRef)
    return True

walkMonsterStartGo :: EntThink
walkMonsterStartGo = EntThink "walkmonster_start_go" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    self <- readRef selfRef
    when ((self^.eSpawnFlags) .&. 2 == 0 && levelTime < 1) $ do
        void (entThink M.dropToFloor selfRef)
        updatedSelf <- readRef selfRef
        when (isJust (updatedSelf^.eGroundEntity)) $ do
          ok <- M.walkMove selfRef 0 0
          when (not ok) $ do
              finalSelf <- readRef selfRef
              dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
              dprintf (B.concat [finalSelf^.eClassName, " in solid at ", Lib.vtos (finalSelf^.eEntityState.esOrigin), "\n"])
    updatedSelf <- readRef selfRef
    when ((updatedSelf^.eYawSpeed) == 0) $
        modifyRef selfRef (\v -> v & eYawSpeed .~ 40)
    modifyRef selfRef (\v -> v & eViewHeight .~ 25)
    Monster.monsterStartGo selfRef
    finalSelf <- readRef selfRef
    when ((finalSelf^.eSpawnFlags) .&. 2 /= 0) $
        void (entThink Monster.monsterTriggeredStart selfRef)
    return True

aiMove :: AI
aiMove = error "GameAI.aiMove" -- TODO

flyMonsterStart :: EntThink
flyMonsterStart = EntThink "flymonster_start" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eFlags %~ (\a -> a .|. Constants.flFly)
                               & eThink .~ Just flyMonsterStartGo)
    void (Monster.monsterStart selfRef)
    return True

flyMonsterStartGo :: EntThink
flyMonsterStartGo = EntThink "flymonster_start_go" $ \selfRef -> do
    ok <- M.walkMove selfRef 0 0
    unless ok $ do
        self <- readRef selfRef
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat [self^.eClassName, " in solid at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
    modifyRef selfRef (\v -> v & eYawSpeed %~ (\a -> if a == 0 then 20 else a)
                               & eViewHeight .~ 25)
    Monster.monsterStartGo selfRef
    self <- readRef selfRef
    when ((self^.eSpawnFlags) .&. 2 /= 0) $
        void (entThink Monster.monsterTriggeredStart selfRef)
    return True
