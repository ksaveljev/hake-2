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

import           Control.Lens      (use, (^.), (.=), (&), (.~))
import           Control.Monad     (void, when)
import           Data.Bits         ((.&.))
import qualified Data.ByteString   as B
import           Data.Maybe        (isJust)

import qualified Client.M          as M
import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import           Game.GameLocalsT
import           Game.LevelLocalsT
import qualified Game.Monster      as Monster
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib          as Lib

aiCharge :: AI
aiCharge = error "GameAI.aiCharge" -- TODO

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
aiStand = error "GameAI.aiStand" -- TODO

aiWalk :: AI
aiWalk = error "GameAI.aiWalk" -- TODO

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
flyMonsterStart = error "GameAI.flyMonsterStart" -- TODO
