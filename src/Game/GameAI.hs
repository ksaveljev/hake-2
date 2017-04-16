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
import           Control.Monad     (void)
import           Data.Bits         ((.&.))

import qualified Constants
import           Game.EdictT
import           Game.GameLocalsT
import           Game.LevelLocalsT
import qualified Game.Monster      as Monster
import           QuakeRef
import           QuakeState
import           Types

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
walkMonsterStartGo = error "GameAI.walkMonsterStartGo" -- TODO

aiMove :: AI
aiMove = error "GameAI.aiMove" -- TODO

flyMonsterStart :: EntThink
flyMonsterStart = EntThink "flymonster_start" $ \selfRef -> do
    undefined -- TODO
