module Game.GameAI
    ( aiCharge
    , aiMove
    , aiRun
    , aiSetSightClient
    , aiStand
    , aiWalk
    , walkMonsterStart
    ) where

import           Control.Lens      (use, (^.), (.=))
import           Data.Bits         ((.&.))

import qualified Constants
import           Game.EdictT
import           Game.GameLocalsT
import           Game.LevelLocalsT
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
walkMonsterStart = error "GameAI.walkMonsterStart" -- TODO

aiMove :: AI
aiMove = error "GameAI.aiMove" -- TODO