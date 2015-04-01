{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerClient where

import Control.Lens (Traversal', use, (^.), ix, preuse, (.=))
import Control.Monad (when, liftM)
import Data.Bits ((.|.), (.&.))
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameUtil as GameUtil

-- Called when a player drops from the server. Will not be called between levels. 
clientDisconnect :: Traversal' QuakeState (Maybe EdictReference) -> Quake ()
clientDisconnect = undefined -- TODO

{-
- Some information that should be persistant, like health, is still stored
- in the edict structure, so it needs to be mirrored out to the client
- structure before all the edicts are wiped. 
-}
saveClientData :: Quake ()
saveClientData = do
    maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
    edicts <- use $ gameBaseGlobals.gbGEdicts

    mapM_ (\idx -> updateClient (edicts V.! (idx + 1)) idx) [0..maxClients-1]

  where updateClient :: EdictT -> Int -> Quake ()
        updateClient edict idx =
          when (edict^.eInUse) $ do
            Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix idx
            let pers = client^.gcPers

            let updatedPers = pers { _cpHealth     = edict^.eEdictStatus.eHealth
                                   , _cpMaxHealth  = edict^.eEdictStatus.eMaxHealth
                                   , _cpSavedFlags = (edict^.eFlags) .&. (Constants.flGodMode .|. Constants.flNoTarget .|. Constants.flPowerArmor)
                                   }

            coopValue <- liftM (^.cvValue) coopCVar
            let finalPers = if coopValue /= 0
                              then updatedPers { _cpScore = client^.gcResp.crScore }
                              else updatedPers
            
            gameBaseGlobals.gbGame.glClients.ix idx.gcPers .= finalPers

initBodyQue :: Quake ()
initBodyQue = do
    gameBaseGlobals.gbLevel.llBodyQue .= 0
    mapM_ spawnBodyQue [0..Constants.bodyQueueSize-1]

  where spawnBodyQue :: Int -> Quake ()
        spawnBodyQue _ = do
          EdictReference idx <- GameUtil.spawn
          gameBaseGlobals.gbGEdicts.ix idx.eClassName .= "bodyque"

spInfoPlayerStart :: EdictReference -> Quake ()
spInfoPlayerStart _ = io (putStrLn "PlayerClient.spInfoPlayerStart") >> undefined -- TODO

spInfoPlayerDeathmatch :: EdictReference -> Quake ()
spInfoPlayerDeathmatch _ = io (putStrLn "PlayerClient.spInfoPlayerDeathmatch") >> undefined -- TODO

spInfoPlayerCoop :: EdictReference -> Quake ()
spInfoPlayerCoop _ = io (putStrLn "PlayerClient.spInfoPlayerCoop") >> undefined -- TODO

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = return ()
