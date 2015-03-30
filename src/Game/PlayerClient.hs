{-# LANGUAGE Rank2Types #-}
module Game.PlayerClient where

import Control.Lens (Traversal', use, (^.), ix, preuse, (.=))
import Control.Monad (when, liftM)
import Data.Bits ((.|.), (.&.))
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants

-- Called when a player drops from the server. Will not be called between levels. 
clientDisconnect :: Traversal' QuakeState (Maybe Int) -> Quake ()
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
