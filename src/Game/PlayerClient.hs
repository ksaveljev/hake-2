module Game.PlayerClient
  ( saveClientData
  , spInfoPlayerCoop
  , spInfoPlayerDeathmatch
  , spInfoPlayerIntermission
  , spInfoPlayerStart
  ) where

import qualified Constants
import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.GameLocalsT
import           Game.GClientT
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

import           Control.Lens (use, (^.), (&), (.~))
import           Data.Bits ((.&.), (.|.))

saveClientData :: Quake ()
saveClientData =
  do maxClients <- use (gameBaseGlobals.gbGame.glMaxClients)
     coop <- coopCVar
     mapM_ (\idx -> updateClient coop idx =<< readRef (Ref (idx + 1))) [0..maxClients-1]

updateClient :: CVarT -> Int -> EdictT -> Quake ()
updateClient coop idx edict
  | edict^.eInUse =
      do score <- updateClientScore <$> readRef (Ref idx)
         modifyRef (Ref idx) (\v -> v & gcPers.cpHealth .~ (edict^.eHealth)
                                      & gcPers.cpMaxHealth .~ (edict^.eMaxHealth)
                                      & gcPers.cpSavedFlags .~ (edict^.eFlags) .&. (Constants.flGodMode .|. Constants.flNoTarget .|. Constants.flPowerArmor)
                                      & gcPers.cpScore .~ score)
  | otherwise = return ()
  where updateClientScore client
          | (coop^.cvValue) /= 0 = client^.gcResp.crScore
          | otherwise = client^.gcPers.cpScore

spInfoPlayerCoop :: Ref EdictT -> Quake ()
spInfoPlayerCoop = error "PlayerClient.spInfoPlayerCoop" -- TODO

spInfoPlayerDeathmatch :: Ref EdictT -> Quake ()
spInfoPlayerDeathmatch = error "PlayerClient.spInfoPlayerDeathmatch" -- TODO

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = error "PlayerClient.spInfoPlayerIntermission" -- TODO

spInfoPlayerStart :: Ref EdictT -> Quake ()
spInfoPlayerStart = error "PlayerClient.spInfoPlayerStart" -- TODO