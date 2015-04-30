{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerClient where

import Control.Lens (Traversal', use, (^.), ix, preuse, (.=), zoom)
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.|.), (.&.))
import Data.Char (toLower)
import Data.Maybe (isNothing)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameItems as GameItems
import qualified Game.GameMisc as GameMisc
import qualified Game.GameSVCmds as GameSVCmds
import qualified Game.GameUtil as GameUtil
import qualified Game.Info as Info

-- Called when a player drops from the server. Will not be called between levels. 
clientDisconnect :: Traversal' QuakeState (Maybe EdictReference) -> Quake ()
clientDisconnect _ = io (putStrLn "PlayerClient.clientDisconnect") >> undefined -- TODO

{-
- Called when a player begins connecting to the server. The game can refuse
- entrance to a client by returning false. If the client is allowed, the
- connection process will continue and eventually get to ClientBegin()
- Changing levels will NOT cause this to be called again, but loadgames
- will. 
-}
clientConnect :: EdictReference -> B.ByteString -> Quake (Bool, B.ByteString)
clientConnect edictRef@(EdictReference edictIdx) userInfo = do
    -- check to see if they are on the banned IP list
    value <- Info.valueForKey userInfo "ip"
    banned <- GameSVCmds.filterPacket value
    if banned
      then do
        userInfo' <- Info.setValueForKey userInfo "rejmsg" "Banned."
        return (False, userInfo')
      else do
        -- check for a spectator
        value' <- Info.valueForKey userInfo "spectator"
        deathmatchValue <- liftM (^.cvValue) deathmatchCVar

        (done, userInfo') <- if deathmatchValue /= 0 && B.length value' > 0 && value' /= "0"
                               then do
                                 io (putStrLn "PlayerClient.clientConnect") >> undefined -- TODO
                               else do
                                 -- check for a password
                                 pass <- Info.valueForKey userInfo "password"
                                 actualPassword <- liftM (^.cvString) spectatorPasswordCVar
                                 if not (passwordOK actualPassword pass)
                                   then do
                                     info <- Info.setValueForKey userInfo "rejmsg" "Password required or incorrect."
                                     return (True, info)
                                   else
                                     return (False, userInfo)

        if done
          then 
            return (False, userInfo')
          else do
            -- they can connect
            let gClientIdx = edictIdx - 1
                gClientRef = GClientReference gClientIdx
            gameBaseGlobals.gbGEdicts.ix edictIdx.eClient .= Just gClientRef

            -- if there is already a body waiting for us (a loadgame), just
            -- take it, otherwise spawn one from scratch
            Just inUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eInUse
            unless inUse $ do
              -- clear the respawning variables
              initClientResp gClientRef
              autoSaved <- use $ gameBaseGlobals.gbGame.glAutosaved
              Just weapon <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpWeapon

              when (not autoSaved || isNothing weapon) $
                initClientPersistant gClientRef

            userInfo'' <- clientUserInfoChanged edictRef userInfo

            maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients

            when (maxClients > 1) $ do
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              Just netName <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpNetName
              dprintf $ netName `B.append` " connected\n"

            gameBaseGlobals.gbGEdicts.ix edictIdx.eSvFlags .= 0 -- make sure we start with known default
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpConnected .= True

            return (True, userInfo'')

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
spInfoPlayerStart (EdictReference edictIdx) = do
    coopValue <- liftM (^.cvValue) coopCVar

    unless (coopValue == 0) $ do
      mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

      when (mapName == "security") $ do
        time <- use $ gameBaseGlobals.gbLevel.llTime
        -- invoke one of our gross, ugly, disgusting hacks
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
          eaThink .= Just spCreateCoopSpots
          eaNextThink .= time + Constants.frameTime

{-
- QUAKED info_player_deathmatch (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for deathmatch games.
-}
spInfoPlayerDeathmatch :: EdictReference -> Quake ()
spInfoPlayerDeathmatch er = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue == 0
      then GameUtil.freeEdict er
      else void $ think GameMisc.spMiscTeleporterDest er

{-
- QUAKED info_player_coop (1 0 1) (-16 -16 -24) (16 16 32) potential
- spawning position for coop games.
-}
spInfoPlayerCoop :: EdictReference -> Quake ()
spInfoPlayerCoop er@(EdictReference edictIdx) = do
    coopValue <- liftM (^.cvValue) coopCVar

    if coopValue == 0
      then GameUtil.freeEdict er
      else do
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

        let names = [ "jail2", "jail4", "mine1", "mine2", "mine3", "mine4" , "lab"
                    , "boss1", "fact3", "biggun", "space", "command", "power2", "strike" ]

        when (any (== mapName) names) $ do
          -- invoke one of our gross, ugly, disgusting hacks
          time <- use $ gameBaseGlobals.gbLevel.llTime

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaThink .= Just spFixCoopSpots
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaNextThink .= time + Constants.frameTime

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = return ()

spFixCoopSpots :: EntThink
spFixCoopSpots =
  GenericEntThink "SP_FixCoopSpots" $ \_ -> do
    io (putStrLn "PlayerClient.spFixCoopSpots") >> undefined -- TODO

spCreateCoopSpots :: EntThink
spCreateCoopSpots =
  GenericEntThink "SP_CreateCoopSpots" $ \_ -> do
    io (putStrLn "PlayerClient.spCreateCoopSpots") >> undefined -- TODO

clientBeginServerFrame :: EdictReference -> Quake ()
clientBeginServerFrame _ = io (putStrLn "PlayerClient.clientBeginServerFrame") >> undefined -- TODO

initClientResp :: GClientReference -> Quake ()
initClientResp (GClientReference gClientIdx) = do
    frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
    Just pers <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcResp .= newClientRespawnT { _crEnterFrame = frameNum, _crCoopRespawn = pers }

{-
- This is only called when the game first initializes in single player, but
- is called after each death and level change in deathmatch. 
-}
initClientPersistant :: GClientReference -> Quake ()
initClientPersistant (GClientReference gClientIdx) = do
    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers .= newClientPersistantT

    Just itemRef@(GItemReference itemIdx) <- GameItems.findItem "Blaster"

    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers) $ do
      cpSelectedItem .= itemIdx
      cpInventory.ix itemIdx .= 1

      cpWeapon .= Just itemRef

      cpHealth .= 100
      cpMaxHealth .= 100

      cpMaxBullets .= 200
      cpMaxShells .= 100
      cpMaxRockets .= 50
      cpMaxGrenades .= 50
      cpMaxCells .= 200
      cpMaxSlugs .= 50

      cpConnected .= True

clientUserInfoChanged :: EdictReference -> B.ByteString -> Quake B.ByteString
clientUserInfoChanged _ _ = do
    io (putStrLn "PlayerClient.clientUserInfoChanged") >> undefined -- TODO

passwordOK :: B.ByteString -> B.ByteString -> Bool
passwordOK p1 p2 =
    if B.length p1 > 0 && p1 /= "none" && p1 /= p2
      then False
      else True
