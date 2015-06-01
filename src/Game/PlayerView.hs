{-# LANGUAGE MultiWayIf #-}
module Game.PlayerView where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom, (*=), (+=))
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import Data.Maybe (isJust)
import Linear (V3, _x, _y, _z)

import Quake
import QuakeState
import qualified Constants
import qualified Game.PlayerHud as PlayerHud
import qualified Util.Math3D as Math3D

{-
- Called for each player at the end of the server frame and right after
- spawning.
-}
clientEndServerFrame :: EdictReference -> Quake ()
clientEndServerFrame edictRef@(EdictReference edictIdx) = do
    setCurrentPlayerAndClient

    -- If the origin or velocity have changed since ClientThink(),
    -- update the pmove values. This will happen when the client
    -- is pushed by a bmodel or kicked by an explosion.
    -- 
    -- If it wasn't updated here, the view position would lag a frame
    -- behind the body position when pushed -- "sinking into plats"
    updatePMoveValues

    -- If the end of unit layout is displayed, don't give
    -- the player any normal movement attributes
    done <- checkIntermissionTime

    unless done $ do
      setupAngleVectors

      -- burn from lava, etc
      worldEffects

      -- set model angles from view angles so other things in
      -- the world can tell which direction you are looking
      setModelAngles

      -- calculate speed and cycle to be used for
      -- all cyclic walking effects
      calculateSpeedAndCycle

      -- detect hitting the floor
      fallingDamage edictRef

      -- apply all the damage taken this frame
      damageFeedback edictRef

      -- determine the view offsets
      calcViewOffset edictRef

      -- determine the gun offsets
      calcGunOffset edictRef

      -- determine the full screen color blend
      -- must be after viewoffset, so eye contents can be
      -- accurately determined
      -- FIXME: with client prediction, the contents
      -- should be determined by the client
      calcBlend edictRef

      -- chase cam stuff
      setCameraStuff

      PlayerHud.checkChaseStats edictRef

      setClientEvent edictRef

      setClientEffects edictRef

      setClientSound edictRef

      setClientFrame edictRef

      io (putStrLn "PlayerView.clientEndServerFrame") >> undefined -- TODO

  where setCurrentPlayerAndClient :: Quake ()
        setCurrentPlayerAndClient = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          gameBaseGlobals.gbCurrentPlayer .= Just edictRef
          gameBaseGlobals.gbCurrentClient .= edict^.eClient

        updatePMoveValues :: Quake ()
        updatePMoveValues = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          Just (GClientReference gClientIdx) <- use $ gameBaseGlobals.gbCurrentClient
          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psPMoveState) $ do
            pmsOrigin .= fmap (truncate . (* 8.0)) (edict^.eEntityState.esOrigin)
            pmsVelocity .= fmap (truncate . (* 8.0)) (edict^.eEdictPhysics.eVelocity)

        checkIntermissionTime :: Quake Bool
        checkIntermissionTime = do
          intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime
          if intermissionTime /= 0
            then do
              -- FIXME: add view drifting here?
              Just (GClientReference gClientIdx) <- use $ gameBaseGlobals.gbCurrentClient
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psBlend._z .= 0
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psFOV .= 90
              PlayerHud.setStats edictRef
              return True
            else
              return False

        setupAngleVectors :: Quake ()
        setupAngleVectors = do
          Just (GClientReference gClientIdx) <- use $ gameBaseGlobals.gbCurrentClient
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          let (Just forward, Just right, Just up) = Math3D.angleVectors (gClient^.gcVAngle) True True True
          gameBaseGlobals.gbForward .= forward
          gameBaseGlobals.gbRight .= right
          gameBaseGlobals.gbUp .= up

        setModelAngles :: Quake ()
        setModelAngles = do
          Just (GClientReference gClientIdx) <- use $ gameBaseGlobals.gbCurrentClient
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          if (gClient^.gcVAngle.(Math3D.v3Access Constants.pitch)) > 180
            then gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin.(v3setter Constants.pitch) .= ((-360) + (gClient^.gcVAngle.(Math3D.v3Access Constants.pitch))) / 3
            else gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin.(v3setter Constants.pitch) .= (gClient^.gcVAngle.(Math3D.v3Access Constants.pitch)) / 3

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin.(v3setter Constants.yaw) .= (gClient^.gcVAngle.(Math3D.v3Access Constants.yaw))
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin.(v3setter Constants.roll) .= 0

          preuse (gameBaseGlobals.gbGEdicts.ix edictIdx) >>= \(Just edict) -> do
            roll <- calcRoll (edict^.eEntityState.esAngles) (edict^.eEdictPhysics.eVelocity)
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin.(v3setter Constants.roll) .= roll * 4

        v3setter x = case x of
                       0 -> _x
                       1 -> _y
                       2 -> _z
                       _ -> undefined -- shouldn't happen

        calculateSpeedAndCycle :: Quake ()
        calculateSpeedAndCycle = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          Just (GClientReference gClientIdx) <- use $ gameBaseGlobals.gbCurrentClient

          let velocity = edict^.eEdictPhysics.eVelocity
              xyspeed = sqrt ((velocity^._x) * (velocity^._x) + (velocity^._y) * (velocity^._y))

          if | xyspeed < 5 -> do
                 gameBaseGlobals.gbBobMove .= 0
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcBobTime .= 0 -- start at beginning of cycle again

             | isJust (edict^.eEdictOther.eoGroundEntity) -> do
                 gameBaseGlobals.gbBobMove .= if | xyspeed > 210 -> 0.25
                                                 | xyspeed > 100 -> 0.125
                                                 | otherwise -> 0.0625

             | otherwise -> return ()

          use (gameBaseGlobals.gbBobMove) >>= \bobmove ->
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcBobTime += bobmove

          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          let bobTime = gClient^.gcBobTime
              bobTime' = if fromIntegral (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                           then bobTime * 4
                           else bobTime

          gameBaseGlobals.gbBobCycle .= truncate bobTime'
          gameBaseGlobals.gbBobFracSin .= abs (sin (bobTime' * pi))

        setCameraStuff :: Quake ()
        setCameraStuff = do
          io (putStrLn "PlayerView.setCameraSTuff") >> undefined -- TODO

worldEffects :: Quake ()
worldEffects = do
    io (putStrLn "PlayerView.worldEffects") >> undefined -- TODO

calcRoll :: V3 Float -> V3 Float -> Quake Float
calcRoll _ _ = do
    io (putStrLn "PlayerView.calcRoll") >> undefined -- TODO

fallingDamage :: EdictReference -> Quake ()
fallingDamage _ = do
    io (putStrLn "PlayerView.fallingDamage") >> undefined -- TODO

damageFeedback :: EdictReference -> Quake ()
damageFeedback _ = do
    io (putStrLn "PlayerView.damageFeedback") >> undefined -- TODO

calcViewOffset :: EdictReference -> Quake ()
calcViewOffset _ = do
    io (putStrLn "PlayerView.calcViewOffset") >> undefined -- TODO

calcGunOffset :: EdictReference -> Quake ()
calcGunOffset _ = do
    io (putStrLn "PlayerView.calcGunOffset") >> undefined -- TODO

calcBlend :: EdictReference -> Quake ()
calcBlend _ = do
    io (putStrLn "PlayerView.calcBlend") >> undefined -- TODO

setClientEvent :: EdictReference -> Quake ()
setClientEvent _ = do
    io (putStrLn "PlayerView.setClientEvent") >> undefined -- TODO

setClientEffects :: EdictReference -> Quake ()
setClientEffects _ = do
    io (putStrLn "PlayerView.setClientEffects") >> undefined -- TODO

setClientSound :: EdictReference -> Quake ()
setClientSound _ = do
    io (putStrLn "PlayerView.setClientSound") >> undefined -- TODO

setClientFrame :: EdictReference -> Quake ()
setClientFrame _ = do
    io (putStrLn "PlayerView.setClientFrame") >> undefined -- TODO
