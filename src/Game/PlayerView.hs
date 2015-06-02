{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.PlayerView where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom, (*=), (+=), (-=), (%=))
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (isJust, isNothing)
import Linear (V3, _x, _y, _z)

import Quake
import QuakeState
import qualified Constants
import qualified Game.GameItems as GameItems
import qualified Game.Monsters.MPlayer as MPlayer
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
setClientEffects (EdictReference edictIdx) = do
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState) $ do
      esEffects .= 0
      esRenderFx .= 0

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime

    unless ((edict^.eEdictStatus.eHealth) <= 0 || intermissionTime /= 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime

      when ((edict^.eEdictStatus.ePowerArmorTime) > levelTime) $ do
        let paType = GameItems.powerArmorType edict

        if | paType == Constants.powerArmorScreen ->
               gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efPowerScreen)

           | paType == Constants.powerArmorShield -> do
               gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efColorShell)
               gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esRenderFx %= (.|. Constants.rfShellGreen)

           | otherwise -> return ()

      let Just (GClientReference gClientIdx) = edict^.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

      when (truncate (gClient^.gcQuadFrameNum) > frameNum) $ do
        let remaining = truncate (gClient^.gcQuadFrameNum) - frameNum
        when (remaining > 30 || (remaining .&. 4) /= 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efQuad)

      when (truncate (gClient^.gcInvincibleFrameNum) > frameNum) $ do
        let remaining = truncate (gClient^.gcInvincibleFrameNum) - frameNum
        when (remaining > 30 || (remaining .&. 4) /= 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efPent)

      -- show cheaters!!!
      when ((edict^.eFlags) .&. Constants.flGodMode /= 0) $ do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState) $ do
          esEffects %= (.|. Constants.efColorShell)
          esRenderFx %= (.|. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue))

setClientSound :: EdictReference -> Quake ()
setClientSound edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just gClientRef = edict^.eClient

    checkHelpChanged gClientRef

    -- help beep (no more than three times)
    helpBeep gClientRef

    setSound gClientRef

  where checkHelpChanged :: GClientReference -> Quake ()
        checkHelpChanged (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          helpChanged <- use $ gameBaseGlobals.gbGame.glHelpChanged

          when ((gClient^.gcPers.cpGameHelpChanged) /= helpChanged) $ do
            zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers) $ do
              cpGameHelpChanged .= helpChanged
              cpHelpChanged .= 1

        helpBeep :: GClientReference -> Quake ()
        helpBeep (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

          when ((gClient^.gcPers.cpHelpChanged) /= 0 && (gClient^.gcPers.cpHelpChanged) <= 3 && (frameNum .&. 63) == 0) $ do
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpHelpChanged += 1
            gameImport <- use $ gameBaseGlobals.gbGameImport
            let sound = gameImport^.giSound
                soundIndex = gameImport^.giSoundIndex

            idx <- soundIndex (Just "misc/pc_up.wav")

            sound (Just edictRef) Constants.chanVoice idx 1 Constants.attnStatic 0

        setSound :: GClientReference -> Quake ()
        setSound (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          weap <- case gClient^.gcPers.cpWeapon of
                    Nothing -> return ""
                    Just (GItemReference weaponIdx) -> do
                      Just weapon <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx
                      return $ weapon^.giClassName

          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

          snd <- if | (edict^.eWaterLevel) /= 0 && (edict^.eWaterType) .&. (Constants.contentsLava .|. Constants.contentsSlime) /= 0 ->
                        use $ gameBaseGlobals.gbSndFry

                    | weap == "weapon_railgun" -> soundIndex (Just "weapons/rg_hum.wav")

                    | weap == "weapon_bfg" -> soundIndex (Just "weapons/bfg_hum.wav")

                    | (gClient^.gcWeaponSound) /= 0 -> return (gClient^.gcWeaponSound)

                    | otherwise -> return 0

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSound .= snd

setClientFrame :: EdictReference -> Quake ()
setClientFrame (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    -- only when we are in the player model
    when ((edict^.eEntityState.esModelIndex) == 255) $ do
      let Just (GClientReference gClientIdx) = edict^.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

      let duck = if fromIntegral (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                   then True
                   else False

      xyspeed <- use $ gameBaseGlobals.gbXYSpeed

      let run = if xyspeed /= 0 then True else False
          skip = checkSkip edict gClient duck run

      done <- checkIfDone edict gClient

      unless done $ do
        -- return to either a running or standing frame
        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcAnimPriority .= Constants.animBasic
          gcAnimDuck .= duck
          gcAnimRun .= run

        if | isNothing (edict^.eEdictOther.eoGroundEntity) -> do
               gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimPriority .= Constants.animJump
               when ((edict^.eEntityState.esFrame) /= MPlayer.frameJump2) $
                 gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameJump1
               gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameJump2

           | run -> do -- running
               if duck
                 then do
                   gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameCRWalk1
                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRWalk6
                 else do
                   gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameRun1
                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameRun6

           | otherwise -> do -- standing
               if duck
                 then do
                   gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameCRStnd01
                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRStnd19
                 else do
                   gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameStand01
                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameStand40

  where checkSkip :: EdictT -> GClientT -> Bool -> Bool -> Bool
        checkSkip edict gClient duck run =
          let a = if duck /= (gClient^.gcAnimDuck) && (gClient^.gcAnimPriority) < Constants.animDeath
                    then True
                    else False
              b = if run /= (gClient^.gcAnimRun) && (gClient^.gcAnimPriority) == Constants.animBasic
                    then True
                    else False
              c = if isNothing (edict^.eEdictOther.eoGroundEntity) && (gClient^.gcAnimPriority) <= Constants.animWave
                    then True
                    else False
          in a || b || c

        checkIfDone :: EdictT -> GClientT -> Quake Bool
        checkIfDone edict gClient = do
          if (gClient^.gcAnimPriority) == Constants.animReverse
            then if (edict^.eEntityState.esFrame) > (gClient^.gcAnimEnd)
                   then do
                     gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame -= 1
                     return True
                   else
                     checkDeath edict gClient

            else if (edict^.eEntityState.esFrame) < (gClient^.gcAnimEnd) -- continue an animation
                   then do
                     gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame += 1
                     return True
                   else
                     checkDeath edict gClient

        checkDeath :: EdictT -> GClientT -> Quake Bool
        checkDeath edict gClient =
          if (gClient^.gcAnimPriority) == Constants.animDeath
            then return True -- stay there
            else checkJump edict gClient

        checkJump :: EdictT -> GClientT -> Quake Bool
        checkJump edict gClient = do
          if (gClient^.gcAnimPriority) == Constants.animJump
            then do
              if isNothing (edict^.eEdictOther.eoGroundEntity)
                then return True -- stay there
                else do
                  let Just (GClientReference gClientIdx) = edict^.eClient
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimPriority .= Constants.animWave
                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameJump3
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameJump6
                  return True
            else
              return False
