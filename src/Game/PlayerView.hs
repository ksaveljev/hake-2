{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.PlayerView where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom, (*=), (+=), (-=), (%=), (%~))
import Control.Monad (unless, when, liftM)
import Data.Bits ((.&.), (.|.), complement, xor)
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (V3(..), _x, _y, _z, dot, normalize)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameCombat as GameCombat
import qualified Game.GameItems as GameItems
import qualified Game.Monsters.MPlayer as MPlayer
import qualified Game.PlayerHud as PlayerHud
import qualified Game.PlayerWeapon as PlayerWeapon
import qualified Util.Lib as Lib
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

-- General effect handling for a player.
worldEffects :: Quake ()
worldEffects = do
    Just edictRef@(EdictReference edictIdx) <- use $ gameBaseGlobals.gbCurrentPlayer
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if (edict^.eMoveType) == Constants.moveTypeNoClip
      then do
        -- don't need air
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eAirFinished .= levelTime + 12
      else do
        Just gClientRef@(GClientReference gClientIdx) <- use $ gameBaseGlobals.gbCurrentClient
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
        frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

        let waterLevel = edict^.eWaterLevel
            oldWaterLevel = gClient^.gcOldWaterLevel
        
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcOldWaterLevel .= waterLevel

        let breather = truncate (gClient^.gcBreatherFrameNum) > frameNum
            enviroSuit = truncate (gClient^.gcEnviroFrameNum) > frameNum

        -- if just entered a water volume, play a sound
        waterEnterPlaySound edictRef waterLevel oldWaterLevel

        -- if just completely exited a water volume, play a sound
        waterExitPlaySound edictRef waterLevel oldWaterLevel

        -- check for head just going under water
        checkHeadUnderWater edictRef waterLevel oldWaterLevel

        -- check for head just coming out of water
        checkHeadOutOfWater edictRef waterLevel oldWaterLevel

        -- check for drowning
        checkForDrowning edictRef gClientRef waterLevel breather enviroSuit

        -- check for sizzle damage
        checkForSizzleDamage edictRef gClientRef waterLevel enviroSuit

  where waterEnterPlaySound :: EdictReference -> Int -> Int -> Quake ()
        waterEnterPlaySound edictRef@(EdictReference edictIdx) waterLevel oldWaterLevel =
          when (oldWaterLevel == 0 && waterLevel /= 0) $ do
            Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
            gameImport <- use $ gameBaseGlobals.gbGameImport
            let sound = gameImport^.giSound
                soundIndex = gameImport^.giSoundIndex

            PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf

            idx <- if | (edict^.eWaterType) .&. Constants.contentsLava /= 0 ->
                          soundIndex (Just "player/lava_in.wav") >>= return . Just

                      | (edict^.eWaterType) .&. Constants.contentsSlime /= 0 ->
                          soundIndex (Just "player/watr_in.wav") >>= return . Just

                      | (edict^.eWaterType) .&. Constants.contentsWater /= 0 ->
                          soundIndex (Just "player/watr_in.wav") >>= return . Just
                         
                      | otherwise -> return Nothing

            when (isJust idx) $
              sound (Just edictRef) Constants.chanBody (fromJust idx) 1 Constants.attnNorm 0

            gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags %= (.|. Constants.flInWater)

            -- clear damage_debounce, so the pain sound will play immediately
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictTiming.etDamageDebounceTime .= levelTime - 1

        waterExitPlaySound :: EdictReference -> Int -> Int -> Quake ()
        waterExitPlaySound edictRef@(EdictReference edictIdx) waterLevel oldWaterLevel =
          when (oldWaterLevel /= 0 && waterLevel == 0) $ do
            Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
            gameImport <- use $ gameBaseGlobals.gbGameImport
            let sound = gameImport^.giSound
                soundIndex = gameImport^.giSoundIndex

            PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf

            idx <- soundIndex (Just "player/watr_out.wav")
            sound (Just edictRef) Constants.chanBody idx 1 Constants.attnNorm 0

            gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags %= (.&. (complement Constants.flInWater))

        checkHeadUnderWater :: EdictReference -> Int -> Int -> Quake ()
        checkHeadUnderWater edictRef@(EdictReference edictIdx) waterLevel oldWaterLevel =
          when (oldWaterLevel /= 3 && waterLevel == 3) $ do
            gameImport <- use $ gameBaseGlobals.gbGameImport
            let sound = gameImport^.giSound
                soundIndex = gameImport^.giSoundIndex

            idx <- soundIndex (Just "player/watr_un.wav")
            sound (Just edictRef) Constants.chanBody idx 1 Constants.attnNorm 0

        checkHeadOutOfWater :: EdictReference -> Int -> Int -> Quake ()
        checkHeadOutOfWater edictRef@(EdictReference edictIdx) waterLevel oldWaterLevel =
          when (oldWaterLevel == 3 && waterLevel /= 3) $ do
            Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            gameImport <- use $ gameBaseGlobals.gbGameImport
            let sound = gameImport^.giSound
                soundIndex = gameImport^.giSoundIndex

            if | (edict^.eEdictPhysics.eAirFinished) < levelTime -> do -- gasp for air
                   idx <- soundIndex (Just "player/gasp1.wav")
                   sound (Just edictRef) Constants.chanVoice idx 1 Constants.attnNorm 0
                   PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf

               | (edict^.eEdictPhysics.eAirFinished) < levelTime + 11 -> do -- just break surface
                   idx <- soundIndex (Just "player/gasp2.wav")
                   sound (Just edictRef) Constants.chanVoice idx 1 Constants.attnNorm 0

               | otherwise -> return ()

        checkForDrowning :: EdictReference -> GClientReference -> Int -> Bool -> Bool -> Quake ()
        checkForDrowning edictRef@(EdictReference edictIdx) gClientRef@(GClientReference gClientIdx) waterLevel breather enviroSuit =
          if waterLevel == 3
            then do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime
              gameImport <- use $ gameBaseGlobals.gbGameImport
              let sound = gameImport^.giSound
                  soundIndex = gameImport^.giSoundIndex

              -- breather or envirosuit give air
              when (breather || enviroSuit) $ do
                gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eAirFinished .= levelTime + 10
                Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
                Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
                frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

                when ((truncate (gClient^.gcBreatherFrameNum) - frameNum) `mod` 25 == 0) $ do
                  idx <- if (gClient^.gcBreatherSound) == 0
                           then soundIndex (Just "player/u_breath1.wav")
                           else soundIndex (Just "player/u_breath2.wav")
                  sound (Just edictRef) Constants.chanAuto idx 1 Constants.attnNorm 0
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcBreatherSound %= (`xor` 1)
                  PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf
                  -- FIXME: release a bubble?

              -- if out of air, start drowning
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
              
              when ((edict^.eEdictPhysics.eAirFinished) < levelTime) $ do -- drown!
                let Just (GClientReference clientIdx) = edict^.eClient
                Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix clientIdx

                when ((client^.gcNextDrownTime) < levelTime && (edict^.eEdictStatus.eHealth) > 0) $ do
                  gameBaseGlobals.gbGame.glClients.ix clientIdx.gcNextDrownTime .= levelTime + 1

                  -- take more damage the longer underwater
                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictStatus.eDmg %= (\v -> if v + 2 > 15 then 15 else v + 2)

                  -- play a gurp sound instead of a normal pain sound
                  playDrowningSound edictRef

                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictTiming.etPainDebounceTime .= levelTime

                  v3o <- use $ globals.vec3Origin
                  preuse (gameBaseGlobals.gbGEdicts.ix edictIdx) >>= \(Just ent) ->
                    GameCombat.damage edictRef
                                      (EdictReference 0)
                                      (EdictReference 0)
                                      v3o
                                      (ent^.eEntityState.esOrigin)
                                      v3o
                                      (ent^.eEdictStatus.eDmg)
                                      0
                                      Constants.damageNoArmor
                                      Constants.modWater
            else do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime

              zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
                eEdictPhysics.eAirFinished .= levelTime + 12
                eEdictStatus.eDmg .= 2

        playDrowningSound :: EdictReference -> Quake ()
        playDrowningSound edictRef@(EdictReference edictIdx) = do
          gameImport <- use $ gameBaseGlobals.gbGameImport
          let sound = gameImport^.giSound
              soundIndex = gameImport^.giSoundIndex

          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          r <- Lib.rand

          idx <- if | (edict^.eEdictStatus.eHealth) <= (edict^.eEdictStatus.eDmg) ->
                        soundIndex (Just "player/drown1.wav")
                    | r .&. 1 /= 0 ->
                        soundIndex (Just "*gurp1.wav")
                    | otherwise ->
                        soundIndex (Just "*gurp2.wav")

          sound (Just edictRef) Constants.chanVoice idx 1 Constants.attnNorm 0

        checkForSizzleDamage :: EdictReference -> GClientReference -> Int -> Bool -> Quake ()
        checkForSizzleDamage edictRef@(EdictReference edictIdx) gClientRef@(GClientReference gClientIdx) waterLevel enviroSuit = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          when (waterLevel /= 0 && (edict^.eWaterType) .&. (Constants.contentsLava .|. Constants.contentsSlime) /= 0) $ do
            when ((edict^.eWaterType) .&. Constants.contentsLava /= 0) $ do
              Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime
              frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

              when ((edict^.eEdictStatus.eHealth) > 0 && (edict^.eEdictTiming.etPainDebounceTime) <= levelTime && truncate (gClient^.gcInvincibleFrameNum) < frameNum) $ do
                r <- Lib.rand

                gameImport <- use $ gameBaseGlobals.gbGameImport
                let sound = gameImport^.giSound
                    soundIndex = gameImport^.giSoundIndex

                idx <- if r .&. 1 /= 0
                         then soundIndex (Just "player/burn1.wav")
                         else soundIndex (Just "player/burn2.wav")

                sound (Just edictRef) Constants.chanVoice idx 1 Constants.attnNorm 0

                gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictTiming.etPainDebounceTime .= levelTime + 1

              v3o <- use $ globals.vec3Origin
              if enviroSuit -- take 1/3 damage with envirosuit
                then GameCombat.damage edictRef
                                       (EdictReference 0)
                                       (EdictReference 0)
                                       v3o
                                       (edict^.eEntityState.esOrigin)
                                       v3o
                                       waterLevel
                                       0
                                       0
                                       Constants.modLava
                else GameCombat.damage edictRef
                                       (EdictReference 0)
                                       (EdictReference 0)
                                       v3o
                                       (edict^.eEntityState.esOrigin)
                                       v3o
                                       (3 * waterLevel)
                                       0
                                       0
                                       Constants.modLava

            when ((edict^.eWaterType) .&. Constants.contentsSlime /= 0) $ do
              unless enviroSuit $ do -- no damage from slime with envirosuit
                v3o <- use $ globals.vec3Origin
                GameCombat.damage edictRef
                                  (EdictReference 0)
                                  (EdictReference 0)
                                  v3o
                                  (edict^.eEntityState.esOrigin)
                                  v3o
                                  waterLevel
                                  0
                                  0
                                  Constants.modSlime

calcRoll :: V3 Float -> V3 Float -> Quake Float
calcRoll angles velocity = do
    right <- use $ gameBaseGlobals.gbRight

    let side = velocity `dot` right
        sign = if side < 0 then -1 else 1
        side' = abs side

    rollAngleValue <- liftM (^.cvValue) svRollAngleCVar
    rollSpeedValue <- liftM (^.cvValue) svRollSpeedCVar

    let side'' = if side' < rollSpeedValue
                   then side' * rollAngleValue / rollSpeedValue
                   else rollAngleValue

    return (side'' * sign)

-- Calculated damage and effect when a player falls down.
fallingDamage :: EdictReference -> Quake ()
fallingDamage edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    unless ((edict^.eEntityState.esModelIndex) /= 255 || (edict^.eMoveType) == Constants.moveTypeNoClip) $ do
      let Just (GClientReference gClientIdx) = edict^.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      let maybeDelta = if (gClient^.gcOldVelocity._z) < 0 && (edict^.eEdictPhysics.eVelocity._z) > (gClient^.gcOldVelocity._z) && isNothing (edict^.eEdictOther.eoGroundEntity)
                         then Just (gClient^.gcOldVelocity._z)
                         else if isNothing (edict^.eEdictOther.eoGroundEntity)
                                then Nothing
                                else Just ((edict^.eEdictPhysics.eVelocity._z) - (gClient^.gcOldVelocity._z))

      case maybeDelta of
        Nothing -> return () -- we are done
        Just delta -> do
          maybeDelta' <- updateDelta edict (delta * delta * 0.0001)

          case maybeDelta' of
            Nothing -> return () -- we are done
            Just delta' -> do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcFallValue %= (\x -> if delta' * 0.5 > 40 then 40 else delta' * 0.5)
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcFallTime .= levelTime + Constants.fallTime

              if delta' > 30
                then do
                  when ((edict^.eEdictStatus.eHealth) > 0) $
                    if (edict^.eEdictStatus.eHealth) >= 55
                      then gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEvent .= Constants.evFallFar
                      else gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEvent .= Constants.evFall

                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictTiming.etPainDebounceTime .= levelTime -- no normal pain sound

                  let damage :: Int = truncate ((delta' - 30) / 2)
                      damage' = if damage < 1 then 1 else damage
                      dir = V3 0 0 1

                  deathmatchValue <- liftM (^.cvValue) deathmatchCVar
                  dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

                  when (deathmatchValue == 0 || dmFlagsValue .&. Constants.dfNoFalling == 0) $ do
                    v3o <- use $ globals.vec3Origin
                    GameCombat.damage edictRef
                                      (EdictReference 0)
                                      (EdictReference 0)
                                      dir
                                      (edict^.eEntityState.esOrigin)
                                      v3o
                                      damage'
                                      0
                                      0
                                      Constants.modFalling
                else
                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEvent .= Constants.evFallShort

  where updateDelta :: EdictT -> Float -> Quake (Maybe Float)
        updateDelta edict delta =
          -- never take falling damage if completely underwater
          if (edict^.eWaterLevel) == 3
            then return Nothing
            else let delta' = if | (edict^.eWaterLevel) == 2 -> delta * 0.25
                                 | (edict^.eWaterLevel) == 1 -> delta * 0.5
                                 | otherwise -> delta
                 in if | delta' < 1 -> return Nothing
                       | delta' < 15 -> do
                           gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEvent .= Constants.evFootstep
                           return Nothing
                       | otherwise -> return (Just delta')

{-
- =============== 
- P_DamageFeedback
- 
- Handles color blends and view kicks 
- ===============
-}
damageFeedback :: EdictReference -> Quake ()
damageFeedback playerRef@(EdictReference playerIdx) = do
    Just player <- preuse $ gameBaseGlobals.gbGEdicts.ix playerIdx

    let Just (GClientReference gClientIdx) = player^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    -- flash the backgrounds behind the status numbers
    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statFlashes .= 0

    when ((gClient^.gcDamageBlood) /= 0) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statFlashes %= (.|. 1)

    when ((gClient^.gcDamageArmor) /= 0 && (player^.eFlags) .&. Constants.flGodMode == 0 && truncate (gClient^.gcInvincibleFrameNum) <= frameNum) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statFlashes %= (.|. 2)

    -- total points of damage shot at the player this frame
    let count = (gClient^.gcDamageBlood) + (gClient^.gcDamageArmor) + (gClient^.gcDamagePArmor)

    unless (count == 0) $ do -- unless (didn't take any damage)
      -- start a pain animation if still in the player model
      when ((gClient^.gcAnimPriority) < Constants.animPain && (player^.eEntityState.esModelIndex) == 255) $ do
        if fromIntegral (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
          then do
            gameBaseGlobals.gbGEdicts.ix playerIdx.eEntityState.esFrame .= MPlayer.frameCRPain1 - 1
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRPain4
          else do
            gameBaseGlobals.gbXxxi %= (\x -> (x + 1) `mod` 3)
            xxxi <- use $ gameBaseGlobals.gbXxxi

            case xxxi of
              0 -> do
                gameBaseGlobals.gbGEdicts.ix playerIdx.eEntityState.esFrame .= MPlayer.framePain101 - 1
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.framePain104

              1 -> do
                gameBaseGlobals.gbGEdicts.ix playerIdx.eEntityState.esFrame .= MPlayer.framePain201 - 1
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.framePain204

              2 -> do
                gameBaseGlobals.gbGEdicts.ix playerIdx.eEntityState.esFrame .= MPlayer.framePain301 - 1
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.framePain304

              _ -> undefined -- shouldn't ever happen

      let realCount = count
          count' = if count < 10 then 10 else count -- always make a visible effect

      -- play an apropriate pain sound
      when (levelTime > (player^.eEdictTiming.etPainDebounceTime) && (player^.eFlags) .&. Constants.flGodMode == 0 && truncate (gClient^.gcInvincibleFrameNum) <= frameNum) $ do
        r <- Lib.rand
        let r' = 1 + (r .&. 1)

        gameBaseGlobals.gbGEdicts.ix playerIdx.eEdictTiming.etPainDebounceTime .= levelTime + 0.7

        let l = if | (player^.eEdictStatus.eHealth) < 25 -> 25
                   | (player^.eEdictStatus.eHealth) < 50 -> 50
                   | (player^.eEdictStatus.eHealth) < 75 -> 75
                   | otherwise -> 100


        gameImport <- use $ gameBaseGlobals.gbGameImport
        let sound = gameImport^.giSound
            soundIndex = gameImport^.giSoundIndex

        idx <- soundIndex (Just ("*pain" `B.append` BC.pack (show l) `B.append` "_" `B.append` BC.pack (show r') `B.append` ".wav")) -- IMPROVE?
        sound (Just playerRef) Constants.chanVoice idx 1 Constants.attnNorm 0

      -- the total alpha of the blend is always proportional to count
      when ((gClient^.gcDamageAlpha) < 0) $
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcDamageAlpha .= 0

      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcDamageAlpha += fromIntegral count' * 0.01
      preuse (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcDamageAlpha) >>= \(Just damageAlpha) ->
        if | damageAlpha < 0.2 ->gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcDamageAlpha .= 0.2
           | damageAlpha > 0.6 ->gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcDamageAlpha .= 0.6 -- don't go too saturated
           | otherwise -> return ()

      -- the color of the blend will vary based on how much was absorbed by
      -- different armors
      let powerColor :: V3 Float = V3 0 1 0
          aColor :: V3 Float = V3 1 1 1
          bColor :: V3 Float = V3 1 0 0

          realCountF :: Float = fromIntegral realCount

          v :: V3 Float = V3 0 0 0
          v' = if (gClient^.gcDamagePArmor) /= 0
                 then v + fmap (* (fromIntegral (gClient^.gcDamagePArmor) / realCountF)) powerColor
                 else v
          v'' = if (gClient^.gcDamageArmor) /= 0
                  then v' + fmap (* (fromIntegral (gClient^.gcDamageArmor) / realCountF)) aColor
                  else v'
          v''' = if (gClient^.gcDamageBlood) /= 0
                   then v'' + fmap (* (fromIntegral (gClient^.gcDamageBlood) / realCountF)) bColor
                   else v''

      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcDamageBlend .= v'''

      -- calculate view angle kicks
      let kick = abs (gClient^.gcDamageKnockback)

      when (kick /= 0 && (player^.eEdictStatus.eHealth) > 0) $ do -- kick of 0 means no view adjust at all
        right <- use $ gameBaseGlobals.gbRight
        forward <- use $ gameBaseGlobals.gbForward

        let kick' :: Float = (fromIntegral kick) * 100 / (fromIntegral $ player^.eEdictStatus.eHealth)
            kick'' = if kick' < fromIntegral count' * 0.5
                       then fromIntegral count' * 0.5
                       else kick'
            kick''' = if kick'' > 50
                        then 50
                        else kick''
            
            vv = (gClient^.gcDamageFrom) - (player^.eEntityState.esOrigin)
            vv' = normalize vv
            side = vv `dot` right

        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVDmgRoll .= kick''' * side * 0.3

        let side' = negate (vv `dot` forward)

        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVDmgPitch .= kick''' * side' * 0.3
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVDmgTime .= levelTime + Constants.damageTime

      -- clear totals
      zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
        gcDamageBlood .= 0
        gcDamageArmor .= 0
        gcDamagePArmor .= 0
        gcDamageKnockback .= 0

{-
- 
- fall from 128: 400 = 160000 
- fall from 256: 580 = 336400 
- fall from 384: 720 = 518400 
- fall from 512: 800 = 640000 
- fall from 640: 960 =  
- damage = deltavelocity*deltavelocity * 0.0001
-}
calcViewOffset :: EdictReference -> Quake ()
calcViewOffset (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just gClientRef@(GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    -- if dead, fix the angle and don't add any kick
    angles <- if (edict^.eEdictStatus.eDeadFlag) /= 0
                then do
                  zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psViewAngles) $ do
                    access Constants.roll .= 40
                    access Constants.pitch .= -15
                    access Constants.yaw .= gClient^.gcKillerYaw

                  return (V3 0 0 0)
                else do
                  right <- use $ gameBaseGlobals.gbRight
                  forward <- use $ gameBaseGlobals.gbForward

                      -- add angles based on weapon kick
                  let angles = gClient^.gcKickAngles

                  -- add angles based on damage kick
                  angles' <- addDamageKickAngles gClientRef angles
                  -- add angles based on fall kick
                  angles'' <- addFallKickAngles gClientRef angles'
                  -- add angles based on velocity
                  angles''' <- addVelocityAngles angles''
                  -- add angles based on bob
                  angles'''' <- addBobAngles gClientRef angles'''

                  return angles'''

    bobFracSin <- use $ gameBaseGlobals.gbBobFracSin
    xyspeed <- use $ gameBaseGlobals.gbXYSpeed
    bobUpValue <- liftM (^.cvValue) bobUpCVar
    let bob :: Float = bobFracSin * xyspeed * bobUpValue
        bob' = if bob > 6 then 6 else bob
        V3 a b c = (V3 0 0 (fromIntegral (edict^.eEdictStatus.eViewHeight) + bob')) + (gClient^.gcKickOrigin)
        a' = if | a < -14 -> -14
                | a > 14 -> 14
                | otherwise -> a
        b' = if | b < -14 -> -14
                | b > 14 -> 14
                | otherwise -> b
        c' = if | c < -22 -> -22
                | c > 30 -> 30
                | otherwise -> c

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psViewOffset .= V3 a' b' c'

  where addDamageKickAngles :: GClientReference -> V3 Float -> Quake (V3 Float)
        addDamageKickAngles (GClientReference gClientIdx) angles = do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          let ratio = ((gClient^.gcVDmgTime) - levelTime) / Constants.damageTime
          ratio' <- if ratio < 0
                      then do
                        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVDmgPitch .= 0
                        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcVDmgRoll .= 0
                        return 0
                      else
                        return ratio

          let angles' = (access Constants.pitch) %~ (+ (ratio' * (gClient^.gcVDmgPitch))) $ angles
              angles'' = (access Constants.roll) %~ (+ (ratio' * (gClient^.gcVDmgRoll))) $ angles'

          return angles''

        addFallKickAngles :: GClientReference -> V3 Float -> Quake (V3 Float)
        addFallKickAngles (GClientReference gClientIdx) angles = do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          let ratio = ((gClient^.gcFallTime) - levelTime) / Constants.fallTime
              ratio' = if ratio < 0 then 0 else ratio
              angles' = (access Constants.pitch) %~ (+ (ratio' * (gClient^.gcFallValue))) $ angles

          return angles'

        addVelocityAngles :: V3 Float -> Quake (V3 Float)
        addVelocityAngles angles = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          runPitchValue <- liftM (^.cvValue) runPitchCVar
          runRollValue <- liftM (^.cvValue) runRollCVar
          forward <- use $ gameBaseGlobals.gbForward
          right <- use $ gameBaseGlobals.gbRight

          let delta = (edict^.eEdictPhysics.eVelocity) `dot` forward
              delta' = (edict^.eEdictPhysics.eVelocity) `dot` right
              angles' = (access Constants.pitch) %~ (+ (delta * runPitchValue)) $ angles
              angles'' = (access Constants.roll) %~ (+ (delta' * runRollValue)) $ angles'

          return angles''

        addBobAngles :: GClientReference -> V3 Float -> Quake (V3 Float)
        addBobAngles (GClientReference gClientIdx) angles = do
          bobFracSin <- use $ gameBaseGlobals.gbBobFracSin
          bobCycle <- use $ gameBaseGlobals.gbBobCycle
          xyspeed <- use $ gameBaseGlobals.gbXYSpeed
          bobPitchValue <- liftM (^.cvValue) bobPitchCVar
          bobRollValue <- liftM (^.cvValue) bobRollCVar
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          let delta = bobFracSin * bobPitchValue * xyspeed
              delta' = if fromIntegral (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                         then delta * 6 -- crouching
                         else delta
              angles' = (access Constants.pitch) %~ (+ delta') $ angles

              delta'' = bobFracSin * bobRollValue * xyspeed
              delta''' = if fromIntegral (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                           then delta'' * 6 -- crouching
                           else delta''
              delta'''' = if bobCycle .&. 1 /= 0 then negate delta''' else delta'''
              angles'' = (access Constants.roll) %~ (+ delta'''') $ angles'

          return angles''

        access = \x -> case x of
                   0 -> _x
                   1 -> _y
                   2 -> _z
                   _ -> undefined -- shouldn't happen

calcGunOffset :: EdictReference -> Quake ()
calcGunOffset _ = do
    io (putStrLn "PlayerView.calcGunOffset") >> undefined -- TODO

calcBlend :: EdictReference -> Quake ()
calcBlend _ = do
    io (putStrLn "PlayerView.calcBlend") >> undefined -- TODO

setClientEvent :: EdictReference -> Quake ()
setClientEvent (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    unless ((edict^.eEntityState.esEvent) /= 0) $ do
      xyspeed <- use $ gameBaseGlobals.gbXYSpeed

      when (isJust (edict^.eEdictOther.eoGroundEntity) && xyspeed > 225) $ do
        Just (GClientReference currentClientIdx) <- use $ gameBaseGlobals.gbCurrentClient
        Just bobTime <- preuse $ gameBaseGlobals.gbGame.glClients.ix currentClientIdx.gcBobTime
        bobMove <- use $ gameBaseGlobals.gbBobMove
        bobCycle <- use $ gameBaseGlobals.gbBobCycle
        when (truncate (bobTime + bobMove) /= bobCycle) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEvent .= Constants.evFootstep

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
