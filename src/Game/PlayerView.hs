module Game.PlayerView
    ( clientEndServerFrame
    ) where

import           Control.Lens           (use, ix, (^.), (.=), (%=), (&), (.~), (+~), (-~), (%~))
import           Control.Monad          (when, unless)
import           Data.Bits              (complement, xor, (.&.), (.|.))
import qualified Data.ByteString        as B
import           Data.Maybe             (isJust, isNothing)
import           Linear                 (V3(..), dot, normalize, _x, _y, _z)

import qualified Constants
import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameCombat        as GameCombat
import qualified Game.GameItems         as GameItems
import           Game.GameLocalsT
import           Game.GClientT
import           Game.GItemT
import           Game.LevelLocalsT
import qualified Game.Monsters.MPlayer  as MPlayer
import qualified Game.PlayerHud         as PlayerHud
import           Game.PlayerStateT
import qualified Game.PlayerWeapon      as PlayerWeapon
import           Game.PMoveStateT
import qualified QCommon.Com            as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary            (encode)
import qualified Util.Lib               as Lib
import qualified Util.Math3D            as Math3D

clientEndServerFrame :: Ref EdictT -> Quake ()
clientEndServerFrame edictRef = do
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
        edict <- readRef edictRef
        gClientRef <- getGClientRef (edict^.eClient)
        gClient <- readRef gClientRef
        modifyRef gClientRef (\v -> v & gcOldVelocity   .~ (edict^.eVelocity)
                                      & gcOldViewAngles .~ (gClient^.gcPlayerState.psViewAngles)
                                      & gcKickOrigin    .~ V3 0 0 0
                                      & gcKickAngles    .~ V3 0 0 0)
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        -- if the scoreboard is up, update it
        when ((gClient^.gcShowScores) && frameNum .&. 31 == 0) $ do
            PlayerHud.deathmatchScoreboardMessage edictRef (edict^.eEnemy)
            unicast <- use (gameBaseGlobals.gbGameImport.giUnicast)
            unicast edictRef False
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.clientEndServerFrame edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    setCurrentPlayerAndClient = do
        edict <- readRef edictRef
        gameBaseGlobals.gbCurrentPlayer .= Just edictRef
        gameBaseGlobals.gbCurrentClient .= edict^.eClient
    updatePMoveValues = do
        edict <- readRef edictRef
        gClientRef <- getCurrentClientRef =<< use (gameBaseGlobals.gbCurrentClient)
        modifyRef gClientRef (\v -> v & gcPlayerState.psPMoveState.pmsOrigin .~ fmap (truncate . (* 8.0)) (edict^.eEntityState.esOrigin)
                                      & gcPlayerState.psPMoveState.pmsVelocity .~ fmap (truncate . (* 8.0)) (edict^.eVelocity))
    getCurrentClientRef Nothing = do
        Com.fatalError "PlayerView.clientEndServerFrame current client is Nothing"
        return (Ref (-1))
    getCurrentClientRef (Just gClientRef) = return gClientRef
    checkIntermissionTime = do
        intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
        if intermissionTime /= 0
            then do
                -- FIXME: add view drifting here?
                gClientRef <- getCurrentClientRef =<< use (gameBaseGlobals.gbCurrentClient)
                modifyRef gClientRef (\v -> v & gcPlayerState.psBlend._z .~ 0
                                              & gcPlayerState.psFOV .~ 90)
                PlayerHud.setStats edictRef
                return True
            else
                return False
    setupAngleVectors = do
        gClientRef <- getCurrentClientRef =<< use (gameBaseGlobals.gbCurrentClient)
        gClient <- readRef gClientRef
        let (forward, right, up) = Math3D.angleVectors (gClient^.gcVAngle) True True True
        gameBaseGlobals.gbForward .= forward
        gameBaseGlobals.gbRight .= right
        gameBaseGlobals.gbUp .= up
    setModelAngles = do
        gClientRef <- getCurrentClientRef =<< use (gameBaseGlobals.gbCurrentClient)
        gClient <- readRef gClientRef
        if (gClient^.gcVAngle._x) > 180
            then modifyRef edictRef (\v -> v & eEntityState.esAngles._x .~ ((-360) + (gClient^.gcVAngle._x)) / 3)
            else modifyRef edictRef (\v -> v & eEntityState.esAngles._x .~ (gClient^.gcVAngle._x) / 3)
        modifyRef edictRef (\v -> v & eEntityState.esAngles._y .~ (gClient^.gcVAngle._y)
                                    & eEntityState.esAngles._z .~ 0)
        edict <- readRef edictRef
        roll <- calcRoll (edict^.eEntityState.esAngles) (edict^.eVelocity)
        modifyRef edictRef (\v -> v & eEntityState.esAngles._z .~ roll * 4)
    calculateSpeedAndCycle = do
        edict <- readRef edictRef
        gClientRef <- getCurrentClientRef =<< use (gameBaseGlobals.gbCurrentClient)
        let velocity = edict^.eVelocity
            xyspeed = sqrt ((velocity^._x) * (velocity^._x) + (velocity^._y) * (velocity^._y))
        updateBobMove gClientRef edict xyspeed
        bobMove <- use (gameBaseGlobals.gbBobMove)
        modifyRef gClientRef (\v -> v & gcBobTime +~ bobMove)
        gClient <- readRef gClientRef
        let bobTime = gClient^.gcBobTime
            bobTime' = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0
                           then bobTime * 4
                           else bobTime
        gameBaseGlobals.gbBobCycle .= truncate bobTime'
        gameBaseGlobals.gbBobFracSin .= abs (sin (bobTime' * pi))
    updateBobMove gClientRef edict xyspeed
        | xyspeed < 5 = do
            gameBaseGlobals.gbBobMove .= 0
            modifyRef gClientRef (\v -> v & gcBobTime .~ 0) -- start at beginning of cycle again
        | isJust (edict^.eGroundEntity) =
            gameBaseGlobals.gbBobMove .= calcBobMove xyspeed
        | otherwise = return ()
    calcBobMove xyspeed
        | xyspeed > 210 = 0.25
        | xyspeed > 100 = 0.125
        | otherwise     = 0.0625
    setCameraStuff = do
        edict <- readRef edictRef
        gClientRef <- getGClientRef (edict^.eClient)
        gClient <- readRef gClientRef
        if gClient^.gcResp.crSpectator
            then PlayerHud.setSpectatorStats edictRef
            else PlayerHud.setStats edictRef

worldEffects :: Quake ()
worldEffects = do
    Just edictRef <- use $ gameBaseGlobals.gbCurrentPlayer
    edict <- readRef edictRef
    doWorldEffects edictRef edict

doWorldEffects :: Ref EdictT -> EdictT -> Quake ()
doWorldEffects edictRef initialEdict
    | (initialEdict^.eMoveType) == Constants.moveTypeNoClip = do
        -- don't need air
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef edictRef (\v -> v & eAirFinished .~ levelTime + 12)
    | otherwise = do
        gClientRef <- getCurrentClientRef =<< use (gameBaseGlobals.gbCurrentClient)
        gClient <- readRef gClientRef
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        let waterLevel = initialEdict^.eWaterLevel
            oldWaterLevel = gClient^.gcOldWaterLevel
            breather = truncate (gClient^.gcBreatherFrameNum) > frameNum
            enviroSuit = truncate (gClient^.gcEnviroFrameNum) > frameNum
        modifyRef gClientRef (\v -> v & gcOldWaterLevel .~ waterLevel)
        -- if just entered a water volume, play a sound
        waterEnterPlaySound waterLevel oldWaterLevel
        -- if just completely exited a water volume, play a sound
        waterExitPlaySound waterLevel oldWaterLevel
        -- check for head just going under water
        checkHeadUnderWater waterLevel oldWaterLevel
        -- check for head just coming out of water
        checkHeadOutOfWater waterLevel oldWaterLevel
        -- check for drowning
        checkForDrowning gClientRef waterLevel breather enviroSuit
        -- check for sizzle damage
        checkForSizzleDamage gClientRef waterLevel enviroSuit
  where
    getCurrentClientRef Nothing = do
        Com.fatalError "PlayerView.doWorldEffects current client is Nothing"
        return (Ref (-1))
    getCurrentClientRef (Just gClientRef) = return gClientRef
    waterEnterPlaySound waterLevel oldWaterLevel =
        when (oldWaterLevel == 0 && waterLevel /= 0) $ do
            edict <- readRef edictRef
            gameImport <- use (gameBaseGlobals.gbGameImport)
            PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf
            soundIdx <- getSoundIndex gameImport edict
            maybe (return ()) (playSound gameImport) soundIdx
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef edictRef (\v -> v & eFlags %~ (.|. Constants.flInWater)
                                        -- clear damage_debounce, so the pain sound will play immediately
                                        & eDamageDebounceTime .~ levelTime - 1)
    getSoundIndex gameImport edict
        | (edict^.eWaterType) .&. Constants.contentsLava /= 0 =
            (gameImport^.giSoundIndex) (Just "player/lava_in.wav") >>= return . Just
        | (edict^.eWaterType) .&. Constants.contentsSlime /= 0 =
            (gameImport^.giSoundIndex) (Just "player/watr_in.wav") >>= return . Just
        | (edict^.eWaterType) .&. Constants.contentsWater /= 0 =
            (gameImport^.giSoundIndex) (Just "player/watr_in.wav") >>= return . Just
        | otherwise = return Nothing
    playSound gameImport soundIdx =
        (gameImport^.giSound) (Just edictRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0
    waterExitPlaySound waterLevel oldWaterLevel =
        when (oldWaterLevel /= 0 && waterLevel == 0) $ do
            edict <- readRef edictRef
            gameImport <- use (gameBaseGlobals.gbGameImport)
            PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf
            soundIdx <- (gameImport^.giSoundIndex) (Just "player/watr_out.wav")
            (gameImport^.giSound) (Just edictRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0
            modifyRef edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flInWater)))
    checkHeadUnderWater waterLevel oldWaterLevel =
        when (oldWaterLevel /= 3 && waterLevel == 3) $ do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "player/watr_un.wav")
            (gameImport^.giSound) (Just edictRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0
    checkHeadOutOfWater waterLevel oldWaterLevel =
        when (oldWaterLevel == 3 && waterLevel /= 3) $ do
            edict <- readRef edictRef
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            gameImport <- use (gameBaseGlobals.gbGameImport)
            doCheckHeadOutOfWater edict levelTime gameImport
    doCheckHeadOutOfWater edict levelTime gameImport
        | (edict^.eAirFinished) < levelTime = do -- gasp for air
            soundIdx <- (gameImport^.giSoundIndex) (Just "player/gasp1.wav")
            (gameImport^.giSound) (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf
        | (edict^.eAirFinished) < levelTime + 11 = do -- just break surface
            soundIdx <- (gameImport^.giSoundIndex) (Just "player/gasp2.wav")
            (gameImport^.giSound) (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
        | otherwise = return ()
    checkForDrowning gClientRef waterLevel breather enviroSuit
        | waterLevel == 3 = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            gameImport <- use (gameBaseGlobals.gbGameImport)
            -- breather or envirosuit give air
            when (breather || enviroSuit) $ do
                modifyRef edictRef (\v -> v & eAirFinished .~ levelTime + 10)
                gClient <- readRef gClientRef
                edict <- readRef edictRef
                frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
                when ((truncate (gClient^.gcBreatherFrameNum) - frameNum) `mod` 25 == 0) $ do
                  soundIdx <- if (gClient^.gcBreatherSound) == 0
                                  then (gameImport^.giSoundIndex) (Just "player/u_breath1.wav")
                                  else (gameImport^.giSoundIndex) (Just "player/u_breath2.wav")
                  (gameImport^.giSound) (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0
                  modifyRef gClientRef (\v -> v & gcBreatherSound %~ (`xor` 1))
                  PlayerWeapon.playerNoise edictRef (edict^.eEntityState.esOrigin) Constants.pNoiseSelf
                  -- FIXME: release a bubble?
            -- if out of air, start drowning
            edict <- readRef edictRef
            when ((edict^.eAirFinished) < levelTime) $ do -- drown!
                clientRef <- getGClientRef (edict^.eClient)
                client <- readRef clientRef
                when ((client^.gcNextDrownTime) < levelTime && (edict^.eHealth) > 0) $ do
                    modifyRef clientRef (\v -> v & gcNextDrownTime .~ levelTime + 1)
                    -- take more damage the longer underwater
                    modifyRef edictRef (\v -> v & eDmg %~ \vv -> (min (vv + 2) 15))
                    -- play a gurp sound instead of a normal pain sound
                    playDrowningSound
                    modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime)
                    v3o <- use (globals.gVec3Origin)
                    updatedEdict <- readRef edictRef
                    GameCombat.damage edictRef
                                      worldRef
                                      worldRef
                                      v3o
                                      (updatedEdict^.eEntityState.esOrigin)
                                      v3o
                                      (updatedEdict^.eDmg)
                                      0
                                      Constants.damageNoArmor
                                      Constants.modWater
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef edictRef (\v -> v & eAirFinished .~ levelTime + 12
                                        & eDmg .~ 2)
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.doWorldEffects#checkForDrowning edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    playDrowningSound = do
        edict <- readRef edictRef
        r <- Lib.rand
        gameImport <- use (gameBaseGlobals.gbGameImport)
        soundIdx <- getSoundIndex2 edict r gameImport
        (gameImport^.giSound) (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
    getSoundIndex2 edict r gameImport
        | (edict^.eHealth) <= (edict^.eDmg) =
            (gameImport^.giSoundIndex) (Just "player/drown1.wav")
        | r .&. 1 /= 0 =
            (gameImport^.giSoundIndex) (Just "*gurp1.wav")
        | otherwise =
            (gameImport^.giSoundIndex) (Just "*gurp2.wav")
    checkForSizzleDamage gClientRef waterLevel enviroSuit = do
        edict <- readRef edictRef
        when (waterLevel /= 0 && (edict^.eWaterType) .&. (Constants.contentsLava .|. Constants.contentsSlime) /= 0) $ do
            when ((edict^.eWaterType) .&. Constants.contentsLava /= 0) $ do
                gClient <- readRef gClientRef
                levelTime <- use (gameBaseGlobals.gbLevel.llTime)
                frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
                when ((edict^.eHealth) > 0 && (edict^.ePainDebounceTime) <= levelTime && truncate (gClient^.gcInvincibleFrameNum) < frameNum) $ do
                    r <- Lib.rand
                    gameImport <- use (gameBaseGlobals.gbGameImport)
                    soundIdx <- if r .&. 1 /= 0
                                    then (gameImport^.giSoundIndex) (Just "player/burn1.wav")
                                    else (gameImport^.giSoundIndex) (Just "player/burn2.wav")
                    (gameImport^.giSound) (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
                    modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)
                v3o <- use (globals.gVec3Origin)
                GameCombat.damage edictRef
                                  worldRef
                                  worldRef
                                  v3o
                                  (edict^.eEntityState.esOrigin)
                                  v3o
                                  (if enviroSuit then waterLevel else 3 * waterLevel)
                                  0
                                  0
                                  Constants.modLava
            when ((edict^.eWaterType) .&. Constants.contentsSlime /= 0) $
                unless enviroSuit $ do -- no damage from slime with envirosuit
                    v3o <- use (globals.gVec3Origin)
                    GameCombat.damage edictRef
                                      worldRef
                                      worldRef
                                      v3o
                                      (edict^.eEntityState.esOrigin)
                                      v3o
                                      waterLevel
                                      0
                                      0
                                      Constants.modSlime

fallingDamage :: Ref EdictT -> Quake ()
fallingDamage edictRef = do
    edict <- readRef edictRef
    when ((edict^.eEntityState.esModelIndex) == 255 && (edict^.eMoveType) /= Constants.moveTypeNoClip) $ do
        gClientRef <- getGClientRef (edict^.eClient)
        gClient <- readRef gClientRef
        let delta = fmap (\d -> d * d * 0.0001) (calcDelta edict gClient)
            delta' = fmap (\d -> checkWaterLevel (edict^.eWaterLevel) d) delta
        maybe (return ()) (doFallingDamage edict gClientRef) delta'
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.fallingDamage edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    calcDelta edict gClient
        | gClient^.gcOldVelocity._z < 0 && (edict^.eVelocity._z) > (gClient^.gcOldVelocity._z) && isNothing (edict^.eGroundEntity) =
            Just (gClient^.gcOldVelocity._z)
        | isNothing (edict^.eGroundEntity) = Nothing
        | otherwise = Just ((edict^.eVelocity._z) - (gClient^.gcOldVelocity._z))
    checkWaterLevel waterLevel delta 
        | waterLevel == 3 = delta
        | waterLevel == 2 = delta * 0.25
        | waterLevel == 1 = delta * 0.5
        | otherwise = delta
    doFallingDamage edict gClientRef delta
        | (edict^.eWaterLevel) == 3 = return () -- never take falling damage if completely underwater
        | delta < 1 = return ()
        | delta < 15 = modifyRef edictRef (\v -> v & eEntityState.esEvent .~ Constants.evFootstep)
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef gClientRef (\v -> v & gcFallValue .~ min (delta * 0.5) 40
                                          & gcFallTime .~ levelTime + Constants.fallTime)
            proceedFallingDamage edict delta levelTime
    proceedFallingDamage edict delta levelTime
        | delta > 30 = do
            when ((edict^.eHealth) > 0) $
                modifyRef edictRef (\v -> v & eEntityState.esEvent .~ (if delta > 55 then Constants.evFallFar else Constants.evFall))
            modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime)
            deathmatch <- fmap (^.cvValue) deathmatchCVar
            dmFlags <- fmap (truncate . (^.cvValue)) dmFlagsCVar
            when (deathmatch == 0 || dmFlags .&. Constants.dfNoFalling == 0) $ do
                let damage = max 1 (truncate ((delta - 30) / 2) :: Int)
                v3o <- use (globals.gVec3Origin)
                GameCombat.damage edictRef
                                  worldRef
                                  worldRef
                                  (V3 0 0 1)
                                  (edict^.eEntityState.esOrigin)
                                  v3o
                                  damage
                                  0
                                  0
                                  Constants.modFalling
        | otherwise =
            modifyRef edictRef (\v -> v & eEntityState.esEvent .~ Constants.evFallShort)

damageFeedback :: Ref EdictT -> Quake ()
damageFeedback playerRef = do
    player <- readRef playerRef
    gClientRef <- getGClientRef (player^.eClient)
    gClient <- readRef gClientRef
    frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    -- flash the backgrounds behind the status numbers
    modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statFlashes .~ 0)
    when ((gClient^.gcDamageBlood) /= 0) $
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statFlashes %~ (.|. 1))
    when ((gClient^.gcDamageArmor) /= 0 && (player^.eFlags) .&. Constants.flGodMode == 0 && truncate (gClient^.gcInvincibleFrameNum) <= frameNum) $
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statFlashes %~ (.|. 2))
    -- total points of damage shot at the player this frame
    let realCount = (gClient^.gcDamageBlood) + (gClient^.gcDamageArmor) + (gClient^.gcDamagePArmor)
    unless (realCount == 0) $ do -- unless (didn't take any damage)
        -- start a pain animation if still in the player model
        when ((gClient^.gcAnimPriority) < Constants.animPain && (player^.eEntityState.esModelIndex) == 255) $ do
            startPainAnimation gClientRef gClient
        let count = max realCount 10 -- always make a visible effect
        -- play an apropriate pain sound
        when (levelTime > (player^.ePainDebounceTime) && (player^.eFlags) .&. Constants.flGodMode == 0 && truncate (gClient^.gcInvincibleFrameNum) <= frameNum) $ do
            playPainSound player levelTime
        -- the total alpha of the blend is always proportional to count
        when ((gClient^.gcDamageAlpha) < 0) $
            modifyRef gClientRef (\v -> v & gcDamageAlpha .~ 0)
        modifyRef gClientRef (\v -> v & gcDamageAlpha +~ fromIntegral count * 0.01)
        modifyRef gClientRef (\v -> v & gcDamageAlpha %~ (\vv -> min 0.6 (max vv 0.2)))
        -- the color of the blend will vary based on how much was absorbed by
        -- different armors
        modifyRef gClientRef (\v -> v & gcDamageBlend .~ calcDamageBlend gClient realCount)
        -- calculate view angle kicks
        let kick = abs (gClient^.gcDamageKnockback)
        when (kick /= 0 && (player^.eHealth) > 0) $ do -- kick of 0 means no view adjust at all
            right <- use (gameBaseGlobals.gbRight)
            forward <- use (gameBaseGlobals.gbForward)
            let kick' = (fromIntegral kick) * 100 / (fromIntegral (player^.eHealth)) :: Float
                kick'' = if kick' < fromIntegral count * 0.5
                             then fromIntegral count * 0.5
                             else kick'
                kick''' = min 50 kick''
                vv = (gClient^.gcDamageFrom) - (player^.eEntityState.esOrigin)
                vv' = normalize vv
                side = vv' `dot` right
                side' = negate (vv' `dot` forward)
            modifyRef gClientRef (\v -> v & gcVDmgRoll .~ kick''' * side * 0.3
                                          & gcVDmgPitch .~ kick''' * side' * 0.3
                                          & gcVDmgTime .~ levelTime + Constants.damageTime)
        -- clear totals
        modifyRef gClientRef (\v -> v & gcDamageBlood     .~ 0
                                      & gcDamageArmor     .~ 0
                                      & gcDamagePArmor    .~ 0
                                      & gcDamageKnockback .~ 0)
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.damageFeedback player^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    startPainAnimation gClientRef gClient
        | (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0 = do
            modifyRef playerRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRPain1 - 1)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameCRPain4)
        | otherwise = do
            gameBaseGlobals.gbXxxi %= (\x -> (x + 1) `mod` 3)
            xxxi <- use (gameBaseGlobals.gbXxxi)
            case xxxi of
              0 -> do
                modifyRef playerRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain101 - 1)
                modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.framePain104)
              1 -> do
                modifyRef playerRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain201 - 1)
                modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.framePain204)
              2 -> do
                modifyRef playerRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain301 - 1)
                modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.framePain304)
              _ -> return ()
    playPainSound player levelTime = do
        r <- fmap (\v -> 1 + (v .&. 1)) Lib.rand
        modifyRef playerRef (\v -> v & ePainDebounceTime .~ levelTime + 0.7)
        let l | (player^.eHealth) < 25 = 25
              | (player^.eHealth) < 50 = 50
              | (player^.eHealth) < 75 = 75
              | otherwise              = 100 :: Int
        gameImport <- use (gameBaseGlobals.gbGameImport)
        soundIdx <- (gameImport^.giSoundIndex) (Just (B.concat ["*pain", encode l, "_", encode r, ".wav"]))
        (gameImport^.giSound) (Just playerRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
    calcDamageBlend gClient realCount =
        let realCountF = fromIntegral realCount :: Float
            a = V3 0 0 0 :: V3 Float
            b | (gClient^.gcDamagePArmor) /= 0 = a + fmap (* (fromIntegral (gClient^.gcDamagePArmor) / realCountF)) (V3 0 1 0)
              | otherwise = a
            c | (gClient^.gcDamageArmor) /= 0 = b + fmap (* (fromIntegral (gClient^.gcDamageArmor) / realCountF)) (V3 1 1 1)
              | otherwise = b
            d | (gClient^.gcDamageBlood) /= 0 = c + fmap (* (fromIntegral (gClient^.gcDamageBlood) / realCountF)) (V3 1 0 0)
              | otherwise = c
        in d

-- TODO: compare this to quake2 original
calcViewOffset :: Ref EdictT -> Quake ()
calcViewOffset edictRef = do
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    gClient <- readRef gClientRef
    -- if dead, fix the angle and don't add any kick
    setAngles edict gClientRef gClient
    bobFracSin <- use (gameBaseGlobals.gbBobFracSin)
    xyspeed <- use (gameBaseGlobals.gbXYSpeed)
    bobUpValue <- fmap (^.cvValue) bobUpCVar
    let bob = min 6 (bobFracSin * xyspeed * bobUpValue)
        V3 a b c = (V3 0 0 (fromIntegral (edict^.eViewHeight) + bob)) + (gClient^.gcKickOrigin)
        a' = min 14 (max a (-14))
        b' = min 14 (max b (-14))
        c' = min 30 (max c (-22))
    modifyRef gClientRef (\v -> v & gcPlayerState.psViewOffset .~ V3 a' b' c')
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.calcViewOffset edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    setAngles edict gClientRef gClient
        | (edict^.eDeadFlag) /= 0 =
            modifyRef gClientRef (\v -> v & gcPlayerState.psKickAngles .~ V3 0 0 0
                                          & gcPlayerState.psViewAngles .~ V3 (-15) (gClient^.gcKillerYaw) 40)
        | otherwise = do
            -- add angles based on weapon kick
            let angles = gClient^.gcKickAngles
            -- add angles based on damage kick
            angles2 <- addDamageKickAngles gClientRef angles
            -- add angles based on fall kick
            angles3 <- addFallKickAngles gClientRef angles2
            -- add angles based on velocity
            angles4 <- addVelocityAngles angles3
            -- add angles based on bob
            angles5 <- addBobAngles gClientRef angles4
            modifyRef gClientRef (\v -> v & gcPlayerState.psKickAngles .~ angles5)
    addDamageKickAngles gClientRef angles = do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        gClient <- readRef gClientRef
        ratio <- updateRatio gClientRef (((gClient^.gcVDmgTime) - levelTime) / Constants.damageTime)
        updatedGClient <- readRef gClientRef
        let angles' = angles & _x %~ (+ (ratio * (updatedGClient^.gcVDmgPitch)))
            angles'' = angles' & _z %~ (+ (ratio * (updatedGClient^.gcVDmgRoll)))
        return angles''
    updateRatio gClientRef ratio
        | ratio < 0 = do
            modifyRef gClientRef (\v -> v & gcVDmgPitch .~ 0
                                          & gcVDmgRoll .~ 0)
            return 0
        | otherwise = return ratio
    addFallKickAngles gClientRef angles = do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        gClient <- readRef gClientRef
        let ratio = max 0 (((gClient^.gcFallTime) - levelTime) / Constants.fallTime)
            angles' = angles & _x %~ (+ (ratio * (gClient^.gcFallValue)))
        return angles'
    addVelocityAngles angles = do
        edict <- readRef edictRef
        runPitch <- fmap (^.cvValue) runPitchCVar
        runRoll <- fmap (^.cvValue) runRollCVar
        forward <- use (gameBaseGlobals.gbForward)
        right <- use (gameBaseGlobals.gbRight)
        let delta = (edict^.eVelocity) `dot` forward
            delta' = (edict^.eVelocity) `dot` right
            angles' = angles & _x %~ (+ (delta * runPitch))
            angles'' = angles' & _z %~ (+ (delta' * runRoll))
        return angles''
    addBobAngles gClientRef angles = do
        bobFracSin <- use (gameBaseGlobals.gbBobFracSin)
        bobCycle <- use (gameBaseGlobals.gbBobCycle)
        xyspeed <- use (gameBaseGlobals.gbXYSpeed)
        bobPitch <- fmap (^.cvValue) bobPitchCVar
        bobRoll <- fmap (^.cvValue) bobRollCVar
        gClient <- readRef gClientRef
        let delta = bobFracSin * bobPitch * xyspeed
            delta' = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0
                         then delta * 6 -- crouching
                         else delta
            angles' = angles & _x %~ (+ delta')
            delta'' = bobFracSin * bobRoll * xyspeed
            delta''' = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0
                           then delta'' * 6 -- crouching
                           else delta''
            delta'''' = if bobCycle .&. 1 /= 0 then negate delta''' else delta'''
            angles'' = angles' & _z %~ (+ delta'''')
        return angles''

calcGunOffset :: Ref EdictT -> Quake ()
calcGunOffset edictRef = do
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    xyspeed <- use (gameBaseGlobals.gbXYSpeed)
    bobFracSin <- use (gameBaseGlobals.gbBobFracSin)
    bobCycle <- use (gameBaseGlobals.gbBobCycle)
    modifyRef gClientRef (\v -> v & gcPlayerState.psGunAngles._z .~ xyspeed * bobFracSin * 0.005
                                  & gcPlayerState.psGunAngles._y .~ xyspeed * bobFracSin * 0.01)
    when (bobCycle .&. 1 /= 0) $
        modifyRef gClientRef (\v -> v & gcPlayerState.psGunAngles._z %~ negate
                                      & gcPlayerState.psGunAngles._y %~ negate)
    modifyRef gClientRef (\v -> v & gcPlayerState.psGunAngles._x .~ xyspeed * bobFracSin * 0.005)
    -- gun angles from delta movement
    gClient <- readRef gClientRef
    setGunAngles gClientRef gClient 0 3
    forward <- use (gameBaseGlobals.gbForward)
    right <- use (gameBaseGlobals.gbRight)
    up <- use (gameBaseGlobals.gbUp)
    gunX <- fmap (^.cvValue) gunXCVar
    gunY <- fmap (^.cvValue) gunYCVar
    gunZ <- fmap (^.cvValue) gunZCVar
    let offset = getGunOffset forward right up (V3 0 0 0) gunX gunY gunZ 0 3
    modifyRef gClientRef (\v -> v & gcPlayerState.psGunOffset .~ offset)
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.calcGunOffset edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    setGunAngles gClientRef gClient idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            let delta = (gClient^.gcOldViewAngles.(Math3D.v3Access idx)) - (gClient^.gcPlayerState.psViewAngles.(Math3D.v3Access idx))
                delta' = if delta > 180 then delta - 360 else delta
                delta'' = if delta' < -180 then delta' + 360 else delta'
                delta''' = if delta'' > 45 then 45 else delta''
                delta'''' = if delta''' < -45 then -45 else delta'''
            when (idx == Constants.yaw) $
                modifyRef gClientRef (\v -> v & gcPlayerState.psGunAngles._z +~ 0.1 * delta'''')
            modifyRef gClientRef (\v -> v & gcPlayerState.psGunAngles.(if idx == 0 then _x else (if idx == 1 then _y else _z)) +~ 0.2 * delta'''')
            setGunAngles gClientRef gClient (idx + 1) maxIdx
    getGunOffset forward right up acc gunX gunY gunZ idx maxIdx
        | idx >= maxIdx = acc
        | otherwise =
            let a = (forward^.(Math3D.v3Access idx)) * gunY
                b = (right^.(Math3D.v3Access idx)) * gunX
                c = (up^.(Math3D.v3Access idx)) * (negate gunZ)
                V3 a' b' c' = acc
            in getGunOffset forward right up (V3 (a + a') (b + b') (c + c')) gunX gunY gunZ (idx + 1) maxIdx

calcBlend :: Ref EdictT -> Quake ()
calcBlend edictRef = io (putStrLn "PlayerView.calcBlend IMPLEMENT ME!") -- TODO

setClientEvent :: Ref EdictT -> Quake ()
setClientEvent edictRef = do
    edict <- readRef edictRef
    unless ((edict^.eEntityState.esEvent) /= 0) $ do
        xyspeed <- use (gameBaseGlobals.gbXYSpeed)
        when (isJust (edict^.eGroundEntity) && xyspeed > 225) $ do
            currentClientRef <- getCurrentClientRef =<< use (gameBaseGlobals.gbCurrentClient)
            bobTime <- fmap (^.gcBobTime) (readRef currentClientRef)
            bobMove <- use (gameBaseGlobals.gbBobMove)
            bobCycle <- use (gameBaseGlobals.gbBobCycle)
            when (truncate (bobTime + bobMove) /= bobCycle) $
                modifyRef edictRef (\v -> v & eEntityState.esEvent .~ Constants.evFootstep)
  where
    getCurrentClientRef Nothing = do
        Com.fatalError "PlayerView.setClientEvent current client is Nothing"
        return (Ref (-1))
    getCurrentClientRef (Just clientRef) = return clientRef

setClientEffects :: Ref EdictT -> Quake ()
setClientEffects edictRef = do
    modifyRef edictRef (\v -> v & eEntityState.esEffects .~ 0
                                & eEntityState.esRenderFx .~ 0)
    edict <- readRef edictRef
    intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
    unless ((edict^.eHealth) <= 0 || intermissionTime /= 0) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        when ((edict^.ePowerArmorTime) > levelTime) $ do
            paType <- GameItems.powerArmorType edictRef
            updateEffects paType
        gClientRef <- getGClientRef (edict^.eClient)
        gClient <- readRef gClientRef
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        when (truncate (gClient^.gcQuadFrameNum) > frameNum) $ do
            let remaining = truncate (gClient^.gcQuadFrameNum) - frameNum
            when (remaining > 30 || (remaining .&. 4) /= 0) $
                modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efQuad))
        when (truncate (gClient^.gcInvincibleFrameNum) > frameNum) $ do
            let remaining = truncate (gClient^.gcInvincibleFrameNum) - frameNum
            when (remaining > 30 || (remaining .&. 4) /= 0) $
                modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efPent))
        -- show cheaters!!!
        when ((edict^.eFlags) .&. Constants.flGodMode /= 0) $ do
            modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efColorShell)
                                      & eEntityState.esRenderFx %~ (.|. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue)))
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.setClientEffects edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    updateEffects paType
        | paType == Constants.powerArmorScreen =
            modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efPowerScreen))
        | paType == Constants.powerArmorShield =
            modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efColorShell)
                                        & eEntityState.esRenderFx %~ (.|. Constants.rfShellGreen))
        | otherwise = return ()

setClientSound :: Ref EdictT -> Quake ()
setClientSound edictRef = do
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    checkHelpChanged gClientRef
    -- help beep (no more than three times)
    helpBeep gClientRef
    setSound gClientRef
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.setClientSound edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    checkHelpChanged gClientRef = do
        gClient <- readRef gClientRef
        helpChanged <- use (gameBaseGlobals.gbGame.glHelpChanged)
        when ((gClient^.gcPers.cpGameHelpChanged) /= helpChanged) $ do
            modifyRef gClientRef (\v -> v & gcPers.cpGameHelpChanged .~ helpChanged
                                          & gcPers.cpHelpChanged .~ 1)
    helpBeep gClientRef = do
        gClient <- readRef gClientRef
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        when ((gClient^.gcPers.cpHelpChanged) /= 0 && (gClient^.gcPers.cpHelpChanged) <= 3 && (frameNum .&. 63) == 0) $ do
            modifyRef gClientRef (\v -> v & gcPers.cpHelpChanged +~ 1)
            gameImport <- use (gameBaseGlobals.gbGameImport)
            soundIdx <- (gameImport^.giSoundIndex) (Just "misc/pc_up.wav")
            (gameImport^.giSound) (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnStatic 0
    setSound gClientRef = do
        gClient <- readRef gClientRef
        weapon <- getWeapon (gClient^.gcPers.cpWeapon)
        edict <- readRef edictRef
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        soundIdx <- getSoundIndex edict gClient weapon soundIndex
        modifyRef edictRef (\v -> v & eEntityState.esSound .~ soundIdx)
    getWeapon Nothing = return ""
    getWeapon (Just weaponRef) = do
        weapon <- readRef weaponRef
        return (weapon^.giClassName)
    getSoundIndex :: EdictT -> GClientT -> B.ByteString -> (Maybe B.ByteString -> Quake Int) -> Quake Int
    getSoundIndex edict gClient weapon soundIndex
        | (edict^.eWaterLevel) /= 0 && (edict^.eWaterType) .&. (Constants.contentsLava .|. Constants.contentsSlime) /= 0 =
            use (gameBaseGlobals.gbSndFry)
        | weapon == "weapon_railgun" = soundIndex (Just "weapons/rg_hum.wav")
        | weapon == "weapon_bfg" = soundIndex (Just "weapons/bfg_hum.wav")
        | (gClient^.gcWeaponSound) /= 0 = return (gClient^.gcWeaponSound)
        | otherwise = return 0

setClientFrame :: Ref EdictT -> Quake ()
setClientFrame edictRef = do
    edict <- readRef edictRef
    -- only when we are in the player model
    when ((edict^.eEntityState.esModelIndex) == 255) $ do
        gClientRef <- getGClientRef (edict^.eClient)
        gClient <- readRef gClientRef
        xyspeed <- use (gameBaseGlobals.gbXYSpeed)
        let duck = (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfDucked /= 0
            run = xyspeed /= 0
            skip = checkSkip edict gClient duck run
        done <- if skip then return False else checkIfDone edict gClientRef gClient
        unless done $ do
            -- return to either a running or standing frame
            modifyRef gClientRef (\v -> v & gcAnimPriority .~ Constants.animBasic
                                          & gcAnimDuck     .~ duck
                                          & gcAnimRun      .~ run)
            doSetClientFrame gClientRef edict run duck
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerView.setClientFrame edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    doSetClientFrame gClientRef edict run duck
        | isNothing (edict^.eGroundEntity) = do
            modifyRef gClientRef (\v -> v & gcAnimPriority .~ Constants.animJump
                                          & gcAnimEnd      .~ MPlayer.frameJump2)
            when ((edict^.eEntityState.esFrame) /= MPlayer.frameJump2) $
                modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameJump1)
        | run && duck = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRWalk1)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameCRWalk6)
        | run = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameRun1)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameRun6)
        | duck = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRStnd01)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameCRStnd19)
        | otherwise = do
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameStand01)
            modifyRef gClientRef (\v -> v & gcAnimEnd .~ MPlayer.frameStand40)
    checkSkip edict gClient duck run =
        let a = duck /= (gClient^.gcAnimDuck) && (gClient^.gcAnimPriority) < Constants.animDeath
            b = run /= (gClient^.gcAnimRun) && (gClient^.gcAnimPriority) == Constants.animBasic
            c = isNothing (edict^.eGroundEntity) && (gClient^.gcAnimPriority) <= Constants.animWave
        in a || b || c
    checkIfDone edict gClientRef gClient
        | (gClient^.gcAnimPriority) == Constants.animReverse =
            if (edict^.eEntityState.esFrame) > (gClient^.gcAnimEnd)
                then do
                    modifyRef edictRef (\v -> v & eEntityState.esFrame -~ 1)
                    return True
                else
                    checkDeath edict gClientRef gClient
        | otherwise =
            if (edict^.eEntityState.esFrame) < (gClient^.gcAnimEnd) -- continue an animation
                then do
                    modifyRef edictRef (\v -> v & eEntityState.esFrame +~ 1)
                    return True
                else
                    checkDeath edict gClientRef gClient
    checkDeath edict gClientRef gClient
        | (gClient^.gcAnimPriority) == Constants.animDeath = return True -- stay there
        | otherwise = checkJump edict gClientRef gClient
    checkJump edict gClientRef gClient
        | (gClient^.gcAnimPriority) == Constants.animJump && isNothing (edict^.eGroundEntity) = return True -- stay there
        | (gClient^.gcAnimPriority) == Constants.animJump = do
            modifyRef gClientRef (\v -> v & gcAnimPriority .~ Constants.animWave
                                          & gcAnimEnd .~ MPlayer.frameJump6)
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameJump3)
            return True
        | otherwise = return False

calcRoll :: V3 Float -> V3 Float -> Quake Float
calcRoll angles velocity = do
    right <- use (gameBaseGlobals.gbRight)
    let side = velocity `dot` right
        sign = if side < 0 then -1 else 1
        side' = abs side
    rollAngleValue <- fmap (^.cvValue) svRollAngleCVar
    rollSpeedValue <- fmap (^.cvValue) svRollSpeedCVar
    let side'' = if side' < rollSpeedValue
                     then side' * rollAngleValue / rollSpeedValue
                     else rollAngleValue
    return (side'' * sign)
