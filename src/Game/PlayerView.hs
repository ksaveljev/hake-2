module Game.PlayerView
    ( clientEndServerFrame
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (+~), (%~))
import           Control.Monad         (when, unless)
import           Data.Bits             (complement, xor, (.&.), (.|.))
import           Data.Maybe            (isJust)
import           Linear                (V3(..), dot, _x, _y, _z)

import qualified Constants
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameCombat       as GameCombat
import           Game.GameLocalsT
import           Game.GClientT
import           Game.LevelLocalsT
import qualified Game.PlayerHud        as PlayerHud
import           Game.PlayerStateT
import qualified Game.PlayerWeapon     as PlayerWeapon
import           Game.PMoveStateT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

import {-# SOURCE #-} Game.GameImportT

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
fallingDamage = error "PlayerView.fallingDamage" -- TODO

damageFeedback :: Ref EdictT -> Quake ()
damageFeedback = error "PlayerView.damageFeedback" -- TODO

calcViewOffset :: Ref EdictT -> Quake ()
calcViewOffset = error "PlayerView.calcViewOffset" -- TODO

calcGunOffset :: Ref EdictT -> Quake ()
calcGunOffset = error "PlayerView.calcGunOffset" -- TODO

calcBlend :: Ref EdictT -> Quake ()
calcBlend = error "PlayerView.calcBlend" -- TODO

setClientEvent :: Ref EdictT -> Quake ()
setClientEvent = error "PlayerView.setClientEvent" -- TODO

setClientEffects :: Ref EdictT -> Quake ()
setClientEffects = error "PlayerView.setClientEffects" -- TODO

setClientSound :: Ref EdictT -> Quake ()
setClientSound = error "PlayerView.setClientSound" -- TODO

setClientFrame :: Ref EdictT -> Quake ()
setClientFrame = error "PlayerView.setClientFrame" -- TODO

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
