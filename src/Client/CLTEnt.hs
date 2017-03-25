module Client.CLTEnt
    ( addTEnts
    , clearTEnts
    , parseTEnt
    , registerTEntModels
    , registerTEntSounds
    , smokeAndFlash
    ) where

import           Control.Lens        (use, (^.), (%=), (&), (.~), (%~))
import           Control.Monad       (void, when, unless)
import           Data.Bits           (shiftR, (.|.))
import           Data.IORef          (newIORef, readIORef, modifyIORef')
import           Data.Maybe          (isNothing)
import qualified Data.Vector         as V
import           Linear              (V3(..), norm, normalize, _x, _y, _z)

import           Client.BeamT
import           Client.ClientStateT
import           Client.CLSustainT
import           Client.EntityT
import           Client.ExplosionT
import           Client.LaserT
import           Client.RefDefT
import           Client.RefExportT
import qualified Client.VShared      as ClientV
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com         as Com
import qualified QCommon.CVar        as CVar
import           QuakeRef
import           QuakeState
import           Render.Renderer
import qualified Sound.S             as S
import           Types
import qualified Util.Lib            as Lib

exFree :: Int
exFree = 0

exExplosion :: Int
exExplosion = 1

exMisc :: Int
exMisc = 2

exFlash :: Int
exFlash = 3

exMFlash :: Int
exMFlash = 4

exPoly :: Int
exPoly = 5

exPoly2 :: Int
exPoly2 = 6

clearTEnts :: Quake ()
clearTEnts =
    clTEntGlobals %= (\v -> v & clteExplosions .~ V.replicate Constants.maxExplosions newExplosionT
                              & clteBeams .~ V.replicate Constants.maxBeams newBeamT
                              & cltePlayerBeams .~ V.replicate Constants.maxBeams newBeamT
                              & clteLasers .~ V.replicate Constants.maxLasers newLaserT
                              & clteSustains .~ V.replicate Constants.maxSustains newCLSustainT)

parseTEnt :: Quake ()
parseTEnt = error "CLTEnt.parseTEnt" -- TODO

registerTEntSounds :: Quake ()
registerTEntSounds = do
    sfxRic1      <- S.registerSound "world/ric1.wav"
    sfxRic2      <- S.registerSound "world/ric2.wav"
    sfxRic3      <- S.registerSound "world/ric3.wav"
    sfxLashIt    <- S.registerSound "weapons/lashit.wav"
    sfxSpark5    <- S.registerSound "world/spark5.wav"
    sfxSpark6    <- S.registerSound "world/spark6.wav"
    sfxSpark7    <- S.registerSound "world/spark7.wav"
    sfxRailg     <- S.registerSound "weapons/railgf1a.wav"
    sfxRockExp   <- S.registerSound "weapons/rocklx1a.wav"
    sfxGrenExp   <- S.registerSound "weapons/grenlx1a.wav"
    sfxWatrExp   <- S.registerSound "weapons/xpld_wat.wav"
    sfxFootStep1 <- S.registerSound "player/step1.wav"
    sfxFootStep2 <- S.registerSound "player/step2.wav"
    sfxFootStep3 <- S.registerSound "player/step3.wav"
    sfxFootStep4 <- S.registerSound "player/step4.wav"
    sfxLightning <- S.registerSound "weapons/tesla.wav"
    sfxDisrExp   <- S.registerSound "weapons/disrupthit.wav"
    void (S.registerSound "player/land1.wav")
    void (S.registerSound "player/fall2.wav")
    void (S.registerSound "player/fall1.wav")
    clTEntGlobals %= (\v -> v & clteSfxRic1 .~ sfxRic1
                              & clteSfxRic2 .~ sfxRic2
                              & clteSfxRic3 .~ sfxRic3
                              & clteSfxLashIt .~ sfxLashIt
                              & clteSfxSpark5 .~ sfxSpark5
                              & clteSfxSpark6 .~ sfxSpark6
                              & clteSfxSpark7 .~ sfxSpark7
                              & clteSfxRailg .~ sfxRailg
                              & clteSfxRockExp .~ sfxRockExp
                              & clteSfxGrenExp .~ sfxGrenExp
                              & clteSfxWatrExp .~ sfxWatrExp
                              & clteSfxFootsteps .~ V.fromList [sfxFootStep1, sfxFootStep2, sfxFootStep3, sfxFootStep4]
                              & clteSfxLightning .~ sfxLightning
                              & clteSfxDisrExp .~ sfxDisrExp)

registerTEntModels :: Quake ()
registerTEntModels = do
    renderer <- use (globals.gRenderer)
    proceedRegisterTEntModels renderer

proceedRegisterTEntModels :: Renderer -> Quake ()
proceedRegisterTEntModels renderer = do
    modExplode         <- registerModel "models/objects/explode/tris.md2"
    modSmoke           <- registerModel "models/objects/smoke/tris.md2"
    modFlash           <- registerModel "models/objects/flash/tris.md2"
    modParasiteSegment <- registerModel "models/monsters/parasite/segment/tris.md2"
    modGrappleCable    <- registerModel "models/ctf/segment/tris.md2"
    modParasiteTip     <- registerModel "models/monsters/parasite/tip/tris.md2"
    modExplo4          <- registerModel "models/objects/r_explode/tris.md2"
    modBfgExplo        <- registerModel "sprites/s_bfg2.sp2"
    modPowerScreen     <- registerModel "models/items/armor/effect/tris.md2"
    void (registerModel "models/objects/laser/tris.md2")
    void (registerModel "models/objects/grenade2/tris.md2")
    void (registerModel "models/weapons/v_machn/tris.md2")
    void (registerModel "models/weapons/v_handgr/tris.md2")
    void (registerModel "models/weapons/v_shotg2/tris.md2")
    void (registerModel "models/objects/gibs/bone/tris.md2")
    void (registerModel "models/objects/gibs/sm_meat/tris.md2")
    void (registerModel "models/objects/gibs/bone2/tris.md2")
    -- void (registerModel "models/objects/blaser/tris.md2")
    void (registerPic "w_machinegun")
    void (registerPic "a_bullets")
    void (registerPic "i_health")
    void (registerPic "a_grenades")
    modExplo4Big       <- registerModel "models/objects/r_explode2/tris.md2"
    modLightning       <- registerModel "models/proj/lightning/tris.md2"
    modHeatBeam        <- registerModel "models/proj/beam/tris.md2"
    modMonsterHeatBeam <- registerModel "models/proj/widowbeam/tris.md2"
    clTEntGlobals %= (\v -> v & clteModExplode .~ modExplode
                              & clteModSmoke .~ modSmoke
                              & clteModFlash .~ modFlash
                              & clteModParasiteSegment .~ modParasiteSegment
                              & clteModGrappleCable .~ modGrappleCable
                              & clteModParasiteTip .~ modParasiteTip
                              & clteModExplo4 .~ modExplo4
                              & clteModBfgExplo .~ modBfgExplo
                              & clteModPowerScreen .~ modPowerScreen
                              & clteModLightning .~ modLightning
                              & clteModHeatBeam .~ modHeatBeam
                              & clteModMonsterHeatBeam .~ modMonsterHeatBeam
                              & clteModExplo4Big .~ modExplo4Big)
  where
    registerModel = renderer^.rRefExport.reRegisterModel
    registerPic = renderer^.rRefExport.reRegisterPic

smokeAndFlash :: V3 Float -> Quake ()
smokeAndFlash = error "CLTEnt.smokeAndFlash" -- TODO

addTEnts :: Quake ()
addTEnts = do
    addBeams
    -- PMM - draw plasma beams
    addPlayerBeams
    addExplosions
    addLasers
    -- PMM - set up sustain
    processSustain

addBeams :: Quake ()
addBeams = do
    cl <- use (globals.gCl)
    mapM_ (addBeam cl) (fmap Ref [0..Constants.maxBeams-1])

addBeam :: ClientStateT -> Ref BeamT -> Quake ()
addBeam cl beamRef = do
    beam <- readRef beamRef
    unless (isNothing (beam^.bModel) || (beam^.bEndTime) < (cl^.csTime)) $ do
        modLightning <- use (clTEntGlobals.clteModLightning)
        -- if coming from the player, update the start position
        let start
                | (beam^.bEntity) == (cl^.csPlayerNum) + 1 = -- entity 0 is the world
                    let V3 x y z = cl^.csRefDef.rdViewOrg
                    in V3 x y (z - 22) -- adjust for view height
                | otherwise = beam^.bStart
            org = start + (beam^.bOffset)
            dist = (beam^.bEnd) - org
            (pitch, yaw) 
                | (dist^._y) == 0 && (dist^._x) == 0 =
                    let pitch = if (dist^._z) > 0 then 90 else 270
                    in (pitch, 0)
                | otherwise =
                    let yaw | (dist^._x) /= 0 = (atan2 (dist^._y) (dist^._x)) * 180 / pi
                            | (dist^._y) > 0 = 90
                            | otherwise = 270
                        yaw' = if yaw < 0 then yaw + 360 else yaw
                        forward = sqrt((dist^._x) * (dist^._x) + (dist^._y) * (dist^._y))
                        pitch = (atan2 (dist^._z) forward) * (-180) / pi
                        pitch' = if pitch < 0 then pitch + 360 else pitch
                    in (pitch', yaw')
            d = norm dist
            dist' = normalize dist
            (modelLength, d')
                | (beam^.bModel) == modLightning = (35.0, d - 20) -- correction so it doesn't end in middle of tesla
                | otherwise = (30.0, d)
            steps = fromIntegral (ceiling (d' / modelLength))
            len = (d' - modelLength) / (steps - 1)
        modifyRef beamRef (\v -> v & bStart .~ start)
        doAddBeam beam modLightning org len pitch yaw d' dist' modelLength

doAddBeam :: BeamT -> Maybe (Ref ModelT) -> V3 Float -> Float -> Float -> Float -> Float -> V3 Float -> Float -> Quake ()
doAddBeam beam modLightning org len pitch yaw d dist modelLength
    | (beam^.bModel) == modLightning && d <= modelLength = do
        r <- Lib.rand
        entRef <- io $ newIORef $ newEntityT & eOrigin .~ beam^.bEnd
                                             & eModel .~ beam^.bModel
                                             & enFlags .~ Constants.rfFullBright
                                             & eAngles .~ V3 pitch yaw (fromIntegral (r `mod` 360))
        ClientV.addEntity entRef
    | otherwise =
        constructBeams beam org len modLightning pitch yaw d dist modelLength

constructBeams :: BeamT -> V3 Float -> Float -> Maybe (Ref ModelT) -> Float -> Float -> Float -> V3 Float -> Float -> Quake ()
constructBeams beam org len modLightning pitch yaw d dist modelLength
    | d <= 0 = return ()
    | otherwise = do
        r <- Lib.rand
        constructBeam r
        constructBeams beam (org + fmap (* len) dist) len modLightning pitch yaw (d - modelLength) dist modelLength
  where
    constructBeam r
        | (beam^.bModel) == modLightning = do
            entRef <- io $ newIORef $ newEntityT & eOrigin .~ org
                                                 & eModel .~ (beam^.bModel)
                                                 & enFlags .~ Constants.rfFullBright
                                                 & eAngles .~ V3 (-pitch) (yaw + 180) (fromIntegral (r `mod` 360))
            ClientV.addEntity entRef
        | otherwise = do
            entRef <- io $ newIORef $ newEntityT & eOrigin .~ org
                                                 & eModel .~ (beam^.bModel)
                                                 & eAngles .~ V3 pitch yaw (fromIntegral (r `mod` 360))
            ClientV.addEntity entRef

addPlayerBeams :: Quake ()
addPlayerBeams = do
    cl <- use (globals.gCl)
    handMultiplier <- getHandMultiplier
    mapM_ (addPlayerBeam cl handMultiplier) (fmap Ref [0..Constants.maxBeams-1])
  where
    getHandMultiplier = do
        hand <- CVar.findVar "hand"
        return (maybe 1 convertHandValue hand)
    convertHandValue hand
        | (hand^.cvValue) == 2 = 0
        | (hand^.cvValue) == 1 = -1
        | otherwise = 1

addPlayerBeam :: ClientStateT -> Float -> Ref BeamT -> Quake ()
addPlayerBeam cl handMultiplier beamRef = do
    beam <- readRef beamRef
    unless (isNothing (beam^.bModel) || (beam^.bEndTime) < (cl^.csTime)) $ do
        modLightning <- use (clTEntGlobals.clteModLightning)
        modHeatBeam <- use (clTEntGlobals.clteModHeatBeam)
        hand <- CVar.findVar "hand"
        error "CLTEnt.addPlayerBeam" -- TODO
{-

                  let (org, start, r, f, u) = if isJust modHeatBeam && (b^.bModel) == modHeatBeam
                                                then
                                                  -- if coming from the player, update the start position
                                                  if (b^.bEntity) == (cl'^.csPlayerNum) + 1 -- entity 0 is the world
                                                         -- set up gun position
                                                    then let ps = cl'^.csFrame.fPlayerState
                                                             j = ((cl'^.csFrame.fServerFrame) - 1) .&. Constants.updateMask
                                                             oldFrame = (cl'^.csFrames) V.! j
                                                             oldFrame' = if (oldFrame^.fServerFrame) /= (cl'^.csFrame.fServerFrame) - 1 || not (oldFrame^.fValid)
                                                                           then cl'^.csFrame -- previous frame was dropped or invalid
                                                                           else oldFrame
                                                             ops = oldFrame'^.fPlayerState
                                                             start = (cl'^.csRefDef.rdViewOrg)
                                                                   + (ops^.psGunOffset)
                                                                   + (fmap (* (cl'^.csLerpFrac)) ((ps^.psGunOffset) - (ops^.psGunOffset)))
                                                             org = start + fmap (* (handMultiplier * (b^.bOffset._x))) (cl'^.csVRight)
                                                             org' = org + fmap (* (b^.bOffset._y)) (cl'^.csVForward)
                                                             org'' = org' + fmap (* (b^.bOffset._z)) (cl'^.csVUp)
                                                             org''' = case maybeHand of
                                                                        Nothing -> org''
                                                                        Just hand -> if (hand^.cvValue) == 2
                                                                                       then org'' + fmap (* (-1)) (cl'^.csVUp)
                                                                                       else org''
                                                         in (org''', start, (cl'^.csVRight), (cl'^.csVForward), (cl'^.csVUp))
                                                    else
                                                      (b^.bStart, b^.bStart, V3 0 0 0, V3 0 0 0, V3 0 0 0)
                                                else
                                                  -- if coming from the player, update the start position
                                                  let start = if (b^.bEntity) == (cl'^.csPlayerNum) + 1 -- entity 0 is the world
                                                                then let V3 x y z = cl'^.csRefDef.rdViewOrg
                                                                     in V3 x y (z - 22) -- adjust for view height
                                                                else b^.bStart
                                                      org = start + (b^.bOffset)
                                                  in (org, start, V3 0 0 0, V3 0 0 0, V3 0 0 0)

                      dist = (b^.bEnd) - org
                      (dist', org') = if isJust modHeatBeam && (b^.bModel) == modHeatBeam && (b^.bEntity) == (cl'^.csPlayerNum) + 1
                                        then let len = norm dist
                                                 dist' = fmap (* len) f
                                                       + fmap (* (handMultiplier * (b^.bOffset._x))) r
                                                       + fmap (* (b^.bOffset._y)) f
                                                       + fmap (* (b^.bOffset._z)) u
                                                 org' = case maybeHand of
                                                          Nothing -> org
                                                          Just hand -> if (hand^.cvValue) == 2
                                                                         then org + fmap (* (-1)) (cl'^.csVUp)
                                                                         else org
                                             in (dist', org')
                                        else (dist, org)

                      (pitch, yaw) = if (dist'^._y) == 0 && (dist'^._x) == 0
                                       then let pitch = if (dist'^._z) > 0 then 90 else 270
                                            in (pitch, 0)
                                       else let yaw = if | (dist'^._x) /= 0 -> (atan2 (dist'^._y) (dist'^._x)) * 180 / pi
                                                         | (dist'^._y) > 0 -> 90
                                                         | otherwise -> 270
                                                yaw' = if yaw < 0 then yaw + 360 else yaw
                                                forward = sqrt((dist'^._x) * (dist'^._x) + (dist'^._y) * (dist'^._y))
                                                pitch = (atan2 (dist'^._z) forward) * (-180) / pi
                                                pitch' = if pitch < 0 then pitch + 360 else pitch
                                            in (pitch', yaw')

                  (frameNum, org'') <- if isJust modHeatBeam && (b^.bModel) == modHeatBeam
                                         then
                                           if (b^.bEntity) /= (cl'^.csPlayerNum) + 1
                                             then do
                                               let angles = V3 (-pitch) (yaw + 180) 0
                                                   (Just f', Just r', Just u') = Math3D.angleVectors angles True True True
                                               v3o <- use $ globals.vec3Origin
         
                                               -- if it's a non-origin offset, it's a player,
                                               -- so use the hardcoded player offset
                                               org'' <- if (b^.bOffset) /= v3o
                                                          then 
                                                            return $ org' + fmap (* (1 - (b^.bOffset._x))) r'
                                                                          + fmap (* (0 - (b^.bOffset._y))) f'
                                                                          + fmap (* (0 - (b^.bOffset._z) - 10)) u'
                                                          else do
                                                            CLNewFX.monsterPlasmaShell start
                                                            return org'
         
                                               return (2, org'')
                                             else
                                               return (1, org')
                                         else
                                           return (0, org')

                  clTEntGlobals.cltePlayerBeams.ix idx.bStart .= start

                  -- if it's the heatbeam, draw the particle effect
                  when (isJust modHeatBeam && (b^.bModel) == modHeatBeam && (b^.bEntity) == (cl'^.csPlayerNum) + 1) $
                    CLNewFX.heatBeam org'' dist'

                  let d = norm dist'
                      dist'' = normalize dist'
                      (d', modelLength) = if | (b^.bModel) == modHeatBeam -> (d, 32.0)
                                             | (b^.bModel) == modLightning -> (d - 20, 35.0) -- correction so it doesn't end in middle of tesla
                                             | otherwise -> (d, 30.0)
                      steps = fromIntegral $ ceiling (d' / modelLength)
                      len = (d' - modelLength) / (steps - 1)
         
                  -- PMM - special case for lightning model .. if the real
                  -- length is shorter than the model,
                  -- flip it around & draw it from the end to the start.
                  -- This prevents the model from going through the tesla
                  -- mine (instead it goes through the target)
                  if (b^.bModel) == modLightning && d' <= modelLength
                    then do
                      r <- Lib.rand
                      entRef <- io $ newIORef newEntityT { _eOrigin = b^.bEnd
                                                         , _eModel = b^.bModel
                                                         , _enFlags = Constants.rfFullBright
                                                         , _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360)
                                                         }

                      ClientV.addEntity entRef
                    else do
                      constructBeams b org'' len modHeatBeam modLightning pitch yaw d' dist'' modelLength frameNum
                      addPlayerBeam beams cl' handMultiplier (idx + 1) maxIdx

        constructBeams :: BeamT -> V3 Float -> Float -> Maybe (IORef ModelT) -> Maybe (IORef ModelT) -> Float -> Float -> Float -> V3 Float -> Float -> Int -> Quake ()
        constructBeams b org len modHeatBeam modLightning pitch yaw d dist modelLength frameNum
          | d <= 0 = return ()
          | otherwise = do
              let ent = newEntityT { _eOrigin = org
                                   , _eModel = b^.bModel
                                   }

              r <- Lib.rand

              entRef <- io $ if | isJust modHeatBeam && (b^.bModel) == modHeatBeam ->
                                    newIORef ent { _enFlags = Constants.rfFullBright
                                                 , _eAngles = V3 (-pitch) (yaw + 180) (fromIntegral $ r `mod` 360)
                                                 , _eFrame = frameNum
                                                 }
                                | (b^.bModel) == modLightning ->
                                    newIORef ent { _enFlags = Constants.rfFullBright
                                                 , _eAngles = V3 (-pitch) (yaw + 180) (fromIntegral $ r `mod` 360)
                                                 }

                                | otherwise ->
                                    newIORef ent { _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360) }

              ClientV.addEntity entRef

              let org' = org + fmap (* len) dist
              constructBeams b org' len modHeatBeam modLightning pitch yaw (d - modelLength) dist modelLength frameNum
              -}

addExplosions :: Quake ()
addExplosions = do
    cl <- use (globals.gCl)
    mapM_ (addExplosion cl) (fmap Ref [0..Constants.maxExplosions-1])

addExplosion :: ClientStateT -> Ref ExplosionT -> Quake ()
addExplosion cl explosionRef = do
    explosion <- readRef explosionRef
    unless ((explosion^.eType) == exFree) $ do
        let frac = (fromIntegral (cl^.csTime) - (explosion^.eStart)) / 100
            f = truncate frac
        explosion' <- checkExplosionType frac f explosion
        unless ((explosion'^.eType) == exFree) $ do
            ent <- io (readIORef (explosion'^.eEnt))
            when ((explosion'^.eLight) /= 0) $
                ClientV.addLight (ent^.eOrigin)
                                 ((explosion'^.eLight) * (ent^.eAlpha))
                                 (explosion'^.eLightColor._x)
                                 (explosion'^.eLightColor._y)
                                 (explosion'^.eLightColor._z)
            let f' = max 0 f
            io $ modifyIORef' (explosion'^.eEnt) (\v -> v & eOldOrigin .~ (ent^.eOrigin)
                                                          & eFrame     .~ (explosion'^.eBaseFrame) + f' + 1
                                                          & eOldFrame  .~ (explosion'^.eBaseFrame) + f'
                                                          & eBackLerp  .~ 1 - (cl^.csLerpFrac))
            ClientV.addEntity (explosion'^.eEnt)
  where
    checkExplosionType frac f explosion
        | (explosion^.eType) == exMFlash =
            if f >= (explosion^.eFrames) - 1
                then return (explosion & eType .~ exFree)
                else return explosion
        | (explosion^.eType) == exMisc =
            if f >= (explosion^.eFrames) - 1
                then return (explosion & eType .~ exFree)
                else do
                    io $ modifyIORef' (explosion^.eEnt) (\v -> v & eAlpha .~ 1 - frac / fromIntegral ((explosion^.eFrames) - 1))
                    return explosion
        | (explosion^.eType) == exFlash =
            if f >= 1
                then return (explosion & eType .~ exFree)
                else do
                    io $ modifyIORef' (explosion^.eEnt) (\v -> v & eAlpha .~ 1)
                    return explosion
        | (explosion^.eType) == exPoly =
            if f >= (explosion^.eFrames) - 1
                then return (explosion & eType .~ exFree)
                else do
                    let alpha = (16.0 - fromIntegral f) / 16
                        skinNum | f < 10 = max 0 (f `shiftR` 1)
                                | f < 13 = 5
                                | otherwise = 6
                        flagsUpdate | f < 10 = 0
                                    | otherwise = Constants.rfTranslucent
                    io $ modifyIORef' (explosion^.eEnt) (\v -> v & eAlpha .~ alpha
                                                                 & eSkinNum .~ skinNum
                                                                 & enFlags %~ (.|. flagsUpdate))
                    return explosion
        | (explosion^.eType) == exPoly2 =
            if f >= (explosion^.eFrames) - 1
                then return (explosion & eType .~ exFree)
                else do
                    io $ modifyIORef' (explosion^.eEnt) (\v -> v & eAlpha .~ (5.0 - fromIntegral f) / 5.0
                                                                 & eSkinNum .~ 0
                                                                 & enFlags %~ (.|. Constants.rfTranslucent))
                    return explosion
        | otherwise = return explosion

addLasers :: Quake ()
addLasers = do
    time <- use (globals.gCl.csTime)
    mapM_ (addLaser time) (fmap Ref [0..Constants.maxLasers-1])
  where
    addLaser time laserRef = do
        laser <- readRef laserRef
        when ((laser^.lEndTime) >= time) $
            ClientV.addEntity (laser^.lEnt)

processSustain :: Quake ()
processSustain = do
    time <- use (globals.gCl.csTime)
    mapM_ (process time) (fmap Ref [0..Constants.maxSustains-1])
  where
    process time sustainRef = do
        sustain <- readRef sustainRef
        doProcess time sustainRef sustain
    doProcess time sustainRef sustain
        | (sustain^.clsEndTime) >= time && time >= (sustain^.clsNextThink) =
            maybe thinkError (\think -> void (think sustain)) (sustain^.clsThink)
        | (sustain^.clsEndTime) < time =
            modifyRef sustainRef (\v -> v & clsId .~ 0)
        | otherwise =
            return ()
    thinkError =
        Com.fatalError "CLTEnt.processSustain sustain^.clsThink is Nothing"