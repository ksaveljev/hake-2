{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.CLFX where

-- Client Graphics Effects

import Control.Lens ((.=), ix, use, (^.), preuse, zoom, (&), (.~), (+~), (-~), (%=))
import Control.Monad (unless, when, liftM)
import Data.Bits ((.&.), complement)
import Data.Char (ord)
import Data.IORef (IORef, newIORef, modifyIORef', writeIORef, readIORef)
import Linear (V3(..), _x, _y, _z, norm, normalize)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Types
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Client.CLTEnt as CLTEnt
import {-# SOURCE #-} qualified Client.V as ClientV
import qualified Game.Monsters.MFlash as MFlash
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.MSG as MSG
import qualified Sound.S as S
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

instantParticle :: Float
instantParticle = -10000.0

particleGravity :: Float
particleGravity = 40

colorTable :: UV.Vector Int
colorTable = UV.fromList [ 2 * 8, 13 * 8, 21 * 8, 18 * 8 ]

runDLights :: Quake ()
runDLights = do
    time <- use $ globals.cl.csTime
    dLights <- use $ clientGlobals.cgDLights
    io $ runDLight dLights (fromIntegral time) 0 Constants.maxDLights

  where runDLight :: V.Vector (IORef CDLightT) -> Float -> Int -> Int -> IO ()
        runDLight dLights time idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let dlRef = dLights V.! idx
              dl <- readIORef dlRef

              if | dl^.cdlRadius == 0 -> runDLight dLights time (idx + 1) maxIdx
                 | dl^.cdlDie < time -> modifyIORef' dlRef (\v -> v { _cdlRadius = 0 })
                 | otherwise -> runDLight dLights time (idx + 1) maxIdx
                 -- TODO: original quake2 code does have something else
                 -- here (jake2 is missing a part of this function)

runLightStyles :: Quake ()
runLightStyles = do
    time <- use $ globals.cl.csTime
    lastOfs <- use $ clientGlobals.cgLastOfs
    let ofs = time `div` 100

    unless (ofs == lastOfs) $ do
      clientGlobals.cgLastOfs .= ofs
      lightStyles <- use $ clientGlobals.cgLightStyle
      runLightStyle lightStyles ofs 0 (V.length lightStyles)

  where runLightStyle :: V.Vector CLightStyleT -> Int -> Int -> Int -> Quake ()
        runLightStyle lightStyles ofs idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ls = lightStyles V.! idx

              if | ls^.clsLength == 0 -> do
                     clientGlobals.cgLightStyle.ix idx.clsValue .= V3 1 1 1
                 | ls^.clsLength == 1 -> do
                     let v = (ls^.clsMap) UV.! 0
                     clientGlobals.cgLightStyle.ix idx.clsValue .= V3 v v v
                 | otherwise -> do
                     let v = (ls^.clsMap) UV.! (ofs `mod` (ls^.clsLength))
                     clientGlobals.cgLightStyle.ix idx.clsValue .= V3 v v v

              runLightStyle lightStyles ofs (idx + 1) maxIdx

clearEffects :: Quake ()
clearEffects = do
    clearParticles
    clearDLights
    clearLightStyles

clearParticles :: Quake ()
clearParticles = do
    particles <- use $ clientGlobals.cgParticles

    clientGlobals.cgFreeParticles .= Just (particles V.! 0)
    clientGlobals.cgActiveParticles .= Nothing

    io $ do
      let len = V.length particles - 1
      setParticleChain particles 0 (len - 1)
      modifyIORef' (particles V.! (len - 1)) (\v -> v { _cpNext = Nothing })

  where setParticleChain :: V.Vector (IORef CParticleT) -> Int -> Int -> IO ()
        setParticleChain particles idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              modifyIORef' (particles V.! idx) (\v -> v { _cpNext = Just $ particles V.! (idx + 1) })
              setParticleChain particles (idx + 1) maxIdx

clearDLights :: Quake ()
clearDLights = do
    dlights <- io $ V.replicateM Constants.maxDLights (newIORef newCDLightT)
    clientGlobals.cgDLights .= dlights

clearLightStyles :: Quake ()
clearLightStyles = do
    clientGlobals.cgLightStyle .= V.replicate Constants.maxLightStyles newCLightStyleT
    clientGlobals.cgLastOfs .= -1

-- Int is reference to globals.cl.csConfigStrings
setLightStyle :: Int -> Quake () 
setLightStyle csIdx = do
    Just str <- preuse $ globals.cl.csConfigStrings.ix (csIdx + Constants.csLights)
    let len = B.length str

    when (len >= Constants.maxQPath) $
      Com.comError Constants.errDrop $ "svc_lightstyle length=" `B.append` BC.pack (show len) -- IMPROVE?

    let d :: Float = fromIntegral (ord 'm' - ord 'a') -- so we do not recalculate it every time
        lsMap = UV.unfoldr buildLightStyle (str, d, 0)

    zoom (clientGlobals.cgLightStyle.ix csIdx) $ do
      clsLength .= len
      clsMap .=  lsMap -- TODO: make sure we never want to access something beyond length

  where buildLightStyle :: (B.ByteString, Float, Int) -> Maybe (Float, (B.ByteString, Float, Int))
        buildLightStyle (str, d, idx)
          | idx >= B.length str = Nothing
          | otherwise =
              let a :: Float = fromIntegral $ ord (str `BC.index` idx) - ord 'a'
              in Just (a / d, (str, d, idx + 1))

teleporterParticles :: EntityStateT -> Quake ()
teleporterParticles ent = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addTeleporterParticles freeParticles 0 8

  where addTeleporterParticles :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addTeleporterParticles Nothing _ _ = return ()
        addTeleporterParticles (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              activeParticles <- use $ clientGlobals.cgActiveParticles
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime

              o1 <- Lib.rand
              o2 <- Lib.rand
              o3 <- Lib.rand

              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.rand

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = 0xDB
                                              , _cpOrg = V3 ((ent^.esOrigin._x) - 16 + fromIntegral (o1 .&. 31)) ((ent^.esOrigin._y) - 16 + fromIntegral (o2 .&. 31)) ((ent^.esOrigin._z) - 8 + fromIntegral (o3 .&. 7))
                                              , _cpVel = V3 (v1 * 14) (v2 * 14) (80 + fromIntegral (v3 .&. 3))
                                              , _cpAccel = V3 0 0 (- particleGravity)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = -0.5
                                              })

              addTeleporterParticles (p^.cpNext) (idx + 1) maxIdx

bigTeleportParticles :: V3 Float -> Quake ()
bigTeleportParticles org = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addBigTeleportParticles freeParticles 0 4096

  where addBigTeleportParticles :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addBigTeleportParticles Nothing _ _ = return ()
        addBigTeleportParticles (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              activeParticles <- use $ clientGlobals.cgActiveParticles
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              color <- Lib.rand >>= \r -> return $ fromIntegral (colorTable UV.! (fromIntegral r .&. 3))
              angle <- Lib.rand >>= \r -> return $ pi * 2 * fromIntegral (r .&. 1023) / 1023.0
              dist <- Lib.rand >>= \r -> return $ fromIntegral (r .&. 31)

              o <- Lib.rand
              v1 <- Lib.rand
              v2 <- Lib.rand
              v3 <- Lib.rand
              f <- Lib.randomF

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = color
                                              , _cpOrg = V3 ((org^._x) + (cos angle) * dist) ((org^._y) + (sin angle) * dist) ((org^._z) + 8 + fromIntegral (o `mod` 90))
                                              , _cpVel = V3 ((cos angle) * (70 + fromIntegral (v1 .&. 63))) ((sin angle) * (70 + fromIntegral (v2 .&. 63))) ((-100) + fromIntegral (v3 .&. 31))
                                              , _cpAccel = V3 ((-100) * (cos angle)) ((-100) * (sin angle)) (particleGravity * 4)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-0.3) / (0.5 + f * 0.3)
                                              })

              addBigTeleportParticles (p^.cpNext) (idx + 1) maxIdx

{-
- ============== CL_EntityEvent ==============
- 
- An entity has just been parsed that has an event value
- 
- the female events are there for backwards compatability
-}
entityEvent :: EntityStateT -> Quake ()
entityEvent entityState = do
    if | (entityState^.esEvent) == Constants.evItemRespawn -> do
           sfx <- S.registerSound "items/respawn1.wav"
           S.startSound Nothing (newEdictReference (entityState^.esNumber)) Constants.chanWeapon sfx 1 Constants.attnIdle 0
           itemRespawnParticles (entityState^.esOrigin)

       | (entityState^.esEvent) == Constants.evPlayerTeleport -> do
           sfx <- S.registerSound "misc/tele1.wav"
           S.startSound Nothing (newEdictReference (entityState^.esNumber)) Constants.chanWeapon sfx 1 Constants.attnIdle 0
           teleportParticles (entityState^.esOrigin)

       | (entityState^.esEvent) == Constants.evFootstep -> do
           footstepsValue <- liftM (^.cvValue) clFootstepsCVar
           when (footstepsValue /= 0) $ do
             r <- Lib.rand
             let idx = fromIntegral (r .&. 3)
             Just sfx <- preuse $ clTEntGlobals.clteSfxFootsteps.ix idx
             S.startSound Nothing (newEdictReference (entityState^.esNumber)) Constants.chanBody sfx 1 Constants.attnNorm 0

       | (entityState^.esEvent) == Constants.evFallShort -> do
           sfx <- S.registerSound "player/land1.wav"
           S.startSound Nothing (newEdictReference (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0

       | (entityState^.esEvent) == Constants.evFall -> do
           sfx <- S.registerSound "*fall2.wav"
           S.startSound Nothing (newEdictReference (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0

       | (entityState^.esEvent) == Constants.evFallFar -> do
           sfx <- S.registerSound "*fall1.wav"
           S.startSound Nothing (newEdictReference (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0

       | otherwise ->
           return () -- TODO: expected?

addParticles :: Quake ()
addParticles = do
    activeParticles <- use $ clientGlobals.cgActiveParticles
    cl' <- use $ globals.cl
    addParticle cl' activeParticles Nothing Nothing 0

  where addParticle :: ClientStateT -> Maybe (IORef CParticleT) -> Maybe (IORef CParticleT) -> Maybe (IORef CParticleT) -> Float -> Quake ()
        addParticle _ Nothing active _ _ =
          clientGlobals.cgActiveParticles .= active
        addParticle cl' (Just pRef) activeRef tailRef time = do
          p <- io $ readIORef pRef
          let next = p^.cpNext

          (done, time', alpha) <- if (p^.cpAlphaVel) /= instantParticle
                                    then do
                                      let time' = (fromIntegral (cl'^.csTime) - (p^.cpTime)) * 0.001
                                          alpha = (p^.cpAlpha) + time' * (p^.cpAlphaVel)

                                      if alpha <= 0 -- faded out
                                        then do
                                          freeParticles <- use $ clientGlobals.cgFreeParticles
                                          io $ modifyIORef' pRef (\v -> v { _cpNext = freeParticles })
                                          clientGlobals.cgFreeParticles .= Just pRef
                                          return (True, time', alpha)
                                        else
                                          return (False, time', alpha)
                                    else
                                      return (False, time, p^.cpAlpha)

          if done
            then
              addParticle cl' next activeRef tailRef time'
            else do
              io $ modifyIORef' pRef (\v -> v { _cpNext = Nothing })
              (activeRef', tailRef') <- case tailRef of
                                          Nothing ->
                                            return (Just pRef, Just pRef)
                                          Just ref -> do
                                            io $ modifyIORef' ref (\v -> v { _cpNext = Just pRef })
                                            return (activeRef, Just pRef)

              let alpha' = if alpha > 1 then 1 else alpha
                  color = truncate (p^.cpColor) :: Int
                  time2 = time' * time'
                  org = (p^.cpOrg) + fmap (* time') (p^.cpVel) + fmap (* time2) (p^.cpAccel)

              ClientV.addParticle org color alpha'

              when ((p^.cpAlphaVel) == instantParticle) $
                io $ modifyIORef' pRef (\v -> v { _cpAlpha = 0
                                                , _cpAlphaVel = 0
                                                })

              addParticle cl' next activeRef' tailRef' time'

addDLights :: Quake ()
addDLights = do
    -- TODO: currently simplified version... need to update it to reflect
    -- jake2 version correctly
    dlights <- use $ clientGlobals.cgDLights
    addDLight dlights 0 Constants.maxDLights

  where addDLight :: V.Vector (IORef CDLightT) -> Int -> Int -> Quake ()
        addDLight dlights idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let dlRef = dlights V.! idx
              dl <- io $ readIORef dlRef
              
              if (dl^.cdlRadius) == 0
                then
                  addDLight dlights (idx + 1) maxIdx
                else do
                  ClientV.addLight (dl^.cdlOrigin) (dl^.cdlRadius) (dl^.cdlColor._x) (dl^.cdlColor._y) (dl^.cdlColor._z)
                  addDLight dlights (idx + 1) maxIdx

addLightStyles :: Quake ()
addLightStyles = do
    lightStyles <- use $ clientGlobals.cgLightStyle
    addLightStyle lightStyles 0 Constants.maxLightStyles

  where addLightStyle :: V.Vector CLightStyleT -> Int -> Int -> Quake ()
        addLightStyle lightStyles idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ls = lightStyles V.! idx
              ClientV.addLightStyle idx (ls^.clsValue._x) (ls^.clsValue._y) (ls^.clsValue._z)
              addLightStyle lightStyles (idx + 1) maxIdx

itemRespawnParticles :: V3 Float -> Quake ()
itemRespawnParticles org = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addItemRespawnParticles freeParticles 0 64

  where addItemRespawnParticles :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addItemRespawnParticles Nothing _ _ = return ()
        addItemRespawnParticles (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              activeParticles <- use $ clientGlobals.cgActiveParticles
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              color <- Lib.rand >>= \r -> return (r .&. 3)

              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom
              f <- Lib.randomF

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = 0xD4 + fromIntegral color -- green
                                              , _cpOrg = V3 ((org^._x) + o1 * 8) ((org^._y) + o2 * 8) ((org^._z) + o3 * 8)
                                              , _cpVel = V3 (v1 * 8) (v2 * 8) (v3 * 8)
                                              , _cpAccel = V3 0 0 ((-0.2) * particleGravity)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (1.0 + f * 0.3)
                                              })

              addItemRespawnParticles (p^.cpNext) (idx + 1) maxIdx

teleportParticles :: V3 Float -> Quake ()
teleportParticles org = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addTeleportParticles freeParticles (-16) (-16) (-16)

  where addTeleportParticles :: Maybe (IORef CParticleT) -> Int -> Int -> Int -> Quake ()
        addTeleportParticles Nothing _ _ _ = return ()
        addTeleportParticles (Just pRef) i j k
          | i > 16 = return ()
          | j > 16 = addTeleportParticles (Just pRef) (i + 4) (-16) (-16)
          | k > 32 = addTeleportParticles (Just pRef) i (j + 4) (-16)
          | otherwise = do
              activeParticles <- use $ clientGlobals.cgActiveParticles
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              color <- Lib.rand >>= \r -> return $ fromIntegral (r .&. 7)

              av <- Lib.rand
              o1 <- Lib.rand
              o2 <- Lib.rand
              o3 <- Lib.rand

              let dir = normalize (fmap fromIntegral (V3 (j * 8) (i * 8) (k * 8)))
              vel <- Lib.rand >>= \r -> return (50 + fromIntegral (r .&. 63))

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = 7 + color
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.3 + fromIntegral (av .&. 7) * 0.02)
                                              , _cpOrg = V3 ((org^._x) + fromIntegral i + fromIntegral (o1 .&. 3)) ((org^._y) + fromIntegral j + fromIntegral (o2 .&. 3)) ((org^._z) + fromIntegral k + fromIntegral (o3 .&. 3))
                                              , _cpVel = fmap (* vel) dir
                                              , _cpAccel = V3 0 0 (- particleGravity)
                                              })

              addTeleportParticles (p^.cpNext) i j (k + 4)
    
{-
- =============== CL_ParticleEffect ===============
- 
- Wall impact puffs
-}
particleEffect :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
particleEffect org dir color count = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addEffects freeParticles 0

  where addEffects :: Maybe (IORef CParticleT) -> Int -> Quake ()
        addEffects Nothing _ = return ()
        addEffects (Just pRef) idx
          | idx >= count = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              pTime <- use $ globals.cl.csTime
              r <- Lib.rand
              let pColor = color + fromIntegral (r .&. 7)
              d <- liftM (fromIntegral . (.&. 31)) Lib.rand

              o1 <- Lib.rand
              v1 <- Lib.crandom
              o2 <- Lib.rand
              v2 <- Lib.crandom
              o3 <- Lib.rand
              v3 <- Lib.crandom

              let oRand = V3 o1 o2 o3
                  vRand = V3 v1 v2 v3
                  pOrg = org + fmap (fromIntegral . (subtract 4) . (.&. 7)) oRand + fmap (* d) dir
                  pVel = fmap (* 20) vRand
                  pAccel = V3 0 0 (- particleGravity)
                  pAlpha = 1.0

              r' <- Lib.randomF
              let pAlphaVel = -1 / (0.5 + r' * 0.3)

              io $ writeIORef pRef CParticleT { _cpNext = activeParticles
                                              , _cpTime = fromIntegral pTime
                                              , _cpColor = fromIntegral pColor
                                              , _cpOrg = pOrg
                                              , _cpVel = pVel
                                              , _cpAccel = pAccel
                                              , _cpAlpha = pAlpha
                                              , _cpAlphaVel = pAlphaVel
                                              }

              addEffects (p^.cpNext) (idx + 1)

particleEffect2 :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
particleEffect2 org dir color count = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addParticleEffect2 freeParticles 0

  where addParticleEffect2 :: Maybe (IORef CParticleT) -> Int -> Quake ()
        addParticleEffect2 Nothing _ = return ()
        addParticleEffect2 (Just pRef) idx
          | idx >= count = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime

              d <- Lib.rand >>= \r -> return (fromIntegral (r .&. 7))
              o1 <- Lib.rand
              o2 <- Lib.rand
              o3 <- Lib.rand
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom
              f <- Lib.randomF

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = fromIntegral color
                                              , _cpOrg = org + fmap (fromIntegral . (subtract 4) . (.&. 7)) (V3 o1 o2 o3) + fmap (* d) dir
                                              , _cpVel = fmap (* 20) (V3 v1 v2 v3)
                                              , _cpAccel = V3 0 0 (- particleGravity)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.5 + f * 0.3)
                                              })

              addParticleEffect2 (p^.cpNext) (idx + 1)

particleEffect3 :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
particleEffect3 org dir color count = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addParticleEffect3 freeParticles 0

  where addParticleEffect3 :: Maybe (IORef CParticleT) -> Int -> Quake ()
        addParticleEffect3 Nothing _ = return ()
        addParticleEffect3 (Just pRef) idx
          | idx >= count = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              d <- Lib.rand >>= \r -> return (fromIntegral (r .&. 7))
              o1 <- Lib.rand
              o2 <- Lib.rand
              o3 <- Lib.rand
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom
              f <- Lib.randomF

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = fromIntegral color
                                              , _cpOrg = org + fmap (fromIntegral . (subtract 4) . (.&. 7)) (V3 o1 o2 o3) + fmap (* d) dir
                                              , _cpVel = fmap (* 20) (V3 v1 v2 v3)
                                              , _cpAccel = V3 0 0 particleGravity
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.5 + f * 0.3)
                                              })

              addParticleEffect3 (p^.cpNext) (idx + 1)

explosionParticles :: V3 Float -> Quake ()
explosionParticles org = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addEffects freeParticles 0 256

  where addEffects :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addEffects Nothing _ _ = return ()
        addEffects (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              pTime <- use $ globals.cl.csTime
              r <- Lib.rand
              let pColor = 0xE0 + fromIntegral (r .&. 7)

              o1 <- Lib.rand
              v1 <- Lib.rand
              o2 <- Lib.rand
              v2 <- Lib.rand
              o3 <- Lib.rand
              v3 <- Lib.rand

              let oRand = V3 o1 o2 o3
                  vRand = V3 v1 v2 v3
                  pOrg = org + fmap (fromIntegral . (subtract 16) . (`mod` 32)) oRand
                  pVel = fmap (fromIntegral . (subtract 192) . (`mod` 384)) vRand
                  pAccel = V3 0 0 (- particleGravity)
                  pAlpha = 1.0

              r' <- Lib.randomF
              let pAlphaVel = -0.8 / (0.5 + r' * 0.3)

              io $ writeIORef pRef CParticleT { _cpNext = activeParticles
                                              , _cpTime = fromIntegral pTime
                                              , _cpColor = fromIntegral pColor
                                              , _cpOrg = pOrg
                                              , _cpVel = pVel
                                              , _cpAccel = pAccel
                                              , _cpAlpha = pAlpha
                                              , _cpAlphaVel = pAlphaVel
                                              }

              addEffects (p^.cpNext) (idx + 1) maxIdx

{-
- =============== CL_BlasterParticles ===============
- 
- Wall impact puffs
-}
blasterParticles :: V3 Float -> V3 Float -> Quake ()
blasterParticles org dir = do
    drawParticles 0 40

  where drawParticles :: Int -> Int -> Quake ()
        drawParticles idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              freeParticles <- use $ clientGlobals.cgFreeParticles

              case freeParticles of
                Nothing ->
                  return ()

                Just pRef -> do
                  p <- io $ readIORef pRef
                  activeParticles <- use $ clientGlobals.cgActiveParticles

                  clientGlobals.cgFreeParticles .= (p^.cpNext)
                  io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles })
                  clientGlobals.cgActiveParticles .= Just pRef

                  time <- use $ globals.cl.csTime
                  r <- Lib.rand
                  d <- liftM (fromIntegral . (.&. 15)) Lib.rand
                  o1 <- Lib.rand
                  o2 <- Lib.rand
                  o3 <- Lib.rand
                  v1 <- Lib.crandom
                  v2 <- Lib.crandom
                  v3 <- Lib.crandom
                  f <- Lib.randomF

                  io $ modifyIORef' pRef (\v -> v { _cpTime = fromIntegral time
                                                  , _cpColor = 0xE0 + fromIntegral (r .&. 7)
                                                  , _cpOrg = org + (V3 (fromIntegral $ (o1 .&. 7) - 4) (fromIntegral $ (o2 .&. 7) - 4) (fromIntegral $ (o3 .&. 7) - 4)) + fmap (* d) dir
                                                  , _cpVel = fmap (* 30) dir + fmap (* 40) (V3 v1 v2 v3)
                                                  , _cpAccel = V3 0 0 (- particleGravity)
                                                  , _cpAlpha = 1.0
                                                  , _cpAlphaVel = (-1.0) / (0.5 + f * 0.3)
                                                  })

                  drawParticles (idx + 1) maxIdx

railTrail :: V3 Float -> V3 Float -> Quake ()
railTrail start end = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = normalize vec
        (right, up) = Math3D.makeNormalVectors vec'

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addRailTrail freeParticles vec' right up move 0 len

    let vec'' = fmap (* 0.75) vec'

    freeParticles' <- use $ clientGlobals.cgFreeParticles
    addRailTrail2 freeParticles' vec'' len move

  where addRailTrail :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> Float -> Float -> Quake ()
        addRailTrail Nothing _ _ _ _ _ _ = return ()
        addRailTrail (Just pRef) vec right up move idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              let d = 0.1 * idx
                  c = cos d
                  s = sin d
                  dir = fmap (* c) right + fmap (* s) up

              time <- use $ globals.cl.csTime
              f <- Lib.randomF
              r <- Lib.rand

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles 
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (1.0 + f * 0.2)
                                              , _cpColor = 0x74 + fromIntegral (r .&. 7)
                                              , _cpOrg = move + fmap (* 3) dir
                                              , _cpVel = fmap (* 6) dir
                                              })

              addRailTrail (p^.cpNext) vec right up (move + vec) (idx + 1) maxIdx

        addRailTrail2 :: Maybe (IORef CParticleT) -> V3 Float -> Float -> V3 Float -> Quake ()
        addRailTrail2 Nothing _ _ _ = return ()
        addRailTrail2 (Just pRef) vec len move
          | len <= 0 = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              f <- Lib.randomF
              r <- Lib.rand

              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles 
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.5 + f * 0.2)
                                              , _cpColor = fromIntegral (r .&. 15)
                                              , _cpOrg = move + fmap (* 3) (V3 o1 o2 o3)
                                              , _cpVel = fmap (* 3) (V3 v1 v2 v3)
                                              })

              addRailTrail2 (p^.cpNext) vec (len - 0.75) (move + vec)

bfgExplosionParticles :: V3 Float -> Quake ()
bfgExplosionParticles org = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addBfgExplosionParticles freeParticles 0 256

  where addBfgExplosionParticles :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addBfgExplosionParticles Nothing _ _ = return ()
        addBfgExplosionParticles (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              r <- Lib.rand
              o1 <- Lib.rand
              o2 <- Lib.rand
              o3 <- Lib.rand
              v1 <- Lib.rand
              v2 <- Lib.rand
              v3 <- Lib.rand
              f <- Lib.randomF

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles 
                                              , _cpTime = fromIntegral time
                                              , _cpColor = 0xD0 + fromIntegral (r .&. 7)
                                              , _cpOrg = org + fmap (\v -> fromIntegral ((v `mod` 32) - 16)) (V3 o1 o2 o3)
                                              , _cpVel = fmap (\v -> fromIntegral ((v `mod` 384) - 192)) (V3 v1 v2 v3)
                                              , _cpAccel = V3 0 0 (- particleGravity)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-0.8) / (0.5 + f * 0.3)
                                              })

              addBfgExplosionParticles (p^.cpNext) (idx + 1) maxIdx

bubbleTrail :: V3 Float -> V3 Float -> Quake ()
bubbleTrail start end = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* 32) (normalize vec)

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addBubbleTrail freeParticles vec' move 0 len

  where addBubbleTrail :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Float -> Quake ()
        addBubbleTrail Nothing _ _ _ _ = return ()
        addBubbleTrail (Just pRef) vec move idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              r <- Lib.rand
              f <- Lib.randomF
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles 
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (1.0 + f * 0.2)
                                              , _cpColor = 4 + fromIntegral (r .&. 7)
                                              , _cpOrg = move + fmap (* 2) (V3 o1 o2 o3)
                                              , _cpVel = (fmap (* 5) (V3 v1 v2 v3)) & _z +~ 6
                                              })

              addBubbleTrail (p^.cpNext) vec (move + vec) (idx + 32) maxIdx

parseMuzzleFlash :: Quake ()
parseMuzzleFlash = do
    i <- MSG.readShort (globals.netMessage)

    when (i < 1 || i >= Constants.maxEdicts) $
      Com.comError Constants.errDrop "CL_ParseMuzzleFlash: bad entity"

    w <- MSG.readByte (globals.netMessage)
    let silenced = w .&. Constants.mzSilenced
        weapon = w .&. (complement Constants.mzSilenced)

    Just pl <- preuse $ globals.clEntities.ix i
    dlRef <- allocDLight i
    r <- Lib.rand
    time <- use $ globals.cl.csTime

    let (Just fv, Just rv, _) = Math3D.angleVectors (pl^.ceCurrent.esAngles) True True False
        origin = (pl^.ceCurrent.esOrigin)
               + fmap (* 18) fv
               + fmap (* 16) rv
        radius = if silenced /= 0
                   then 100 + fromIntegral (r .&. 31)
                   else 200 + fromIntegral (r .&. 31)
        volume = if silenced /= 0
                   then 0.2
                   else 1

    io $ modifyIORef' dlRef (\v -> v { _cdlOrigin = origin
                                     , _cdlRadius = radius
                                     , _cdlMinLight = 32
                                     , _cdlDie = fromIntegral time
                                     })

    if | weapon == Constants.mzBlaster -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "weapons/blastf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzBlueHyperblaster -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 0 1})
           soundIdx <- S.registerSound "weapons/hyprbf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzHyperblaster -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0})
           soundIdx <- S.registerSound "weapons/hyprbf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           
       | weapon == Constants.mzMachinegun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           r <- Lib.rand
           let soundName = "weapons/machgf" `B.append` BC.pack (show ((r `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE ?
           soundIdx <- S.registerSound soundName
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzShotgun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "weapons/shotgf1b.wav"
           soundIdx' <- S.registerSound "weapons/shotgr1b.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           S.startSound Nothing (newEdictReference i) Constants.chanAuto soundIdx' volume Constants.attnNorm 0.1

       | weapon == Constants.mzSShotgun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "weapons/sshotf1b.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzChaingun1 -> do
           r <- Lib.rand
           io $ modifyIORef' dlRef (\v -> v { _cdlRadius = 200 + fromIntegral (r .&. 31)
                                            , _cdlColor = V3 1 0.25 0
                                            })
           r' <- Lib.rand
           let soundName = "weapons/machgf" `B.append` BC.pack (show ((r' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx <- S.registerSound soundName
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzChaingun2 -> do
           r <- Lib.rand
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlRadius = 225 + fromIntegral (r .&. 31)
                                            , _cdlColor = V3 1 0.5 0
                                            , _cdlDie = fromIntegral time + 0.1 -- long delay
                                            })
           r' <- Lib.rand
           let soundName = "weapons/machgf" `B.append` BC.pack (show ((r' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx <- S.registerSound soundName
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

           r'' <- Lib.rand
           let soundName' = "weapons/machgf" `B.append` BC.pack (show ((r'' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx' <- S.registerSound soundName'
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx' volume Constants.attnNorm 0.05

       | weapon == Constants.mzChaingun3 -> do
           r <- Lib.rand
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlRadius = 250 + fromIntegral (r .&. 31)
                                            , _cdlColor = V3 1 1 0
                                            , _cdlDie = fromIntegral time + 0.1 -- long delay
                                            })
           r' <- Lib.rand
           let soundName = "weapons/machgf" `B.append` BC.pack (show ((r' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx <- S.registerSound soundName
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

           r'' <- Lib.rand
           let soundName' = "weapons/machgf" `B.append` BC.pack (show ((r'' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx' <- S.registerSound soundName'
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx' volume Constants.attnNorm 0.033

           r''' <- Lib.rand
           let soundName'' = "weapons/machgf" `B.append` BC.pack (show ((r''' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx'' <- S.registerSound soundName''
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx'' volume Constants.attnNorm 0.066

       | weapon == Constants.mzRailgun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0.5 0.5 1 })
           soundIdx <- S.registerSound "weapons/railgf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzRocket -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.2 })
           soundIdx <- S.registerSound "weapons/rocklf1a.wav"
           soundIdx' <- S.registerSound "weapons/rocklr1b.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           S.startSound Nothing (newEdictReference i) Constants.chanAuto soundIdx' volume Constants.attnNorm 0.1

       | weapon == Constants.mzGrenade -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0 })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           soundIdx' <- S.registerSound "weapons/grenlr1b.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           S.startSound Nothing (newEdictReference i) Constants.chanAuto soundIdx' volume Constants.attnNorm 0.1

       | weapon == Constants.mzBFG -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 0 })
           soundIdx <- S.registerSound "weapons/bfg__f1y.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzLogin -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 0
                                            , _cdlDie = fromIntegral time + 1.0
                                            })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
           logoutEffect (pl^.ceCurrent.esOrigin) weapon

       | weapon == Constants.mzLogout -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0 0
                                            , _cdlDie = fromIntegral time + 1.0
                                            })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
           logoutEffect (pl^.ceCurrent.esOrigin) weapon

       | weapon == Constants.mzRespawn -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0
                                            , _cdlDie = fromIntegral time + 1.0
                                            })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
           logoutEffect (pl^.ceCurrent.esOrigin) weapon

       | weapon == Constants.mzPhalanx -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.5 })
           soundIdx <- S.registerSound "weapons/plasshot.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzIonRipper -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.5 })
           soundIdx <- S.registerSound "weapons/rippfire.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzEtfRifle -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0.9 0.7 0 })
           soundIdx <- S.registerSound "weapons/nail1.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzShotgun2 -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "weapons/shotg2.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzHeatBeam -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0
                                            , _cdlDie = fromIntegral time + 100
                                            })

       | weapon == Constants.mzBlaster2 -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 0 })
           -- FIXME: different sound for blaster2 ??
           soundIdx <- S.registerSound "weapons/blastf1a.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzTracker -> do
           -- negative flashes handled the same in gl/soft until CL_AddDLights
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 (-1) (-1) (-1) })
           soundIdx <- S.registerSound "weapons/disint2.wav"
           S.startSound Nothing (newEdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzNuke1 -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0 0
                                            , _cdlDie = fromIntegral time + 100
                                            })

       | weapon == Constants.mzNuke2 -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0
                                            , _cdlDie = fromIntegral time + 100
                                            })

       | weapon == Constants.mzNuke4 -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 0 1
                                            , _cdlDie = fromIntegral time + 100
                                            })

       | weapon == Constants.mzNuke8 -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 1
                                            , _cdlDie = fromIntegral time + 100
                                            })

       | otherwise -> return () -- TODO: some error should be thrown?

parseMuzzleFlash2 :: Quake ()
parseMuzzleFlash2 = do
    ent <- MSG.readShort (globals.netMessage)

    when (ent < 1 || ent >= Constants.maxEdicts) $
      Com.comError Constants.errDrop "CL_ParseMuzzleFlash2: bad entity"

    flashNumber <- MSG.readByte (globals.netMessage)

    -- locate the origin
    Just cent <- preuse $ globals.clEntities.ix ent
    let (Just forward, Just right, _) = Math3D.angleVectors (cent^.ceCurrent.esAngles) True True False
        a = (cent^.ceCurrent.esOrigin._x) + (forward^._x) * ((MFlash.monsterFlashOffset V.! flashNumber)^._x) + (right^._x) * ((MFlash.monsterFlashOffset V.! flashNumber)^._y)
        b = (cent^.ceCurrent.esOrigin._y) + (forward^._y) * ((MFlash.monsterFlashOffset V.! flashNumber)^._x) + (right^._y) * ((MFlash.monsterFlashOffset V.! flashNumber)^._y)
        c = (cent^.ceCurrent.esOrigin._z) + (forward^._z) * ((MFlash.monsterFlashOffset V.! flashNumber)^._x) + (right^._z) * ((MFlash.monsterFlashOffset V.! flashNumber)^._y) + ((MFlash.monsterFlashOffset V.! flashNumber)^._z)
        origin = V3 a b c

    dlRef <- allocDLight ent
    r <- Lib.rand
    time <- use $ globals.cl.csTime

    io $ modifyIORef' dlRef (\v -> v { _cdlOrigin = origin
                                     , _cdlRadius = 200 + fromIntegral (r .&. 31)
                                     , _cdlMinLight = 32
                                     , _cdlDie = fromIntegral time
                                     })

    if | flashNumber `elem` [ Constants.mz2InfantryMachinegun1
                            , Constants.mz2InfantryMachinegun2
                            , Constants.mz2InfantryMachinegun3
                            , Constants.mz2InfantryMachinegun4
                            , Constants.mz2InfantryMachinegun5
                            , Constants.mz2InfantryMachinegun6
                            , Constants.mz2InfantryMachinegun7
                            , Constants.mz2InfantryMachinegun8
                            , Constants.mz2InfantryMachinegun9
                            , Constants.mz2InfantryMachinegun10
                            , Constants.mz2InfantryMachinegun11
                            , Constants.mz2InfantryMachinegun12
                            , Constants.mz2InfantryMachinegun13
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin
           soundIdx <- S.registerSound "infantry/infatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2SoldierMachinegun1
                            , Constants.mz2SoldierMachinegun2
                            , Constants.mz2SoldierMachinegun3
                            , Constants.mz2SoldierMachinegun4
                            , Constants.mz2SoldierMachinegun5
                            , Constants.mz2SoldierMachinegun6
                            , Constants.mz2SoldierMachinegun7
                            , Constants.mz2SoldierMachinegun8
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin
           soundIdx <- S.registerSound "soldier/solatck3.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2GunnerMachinegun1
                            , Constants.mz2GunnerMachinegun2
                            , Constants.mz2GunnerMachinegun3
                            , Constants.mz2GunnerMachinegun4
                            , Constants.mz2GunnerMachinegun5
                            , Constants.mz2GunnerMachinegun6
                            , Constants.mz2GunnerMachinegun7
                            , Constants.mz2GunnerMachinegun8
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin
           soundIdx <- S.registerSound "gunner/gunatck2.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2ActorMachinegun1
                            , Constants.mz2SupertankMachinegun1
                            , Constants.mz2SupertankMachinegun2
                            , Constants.mz2SupertankMachinegun3
                            , Constants.mz2SupertankMachinegun4
                            , Constants.mz2SupertankMachinegun5
                            , Constants.mz2SupertankMachinegun6
                            , Constants.mz2TurretMachinegun
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin
           soundIdx <- S.registerSound "infantry/infatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2Boss2MachinegunL1
                            , Constants.mz2Boss2MachinegunL2
                            , Constants.mz2Boss2MachinegunL3
                            , Constants.mz2Boss2MachinegunL4
                            , Constants.mz2Boss2MachinegunL5
                            , Constants.mz2CarrierMachinegunL1
                            , Constants.mz2CarrierMachinegunL2
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin
           soundIdx <- S.registerSound "infantry/infatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNone 0

       | flashNumber `elem` [ Constants.mz2SoldierBlaster1
                            , Constants.mz2SoldierBlaster2
                            , Constants.mz2SoldierBlaster3
                            , Constants.mz2SoldierBlaster4
                            , Constants.mz2SoldierBlaster5
                            , Constants.mz2SoldierBlaster6
                            , Constants.mz2SoldierBlaster7
                            , Constants.mz2SoldierBlaster8
                            , Constants.mz2TurretBlaster
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "soldier/solatck2.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2FlyerBlaster1
                            , Constants.mz2FlyerBlaster2
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "flyer/flyatck3.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber == Constants.mz2MedicBlaster1 -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "medic/medatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber == Constants.mz2HoverBlaster1 -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "hover/hovatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber == Constants.mz2FloatBlaster1 -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "floater/fltatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2SoldierShotgun1
                            , Constants.mz2SoldierShotgun2
                            , Constants.mz2SoldierShotgun3
                            , Constants.mz2SoldierShotgun4
                            , Constants.mz2SoldierShotgun5
                            , Constants.mz2SoldierShotgun6
                            , Constants.mz2SoldierShotgun7
                            , Constants.mz2SoldierShotgun8
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           CLTEnt.smokeAndFlash origin
           soundIdx <- S.registerSound "soldier/solatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2TankBlaster1
                            , Constants.mz2TankBlaster2
                            , Constants.mz2TankBlaster3
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "tank/tnkatck3.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2TankMachinegun1
                            , Constants.mz2TankMachinegun2
                            , Constants.mz2TankMachinegun3
                            , Constants.mz2TankMachinegun4
                            , Constants.mz2TankMachinegun5
                            , Constants.mz2TankMachinegun6
                            , Constants.mz2TankMachinegun7
                            , Constants.mz2TankMachinegun8
                            , Constants.mz2TankMachinegun9
                            , Constants.mz2TankMachinegun10
                            , Constants.mz2TankMachinegun11
                            , Constants.mz2TankMachinegun12
                            , Constants.mz2TankMachinegun13
                            , Constants.mz2TankMachinegun14
                            , Constants.mz2TankMachinegun15
                            , Constants.mz2TankMachinegun16
                            , Constants.mz2TankMachinegun17
                            , Constants.mz2TankMachinegun18
                            , Constants.mz2TankMachinegun19
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin
           r <- Lib.rand
           let soundName = "tank/tnkatk2" `B.append` B.singleton (97 + fromIntegral (r `mod` 5)) `B.append` ".wav"
           soundIdx <- S.registerSound soundName
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2ChickRocket1
                            , Constants.mz2TurretRocket
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.2 })
           soundIdx <- S.registerSound "chick/chkatck2.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2TankRocket1
                            , Constants.mz2TankRocket2
                            , Constants.mz2TankRocket3
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.2 })
           soundIdx <- S.registerSound "tank/tnkatck1.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2SupertankRocket1
                            , Constants.mz2SupertankRocket2
                            , Constants.mz2SupertankRocket3
                            , Constants.mz2Boss2Rocket1
                            , Constants.mz2Boss2Rocket2
                            , Constants.mz2Boss2Rocket3
                            , Constants.mz2Boss2Rocket4
                            , Constants.mz2CarrierRocket1
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.2 })
           soundIdx <- S.registerSound "tank/rocket.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2GunnerGrenade1
                            , Constants.mz2GunnerGrenade2
                            , Constants.mz2GunnerGrenade3
                            , Constants.mz2GunnerGrenade4
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0 })
           soundIdx <- S.registerSound "gunner/gunatck3.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2GladiatorRailgun1
                            , Constants.mz2CarrierRailgun
                            , Constants.mz2WidowRail
                            ] ->
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0.5 0.5 1.0 })

       | flashNumber == Constants.mz2MakronBfg ->
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0.5 1 0.5 })

       | flashNumber `elem` [ Constants.mz2MakronBlaster1
                            , Constants.mz2MakronBlaster2
                            , Constants.mz2MakronBlaster3
                            , Constants.mz2MakronBlaster4
                            , Constants.mz2MakronBlaster5
                            , Constants.mz2MakronBlaster6
                            , Constants.mz2MakronBlaster7
                            , Constants.mz2MakronBlaster8
                            , Constants.mz2MakronBlaster9
                            , Constants.mz2MakronBlaster10
                            , Constants.mz2MakronBlaster11
                            , Constants.mz2MakronBlaster12
                            , Constants.mz2MakronBlaster13
                            , Constants.mz2MakronBlaster14
                            , Constants.mz2MakronBlaster15
                            , Constants.mz2MakronBlaster16
                            , Constants.mz2MakronBlaster17
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "makron/blaster.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2JorgMachinegunL1
                            , Constants.mz2JorgMachinegunL2
                            , Constants.mz2JorgMachinegunL3
                            , Constants.mz2JorgMachinegunL4
                            , Constants.mz2JorgMachinegunL5
                            , Constants.mz2JorgMachinegunL6
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin
           soundIdx <- S.registerSound "boss3/xfire.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2JorgMachinegunR1
                            , Constants.mz2JorgMachinegunR2
                            , Constants.mz2JorgMachinegunR3
                            , Constants.mz2JorgMachinegunR4
                            , Constants.mz2JorgMachinegunR5
                            , Constants.mz2JorgMachinegunR6
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin

       | flashNumber == Constants.mz2JorgBfg1 ->
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0.5 1 0.5 })

       | flashNumber `elem` [ Constants.mz2Boss2MachinegunR1
                            , Constants.mz2Boss2MachinegunR2
                            , Constants.mz2Boss2MachinegunR3
                            , Constants.mz2Boss2MachinegunR4
                            , Constants.mz2Boss2MachinegunR5
                            , Constants.mz2CarrierMachinegunR1
                            , Constants.mz2CarrierMachinegunR2
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           v3o <- use $ globals.vec3Origin
           particleEffect origin v3o 0 40
           CLTEnt.smokeAndFlash origin

       | flashNumber `elem` [ Constants.mz2StalkerBlaster
                            , Constants.mz2DaedalusBlaster
                            , Constants.mz2MedicBlaster2
                            , Constants.mz2WidowBlaster
                            , Constants.mz2WidowBlasterSweep1
                            , Constants.mz2WidowBlasterSweep2
                            , Constants.mz2WidowBlasterSweep3
                            , Constants.mz2WidowBlasterSweep4
                            , Constants.mz2WidowBlasterSweep5
                            , Constants.mz2WidowBlasterSweep6
                            , Constants.mz2WidowBlasterSweep7
                            , Constants.mz2WidowBlasterSweep8
                            , Constants.mz2WidowBlasterSweep9
                            , Constants.mz2WidowBlaster100
                            , Constants.mz2WidowBlaster90
                            , Constants.mz2WidowBlaster80
                            , Constants.mz2WidowBlaster70
                            , Constants.mz2WidowBlaster60
                            , Constants.mz2WidowBlaster50
                            , Constants.mz2WidowBlaster40
                            , Constants.mz2WidowBlaster30
                            , Constants.mz2WidowBlaster20
                            , Constants.mz2WidowBlaster10
                            , Constants.mz2WidowBlaster0
                            , Constants.mz2WidowBlaster10L
                            , Constants.mz2WidowBlaster20L
                            , Constants.mz2WidowBlaster30L
                            , Constants.mz2WidowBlaster40L
                            , Constants.mz2WidowBlaster50L
                            , Constants.mz2WidowBlaster60L
                            , Constants.mz2WidowBlaster70L
                            , Constants.mz2WidowRun1
                            , Constants.mz2WidowRun2
                            , Constants.mz2WidowRun3
                            , Constants.mz2WidowRun4
                            , Constants.mz2WidowRun5
                            , Constants.mz2WidowRun6
                            , Constants.mz2WidowRun7
                            , Constants.mz2WidowRun8
                            ] -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 0 })
           soundIdx <- S.registerSound "tank/tnkatck3.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber == Constants.mz2WidowDisruptor -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 (-1) (-1) (-1) })
           soundIdx <- S.registerSound "weapons/disint2.wav"
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0

       | flashNumber `elem` [ Constants.mz2WidowPlasmaBeam
                            , Constants.mz2Widow2Beamer1
                            , Constants.mz2Widow2Beamer2
                            , Constants.mz2Widow2Beamer3
                            , Constants.mz2Widow2Beamer4
                            , Constants.mz2Widow2Beamer5
                            , Constants.mz2Widow2BeamSweep1
                            , Constants.mz2Widow2BeamSweep2
                            , Constants.mz2Widow2BeamSweep3
                            , Constants.mz2Widow2BeamSweep4
                            , Constants.mz2Widow2BeamSweep5
                            , Constants.mz2Widow2BeamSweep6
                            , Constants.mz2Widow2BeamSweep7
                            , Constants.mz2Widow2BeamSweep8
                            , Constants.mz2Widow2BeamSweep9
                            , Constants.mz2Widow2BeamSweep10
                            , Constants.mz2Widow2BeamSweep11
                            ] -> do
           r <- Lib.rand
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0
                                            , _cdlRadius = 300 + fromIntegral (r .&. 100)
                                            , _cdlDie = fromIntegral time + 200
                                            })

       | otherwise -> do
           -- TODO: throw some kind of error ??
           return ()

allocDLight :: Int -> Quake (IORef CDLightT)
allocDLight key = do
    dLights <- use $ clientGlobals.cgDLights
    -- first look for an exact key match
    exactMatch <- io $ findExactMatch dLights 0 Constants.maxDLights

    case exactMatch of
      Just em -> return em

      Nothing -> do
        -- then look for anything else
        time <- use $ globals.cl.csTime
        anyMatch <- io $ findAnyDLight (fromIntegral time) dLights 0 Constants.maxDLights

        case anyMatch of
          Just am -> return am

          Nothing -> do
            io $ writeIORef (dLights V.! 0) newCDLightT { _cdlKey = key }
            return (dLights V.! 0)

  where findExactMatch :: V.Vector (IORef CDLightT) -> Int -> Int -> IO (Maybe (IORef CDLightT))
        findExactMatch dLights idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              let dlRef = dLights V.! idx
              dl <- readIORef dlRef

              if (dl^.cdlKey) == key
                then do
                  writeIORef dlRef newCDLightT { _cdlKey = key }
                  return (Just dlRef)
                else
                  findExactMatch dLights (idx + 1) maxIdx

        findAnyDLight :: Float -> V.Vector (IORef CDLightT) -> Int -> Int -> IO (Maybe (IORef CDLightT))
        findAnyDLight time dLights idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              let dlRef = dLights V.! idx
              dl <- readIORef dlRef

              if (dl^.cdlDie) < time
                then do
                  writeIORef dlRef newCDLightT { _cdlKey = key }
                  return (Just dlRef)
                else
                  findAnyDLight time dLights (idx + 1) maxIdx

logoutEffect :: V3 Float -> Int -> Quake ()
logoutEffect org pType = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addLogoutEffect freeParticles 0 500

  where addLogoutEffect :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addLogoutEffect Nothing _ _ = return ()
        addLogoutEffect (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              r <- Lib.rand
              f <- Lib.randomF
              o1 <- Lib.randomF
              o2 <- Lib.randomF
              o3 <- Lib.randomF
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              let r' = fromIntegral (r .&. 7)
                  color = if | pType == Constants.mzLogin -> 0xD0 + r' -- green
                             | pType == Constants.mzLogout -> 0x40 + r' -- red
                             | otherwise -> 0xE0 + r' -- yellow

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = color
                                              , _cpOrg = V3 ((org^._x) - 16 + o1 * 32) ((org^._y) - 16 + o2 * 32) ((org^._z) - 24 + o3 * 56)
                                              , _cpVel = fmap (* 20) (V3 v1 v2 v3)
                                              , _cpAccel = V3 0 0 (- particleGravity)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (1.0 + f * 0.3)
                                              })

              addLogoutEffect (p^.cpNext) (idx + 1) maxIdx

rocketTrail :: V3 Float -> V3 Float -> Int -> Quake ()
rocketTrail start end oldIdx = do
    -- smoke
    diminishingTrail start end oldIdx Constants.efRocket

    -- fire
    let move = start
        vec = end - start
        len = norm vec
        vec' = normalize vec

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addRocketTrail freeParticles vec' move len

  where addRocketTrail :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Quake ()
        addRocketTrail Nothing _ _ _ = return ()
        addRocketTrail (Just pRef) vec move len
          | len <= 0 = return ()
          | otherwise = do
              r <- Lib.rand

              if r .&. 7 == 0
                then do
                  p <- io $ readIORef pRef
                  clientGlobals.cgFreeParticles .= (p^.cpNext)
                  activeParticles <- use $ clientGlobals.cgActiveParticles
                  clientGlobals.cgActiveParticles .= Just pRef

                  time <- use $ globals.cl.csTime
                  r <- Lib.rand
                  f <- Lib.randomF
                  o1 <- Lib.crandom
                  o2 <- Lib.crandom
                  o3 <- Lib.crandom
                  v1 <- Lib.crandom
                  v2 <- Lib.crandom
                  v3 <- Lib.crandom

                  io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                                  , _cpAccel = V3 0 0 (- particleGravity)
                                                  , _cpTime = fromIntegral time
                                                  , _cpAlpha = 1.0
                                                  , _cpAlphaVel = (-1.0) / (1.0 + f * 0.2)
                                                  , _cpColor = 0xDC + fromIntegral (r .&. 3)
                                                  , _cpOrg = move + fmap (* 5) (V3 o1 o2 o3)
                                                  , _cpVel = fmap (* 20) (V3 v1 v2 v3)
                                                  })

                  addRocketTrail (p^.cpNext) vec (move + vec) (len - 1)

                else
                  addRocketTrail (Just pRef) vec (move + vec) (len - 1)

blasterTrail :: V3 Float -> V3 Float -> Quake ()
blasterTrail start end = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* 5) (normalize vec)

    trailParticles move vec' len

  where trailParticles :: V3 Float -> V3 Float -> Float -> Quake ()
        trailParticles move vec len
          | len <= 0 = return ()
          | otherwise = do
              freeParticles <- use $ clientGlobals.cgFreeParticles

              case freeParticles of
                Nothing ->
                  return ()

                Just pRef -> do
                  activeParticles <- use $ clientGlobals.cgActiveParticles
                  time <- use $ globals.cl.csTime

                  p <- io $ readIORef pRef
                  clientGlobals.cgFreeParticles .= (p^.cpNext)
                  io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles })
                  clientGlobals.cgActiveParticles .= Just pRef

                  f <- Lib.randomF
                  o1 <- Lib.crandom
                  o2 <- Lib.crandom
                  o3 <- Lib.crandom
                  v1 <- Lib.crandom
                  v2 <- Lib.crandom
                  v3 <- Lib.crandom

                  io $ modifyIORef' pRef (\v -> v { _cpTime = fromIntegral time
                                                  , _cpAlpha = 1.0
                                                  , _cpAlphaVel = (-1.0) / (0.3 + f * 0.2)
                                                  , _cpColor = 0xE0
                                                  , _cpOrg = move + V3 o1 o2 o3
                                                  , _cpVel = V3 (5 * v1) (5 * v2) (5 * v3)
                                                  , _cpAccel = V3 0 0 0
                                                  })

                  trailParticles (move + vec) vec (len - 5)

-- TODO: oldIdx should be CEntityReference
diminishingTrail :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
diminishingTrail start end oldIdx flags= do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* 0.5) (normalize vec)

    Just old <- preuse $ globals.clEntities.ix oldIdx

    let (orgScale, velScale) = if | (old^.ceTrailCount) > 900 -> (4, 15)
                                  | (old^.ceTrailCount) > 800 -> (2, 10)
                                  | otherwise -> (1, 5)

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addDiminishingTrail freeParticles vec' move orgScale velScale len

  where addDiminishingTrail :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Float -> Float -> Quake ()
        addDiminishingTrail Nothing _ _ _ _ _ = return ()
        addDiminishingTrail (Just pRef) vec move orgScale velScale len
          | len <= 0 = return ()
          | otherwise = do
              r <- Lib.rand
              Just old <- preuse $ globals.clEntities.ix oldIdx

              -- drop less particles as it flies
              if fromIntegral (r .&. 1023) < (old^.ceTrailCount)
                then do
                  p <- io $ readIORef pRef
                  clientGlobals.cgFreeParticles .= (p^.cpNext)
                  activeParticles <- use $ clientGlobals.cgActiveParticles
                  clientGlobals.cgActiveParticles .= Just pRef
                  
                  time <- use $ globals.cl.csTime
                  f <- Lib.randomF
                  color' <- Lib.rand
                  o1 <- Lib.crandom
                  o2 <- Lib.crandom
                  o3 <- Lib.crandom
                  v1 <- Lib.crandom
                  v2 <- Lib.crandom
                  v3 <- Lib.crandom

                  if | flags .&. Constants.efGib /= 0 -> do
                         io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                                         , _cpTime = fromIntegral time
                                                         , _cpAccel = V3 0 0 0
                                                         , _cpAlpha = 1.0
                                                         , _cpAlphaVel = (-1.0) / (1.0 + f * 0.4)
                                                         , _cpColor = 0xE8 + fromIntegral (color' .&. 7)
                                                         , _cpOrg = move + fmap (* orgScale) (V3 o1 o2 o3)
                                                         , _cpVel = (fmap (* velScale) (V3 v1 v2 v3)) & _z -~ particleGravity
                                                         })

                     | flags .&. Constants.efGreenGib /= 0 -> do
                         io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                                         , _cpTime = fromIntegral time
                                                         , _cpAccel = V3 0 0 0
                                                         , _cpAlpha = 1.0
                                                         , _cpAlphaVel = (-1.0) / (1.0 + f * 0.4)
                                                         , _cpColor = 0xDB + fromIntegral (color' .&. 7)
                                                         , _cpOrg = move + fmap (* orgScale) (V3 o1 o2 o3)
                                                         , _cpVel = (fmap (* velScale) (V3 v1 v2 v3)) & _z -~ particleGravity
                                                         })

                     | otherwise -> do
                         io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                                         , _cpTime = fromIntegral time
                                                         , _cpAccel = V3 0 0 20
                                                         , _cpAlpha = 1.0
                                                         , _cpAlphaVel = (-1.0) / (1.0 + f * 0.2)
                                                         , _cpColor = 4 + fromIntegral (color' .&. 7)
                                                         , _cpOrg = move + fmap (* orgScale) (V3 o1 o2 o3)
                                                         , _cpVel = fmap (* velScale) (V3 v1 v2 v3)
                                                         })

                  globals.clEntities.ix oldIdx.ceTrailCount %= (\v -> if v - 5 < 100 then 100 else v - 5)
                  addDiminishingTrail (p^.cpNext) vec (move + vec) orgScale velScale (len - 0.5)

                else do
                  globals.clEntities.ix oldIdx.ceTrailCount %= (\v -> if v - 5 < 100 then 100 else v - 5)
                  addDiminishingTrail (Just pRef) vec (move + vec) orgScale velScale (len - 0.5)

-- TODO: entIdx should be CEntityReference
flyEffect :: Int -> V3 Float -> Quake ()
flyEffect entIdx origin = do
    Just ent <- preuse $ globals.clEntities.ix entIdx
    time <- use $ globals.cl.csTime

    let (startTime, flyStopTime) = if (ent^.ceFlyStopTime) < time
                                     then (time, time + 60000)
                                     else ((ent^.ceFlyStopTime) - 60000, ent^.ceFlyStopTime)
        n = time - startTime
        count = if n < 20000
                  then (n * 162) `div` 20000
                  else let n' = flyStopTime - time
                       in if n' < 20000
                            then (n' * 162) `div` 20000
                            else 162

    globals.clEntities.ix entIdx.ceFlyStopTime .= flyStopTime

    flyParticles origin count

flyParticles :: V3 Float -> Int -> Quake ()
flyParticles origin count = do
    let count' = if count > Constants.numVertexNormals
                   then Constants.numVertexNormals
                   else count

    Just avelocity <- preuse $ clientGlobals.cgAVelocities.ix 0

    when ((avelocity^._x) == 0) $ do
      aVelocities <- V.replicateM Constants.numVertexNormals genAVelocity
      clientGlobals.cgAVelocities .= aVelocities

    time <- use $ globals.cl.csTime
    let lTime  = fromIntegral time / 1000

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addFlyParticles freeParticles lTime 0 count'

  where genAVelocity :: Quake (V3 Float)
        genAVelocity = do
          a1 <- Lib.rand
          a2 <- Lib.rand
          a3 <- Lib.rand
          return (V3 (fromIntegral (a1 .&. 255) * 0.01) (fromIntegral (a2 .&. 255) * 0.01) (fromIntegral (a3 .&. 255) * 0.01))

        addFlyParticles :: Maybe (IORef CParticleT) -> Float -> Int -> Int -> Quake ()
        addFlyParticles Nothing _ _ _ = return ()
        addFlyParticles (Just pRef) lTime idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              io (putStrLn "CLFX.flyParticles") >> undefined -- TODO

flagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
flagTrail start end color = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* 5) (normalize vec)

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addFlagTrail freeParticles vec' move len

  where addFlagTrail :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Quake ()
        addFlagTrail Nothing _ _ _ = return ()
        addFlagTrail (Just pRef) vec move len
          | len <= 0 = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              f <- Lib.randomF
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.8 + f * 0.2)
                                              , _cpColor = color
                                              , _cpOrg = move + fmap (* 16) (V3 o1 o2 o3)
                                              , _cpVel = fmap (* 5) (V3 v1 v2 v3)
                                              })

              addFlagTrail (p^.cpNext) vec (move + vec) (len - 5)

bfgParticles :: EntityT -> Quake ()
bfgParticles _ = do
    io (putStrLn "CLFX.bfgParticles") >> undefined -- TODO

trapParticles :: EntityT -> Quake ()
trapParticles _ = do
    io (putStrLn "CLFX.trapParticles") >> undefined -- TODO

ionRipperTrail :: V3 Float -> V3 Float -> Quake ()
ionRipperTrail _ _ = do
    io (putStrLn "CLFX.ionRipperTrail") >> undefined -- TODO
