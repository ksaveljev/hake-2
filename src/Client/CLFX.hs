{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.CLFX where

-- Client Graphics Effects

import Control.Lens ((.=), ix, use, (^.), preuse, zoom)
import Control.Monad (unless, when, liftM)
import Data.Bits ((.&.), complement)
import Data.Char (ord)
import Data.IORef (IORef, newIORef, modifyIORef', writeIORef, readIORef)
import Linear (V3(..), _x, _y, _z, norm, normalize)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Client.V as ClientV
import qualified QCommon.Com as Com
import qualified QCommon.MSG as MSG
import qualified Sound.S as S
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

instantParticle :: Float
instantParticle = -10000.0

particleGravity :: Int
particleGravity = 40

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
teleporterParticles _ = do
    io (putStrLn "CLFX.teleporterParticles") >> undefined -- TODO

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
           S.startSound Nothing (EdictReference (entityState^.esNumber)) Constants.chanWeapon sfx 1 Constants.attnIdle 0
           itemRespawnParticles (entityState^.esOrigin)

       | (entityState^.esEvent) == Constants.evPlayerTeleport -> do
           sfx <- S.registerSound "misc/tele1.wav"
           S.startSound Nothing (EdictReference (entityState^.esNumber)) Constants.chanWeapon sfx 1 Constants.attnIdle 0
           teleportParticles (entityState^.esOrigin)

       | (entityState^.esEvent) == Constants.evFootstep -> do
           footstepsValue <- liftM (^.cvValue) clFootstepsCVar
           when (footstepsValue /= 0) $ do
             r <- Lib.rand
             let idx = fromIntegral (r .&. 3)
             Just sfx <- preuse $ clTEntGlobals.clteSfxFootsteps.ix idx
             S.startSound Nothing (EdictReference (entityState^.esNumber)) Constants.chanBody sfx 1 Constants.attnNorm 0

       | (entityState^.esEvent) == Constants.evFallShort -> do
           sfx <- S.registerSound "player/land1.wav"
           S.startSound Nothing (EdictReference (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0

       | (entityState^.esEvent) == Constants.evFall -> do
           sfx <- S.registerSound "*fall2.wav"
           S.startSound Nothing (EdictReference (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0

       | (entityState^.esEvent) == Constants.evFallFar -> do
           sfx <- S.registerSound "*fall1.wav"
           S.startSound Nothing (EdictReference (entityState^.esNumber)) Constants.chanAuto sfx 1 Constants.attnNorm 0

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
itemRespawnParticles _ = do
    io (putStrLn "CLFX.itemRespawnParticles") >> undefined -- TODO

teleportParticles :: V3 Float -> Quake ()
teleportParticles _ = do
    io (putStrLn "CLFX.teleportParticles") >> undefined -- TODO
    
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
                  pAccel = fmap fromIntegral (V3 0 0 (negate particleGravity))
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
    io (putStrLn "CLFX.particleEffect2") >> undefined -- TODO

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
                  pAccel = fmap fromIntegral (V3 0 0 (negate particleGravity))
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

blasterParticles :: V3 Float -> V3 Float -> Quake ()
blasterParticles org dir = do
    io (putStrLn "CLFX.blasterParticles") >> undefined -- TODO

railTrail :: V3 Float -> V3 Float -> Quake ()
railTrail _ _ = do
    io (putStrLn "CLFX.railTrail") >> undefined -- TODO

bfgExplosionParticles :: V3 Float -> Quake ()
bfgExplosionParticles _ = do
    io (putStrLn "CLFX.bfgExplosionParticles") >> undefined -- TODO

bubbleTrail :: V3 Float -> V3 Float -> Quake ()
bubbleTrail _ _ = do
    io (putStrLn "CLFX.bubbleTrail") >> undefined -- TODO

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
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzBlueHyperblaster -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 0 1})
           soundIdx <- S.registerSound "weapons/hyprbf1a.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzHyperblaster -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0})
           soundIdx <- S.registerSound "weapons/hyprbf1a.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           
       | weapon == Constants.mzMachinegun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           r <- Lib.rand
           let soundName = "weapons/machgf" `B.append` BC.pack (show ((r `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE ?
           soundIdx <- S.registerSound soundName
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzShotgun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "weapons/shotgf1b.wav"
           soundIdx' <- S.registerSound "weapons/shotgr1b.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           S.startSound Nothing (EdictReference i) Constants.chanAuto soundIdx' volume Constants.attnNorm 0.1

       | weapon == Constants.mzSShotgun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "weapons/sshotf1b.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzChaingun1 -> do
           r <- Lib.rand
           io $ modifyIORef' dlRef (\v -> v { _cdlRadius = 200 + fromIntegral (r .&. 31)
                                            , _cdlColor = V3 1 0.25 0
                                            })
           r' <- Lib.rand
           let soundName = "weapons/machgf" `B.append` BC.pack (show ((r' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx <- S.registerSound soundName
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

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
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

           r'' <- Lib.rand
           let soundName' = "weapons/machgf" `B.append` BC.pack (show ((r'' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx' <- S.registerSound soundName'
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx' volume Constants.attnNorm 0.05

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
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

           r'' <- Lib.rand
           let soundName' = "weapons/machgf" `B.append` BC.pack (show ((r'' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx' <- S.registerSound soundName'
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx' volume Constants.attnNorm 0.033

           r''' <- Lib.rand
           let soundName'' = "weapons/machgf" `B.append` BC.pack (show ((r''' `mod` 5) + 1)) `B.append` "b.wav" -- IMPROVE?
           soundIdx'' <- S.registerSound soundName''
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx'' volume Constants.attnNorm 0.066

       | weapon == Constants.mzRailgun -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0.5 0.5 1 })
           soundIdx <- S.registerSound "weapons/railgf1a.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzRocket -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.2 })
           soundIdx <- S.registerSound "weapons/rocklf1a.wav"
           soundIdx' <- S.registerSound "weapons/rocklr1b.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           S.startSound Nothing (EdictReference i) Constants.chanAuto soundIdx' volume Constants.attnNorm 0.1

       | weapon == Constants.mzGrenade -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0 })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           soundIdx' <- S.registerSound "weapons/grenlr1b.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0
           S.startSound Nothing (EdictReference i) Constants.chanAuto soundIdx' volume Constants.attnNorm 0.1

       | weapon == Constants.mzBFG -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 0 })
           soundIdx <- S.registerSound "weapons/bfg__f1y.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzLogin -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 0
                                            , _cdlDie = fromIntegral time + 1.0
                                            })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
           logoutEffect (pl^.ceCurrent.esOrigin) weapon

       | weapon == Constants.mzLogout -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0 0
                                            , _cdlDie = fromIntegral time + 1.0
                                            })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
           logoutEffect (pl^.ceCurrent.esOrigin) weapon

       | weapon == Constants.mzRespawn -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0
                                            , _cdlDie = fromIntegral time + 1.0
                                            })
           soundIdx <- S.registerSound "weapons/grenlf1a.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx 1 Constants.attnNorm 0
           logoutEffect (pl^.ceCurrent.esOrigin) weapon

       | weapon == Constants.mzPhalanx -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.5 })
           soundIdx <- S.registerSound "weapons/plasshot.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzIonRipper -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 0.5 0.5 })
           soundIdx <- S.registerSound "weapons/rippfire.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzEtfRifle -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0.9 0.7 0 })
           soundIdx <- S.registerSound "weapons/nail1.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzShotgun2 -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0 })
           soundIdx <- S.registerSound "weapons/shotg2.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzHeatBeam -> do
           time <- use $ globals.cl.csTime
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 1 1 0
                                            , _cdlDie = fromIntegral time + 100
                                            })

       | weapon == Constants.mzBlaster2 -> do
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 0 1 0 })
           -- FIXME: different sound for blaster2 ??
           soundIdx <- S.registerSound "weapons/blastf1a.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

       | weapon == Constants.mzTracker -> do
           -- negative flashes handled the same in gl/soft until CL_AddDLights
           io $ modifyIORef' dlRef (\v -> v { _cdlColor = V3 (-1) (-1) (-1) })
           soundIdx <- S.registerSound "weapons/disint2.wav"
           S.startSound Nothing (EdictReference i) Constants.chanWeapon soundIdx volume Constants.attnNorm 0

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
    io (putStrLn "CLFX.parseMuzzleFlash2") >> undefined -- TODO

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
logoutEffect _ _ = do
    io (putStrLn "CLFX.logoutEffect") >> undefined -- TODO

rocketTrail :: V3 Float -> V3 Float -> Int -> Quake ()
rocketTrail _ _ _ = do
    io (putStrLn "CLFX.rocketTrail") >> undefined -- TODO

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

diminishingTrail :: V3 Float -> V3 Float -> Int -> Int -> Quake ()
diminishingTrail _ _ _ _ = do
    io (putStrLn "CLFX.diminishingTrail") >> undefined -- TODO

flyEffect :: Int -> V3 Float -> Quake ()
flyEffect _ _ = do
    io (putStrLn "CLFX.flyEffect") >> undefined -- TODO

flagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
flagTrail _ _ _ = do
    io (putStrLn "CLFX.flagTrail") >> undefined -- TODO

bfgParticles :: EntityT -> Quake ()
bfgParticles _ = do
    io (putStrLn "CLFX.bfgParticles") >> undefined -- TODO

trapParticles :: EntityT -> Quake ()
trapParticles _ = do
    io (putStrLn "CLFX.trapParticles") >> undefined -- TODO
