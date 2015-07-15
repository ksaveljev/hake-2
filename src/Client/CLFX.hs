{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.CLFX where

-- Client Graphics Effects

import Control.Lens ((.=), ix, use, (^.), preuse, zoom)
import Control.Monad (unless, when)
import Data.Char (ord)
import Linear (V3(..), _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified Constants
import {-# SOURCE #-} qualified Client.V as ClientV
import qualified QCommon.Com as Com

instantParticle :: Float
instantParticle = -10000.0

runDLights :: Quake ()
runDLights = do
    time <- use $ globals.cl.csTime
    dLights <- use $ clientGlobals.cgDLights
    runDLight dLights (fromIntegral time) 0 Constants.maxDLights

  where runDLight :: V.Vector CDLightT -> Float -> Int -> Int -> Quake ()
        runDLight dLights time idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let dl = dLights V.! idx

              if | dl^.cdlRadius == 0 -> runDLight dLights time (idx + 1) maxIdx
                 | dl^.cdlDie < time -> clientGlobals.cgDLights.ix idx.cdlRadius .= 0
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
    clientGlobals.cgFreeParticles .= CParticleReference 0
    clientGlobals.cgActiveParticles .= Nothing

    particles <- use $ clientGlobals.cgParticles
    let particles' = V.imap (\idx p -> p { _cpNext = Just (CParticleReference (idx + 1))}) particles
        lastone = particles' V.! (Constants.maxParticles - 1)
        particles'' = particles' V.// [(Constants.maxParticles - 1, lastone { _cpNext = Nothing })]
    clientGlobals.cgParticles .= particles''

clearDLights :: Quake ()
clearDLights = clientGlobals.cgDLights .= V.replicate Constants.maxDLights newCDLightT

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

entityEvent :: EntityStateT -> Quake ()
entityEvent _ = do
    io (putStrLn "CLFX.entityEvent") >> undefined -- TODO

addParticles :: Quake ()
addParticles = do
    activeParticles <- use $ clientGlobals.cgActiveParticles
    cl' <- use $ globals.cl
    addParticle cl' activeParticles Nothing Nothing 0

  where addParticle :: ClientStateT -> Maybe CParticleReference -> Maybe CParticleReference -> Maybe CParticleReference -> Float -> Quake ()
        addParticle _ Nothing active _ _ =
          clientGlobals.cgActiveParticles .= active
        addParticle cl' (Just pRef@(CParticleReference particleIdx)) active tail time = do
          Just p <- preuse $ clientGlobals.cgParticles.ix particleIdx
          let next = p^.cpNext

          (done, time', alpha) <- if (p^.cpAlphaVel) /= instantParticle
                                    then do
                                      let time' = (fromIntegral (cl'^.csTime) - (p^.cpTime)) * 0.001
                                          alpha = (p^.cpAlpha) + time' * (p^.cpAlphaVel)

                                      if alpha <= 0 -- faded out
                                        then do
                                          freeParticles <- use $ clientGlobals.cgFreeParticles
                                          clientGlobals.cgParticles.ix particleIdx.cpNext .= Just freeParticles
                                          clientGlobals.cgFreeParticles .= pRef
                                          return (True, time', alpha)
                                        else
                                          return (False, time', alpha)
                                    else
                                      return (False, time, p^.cpAlpha)

          if done
            then
              addParticle cl' next active tail time'
            else do
              clientGlobals.cgParticles.ix particleIdx.cpNext .= Nothing
              (active', tail') <- case tail of
                                    Nothing ->
                                      return (Just pRef, Just pRef)
                                    Just (CParticleReference idx) -> do
                                      clientGlobals.cgParticles.ix idx.cpNext .= Just pRef
                                      return (active, Just pRef)

              let alpha' = if alpha > 1 then 1 else alpha
                  color = truncate (p^.cpColor) :: Int
                  time2 = time' * time'
                  org = (p^.cpOrg) + fmap (* time') (p^.cpVel) + fmap (* time2) (p^.cpAccel)

              ClientV.addParticle org color alpha'

              when ((p^.cpAlphaVel) == instantParticle) $
                zoom (clientGlobals.cgParticles.ix particleIdx) $ do
                  cpAlphaVel .= 0
                  cpAlpha .= 0

              addParticle cl' next active' tail' time'

addDLights :: Quake ()
addDLights = do
    -- TODO: currently simplified version... need to update it to reflect
    -- jake2 version correctly
    dlights <- use $ clientGlobals.cgDLights
    addDLight dlights 0 Constants.maxDLights

  where addDLight :: V.Vector CDLightT -> Int -> Int -> Quake ()
        addDLight dlights idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let dl = dlights V.! idx
              
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
