{-# LANGUAGE MultiWayIf #-}
module Client.CLFX where

-- Client Graphics Effects

import Control.Lens ((.=), ix, use, (^.))
import Control.Monad (unless)
import Linear (V3(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified Constants

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
setLightStyle _ = do
    io (putStrLn "CLFX.setLightStyle") >> undefined -- TODO
