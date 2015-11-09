{-# LANGUAGE OverloadedStrings #-}
module Client.V where

import Control.Lens (use, (^.), (.=), (+=), zoom, ix, preuse, (%=))
import Control.Monad (void, unless, liftM, when)
import Data.Bits ((.|.), shiftL, shiftR, (.&.))
import Data.IORef (IORef, readIORef)
import Data.Maybe (isJust, fromJust)
import Linear (V3(..), V4(..), _x, _y, _z)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLEnts as CLEnts
import qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Sys.Timer as Timer
import qualified Util.Math3D as Math3D

gunNextF :: XCommandT
gunNextF = do
    globals.gunFrame += 1
    gunFrame' <- use $ globals.gunFrame
    Com.printf ("frame " `B.append` BC.pack (show gunFrame') `B.append` "\n") -- IMPROVE

gunPrevF :: XCommandT
gunPrevF = do
    globals.gunFrame %= (\v -> if v - 1 < 0 then 0 else v - 1)
    gunFrame' <- use $ globals.gunFrame
    Com.printf ("frame " `B.append` BC.pack (show gunFrame') `B.append` "\n") -- IMPROVE

gunModelF :: XCommandT
gunModelF = do
    c <- Cmd.argc

    if c /= 2
      then
        globals.gunModel .= Nothing

      else do
        v <- Cmd.argv 1
        Just renderer <- use $ globals.re

        let name = "models/" `B.append` v `B.append` "/tris.md2"
            registerModel = renderer^.rRefExport.reRegisterModel

        registerModel name >>= (globals.gunModel .=)

viewPosF :: XCommandT
viewPosF = do
    refDef <- use $ globals.cl.csRefDef
    let line = printf "(%i %i %i) : %i\n" (truncate $ refDef^.rdViewOrg._x :: Int) (truncate $ refDef^.rdViewOrg._y :: Int) (truncate $ refDef^.rdViewOrg._z :: Int) (truncate $ refDef^.rdViewAngles._y :: Int) -- IMPROVE: use yaw instead of _y
    Com.printf (BC.pack line)

init :: Quake ()
init = do
    Cmd.addCommand "gun_next" (Just gunNextF)
    Cmd.addCommand "gun_prev" (Just gunPrevF)
    Cmd.addCommand "gun_model" (Just gunModelF)

    Cmd.addCommand "viewpos" (Just viewPosF)

    void $ CVar.get "crosshair" "0" Constants.cvarArchive

    void $ CVar.get "cl_testblend" "0" 0
    void $ CVar.get "cl_testparticles" "0" 0
    void $ CVar.get "cl_testentities" "0" 0
    void $ CVar.get "cl_testlights" "0" 0

    void $ CVar.get "cl_stats" "0" 0

renderView :: Float -> Quake ()
renderView stereoSeparation = do
    shouldSkip <- checkIfShouldSkip

    unless shouldSkip $ do
      checkTimeDemo

      -- an invalid frame will just use the exact previous refdef
      -- we can't use the old frame if the video mode has changed, though...
      frameValid <- use $ globals.cl.csFrame.fValid
      forceRefDef <- use $ globals.cl.csForceRefDef
      pausedValue <- liftM (^.cvValue) clPausedCVar

      when (frameValid && (forceRefDef || pausedValue == 0)) $ do
        globals.cl.csForceRefDef .= False

        clearScene

        -- build a refresh entity list and calc cl.sim*
        -- this also calls CL_CalcViewValues which loads
        -- v_forward, etc.
        CLEnts.addEntities

        clTestParticlesCVar >>= \c ->
          when ((c^.cvValue) /= 0)
            testParticles

        clTestEntitiesCVar >>= \c ->
          when ((c^.cvValue) /= 0)
            testEntities

        clTestLightsCVar >>= \c ->
          when ((c^.cvValue) /= 0)
            testLights

        clTestBlendCVar >>= \c ->
          when ((c^.cvValue) /= 0) $
            globals.cl.csRefDef.rdBlend .= V4 1.0 0.5 0.25 0.5

        -- offset vieworg appropriately if we're doing stereo separation
        when (stereoSeparation /= 0) $ do
          vright <- use $ globals.cl.csVRight
          let tmp = fmap (* stereoSeparation) vright
          globals.cl.csRefDef.rdViewOrg += tmp

        -- never let it sit exactly on a node line, because a water plane
        -- can dissapear when viewed with the eye exactly on it.
        -- the server protocol only specifies to 1/8 pixel, so add 1/16 in
        -- each axis
        vrect <- use $ globals.scrVRect
        cl' <- use $ globals.cl

        zoom (globals.cl.csRefDef) $ do
          rdViewOrg += (V3 (1.0 / 16) (1.0 / 16) (1.0 / 16))

          rdX .= (vrect^.vrX)
          rdY .= (vrect^.vrY)
          rdWidth .= (vrect^.vrWidth)
          rdHeight .= (vrect^.vrHeight)
          rdFovY .= Math3D.calcFov (cl'^.csRefDef.rdFovX) (fromIntegral $ vrect^.vrWidth) (fromIntegral $ vrect^.vrHeight)
          rdTime .= (fromIntegral $ cl'^.csTime) * 0.001

          rdAreaBits .= (cl'^.csFrame.fAreaBits)

        clAddEntitiesCVar >>= \c ->
          when ((c^.cvValue) == 0) $
            vGlobals.vgNumEntities .= 0

        clAddParticlesCVar >>= \c ->
          when ((c^.cvValue) == 0) $
            vGlobals.vgNumParticles .= 0

        clAddLightsCVar >>= \c ->
          when ((c^.cvValue) == 0) $
            vGlobals.vgNumDLights .= 0

        clAddBlendCVar >>= \c ->
          when ((c^.cvValue) == 0) $
            globals.cl.csRefDef.rdBlend .= V4 0 0 0 0

        use (vGlobals.vgNumEntities) >>= \v ->
          globals.cl.csRefDef.rdNumEntities .= v

        use (vGlobals.vgNumParticles) >>= \v ->
          globals.cl.csRefDef.rdNumParticles .= v

        use (vGlobals.vgNumDLights) >>= \v ->
          globals.cl.csRefDef.rdNumDLights .= v

        use (vGlobals.vgEntities) >>= \v ->
          globals.cl.csRefDef.rdEntities .= v

        use (vGlobals.vgDLights) >>= \v ->
          globals.cl.csRefDef.rdDLights .= v

        use (vGlobals.vgLightStyles) >>= \v ->
          globals.cl.csRefDef.rdLightStyles .= v

        globals.cl.csRefDef.rdRdFlags .= (cl'^.csFrame.fPlayerState.psRDFlags)

      Just renderer <- use $ globals.re
      use (globals.cl.csRefDef) >>= \rd ->
        (renderer^.rRefExport.reRenderFrame) rd

      clStatsCVar >>= \c ->
        when ((c^.cvValue) /= 0) $
          Com.printf $ "ent: ??? lt: ??? part: ???" -- TODO: add info here!

      logFile <- use $ globals.logStatsFile
      logStatsCVar >>= \c ->
        when ((c^.cvValue) /= 0 && isJust logFile) $
          io (putStrLn "V.renderView: implement me!")

      finishRenderView
    
  where checkIfShouldSkip :: Quake Bool
        checkIfShouldSkip = do
          state <- use $ globals.cls.csState
          refreshPrepped <- use $ globals.cl.csRefreshPrepped
                                                  -- still loading
          return $ state /= Constants.caActive || not refreshPrepped

        checkTimeDemo :: Quake ()
        checkTimeDemo = do
          timeDemoValue <- liftM (^.cvValue) clTimeDemoCVar

          when (timeDemoValue /= 0) $ do
            timeDemoStart <- use $ globals.cl.csTimeDemoStart

            when (timeDemoStart == 0) $ do
              ms <- Timer.milliseconds
              globals.cl.csTimeDemoStart .= ms

            globals.cl.csTimeDemoFrames += 1

        finishRenderView :: Quake ()
        finishRenderView = do
          vrect <- use $ globals.scrVRect
          SCR.addDirtyPoint (vrect^.vrX) (vrect^.vrY)
          SCR.addDirtyPoint ((vrect^.vrX) + (vrect^.vrWidth) - 1) ((vrect^.vrY) + (vrect^.vrHeight) - 1)
          SCR.drawCrosshair

{-
- ==================== V_ClearScene
- 
- Specifies the model that will be used as the world ====================
-}
clearScene :: Quake ()
clearScene = do
    vGlobals.vgNumDLights .= 0
    vGlobals.vgNumEntities .= 0
    vGlobals.vgNumParticles .= 0

{-
- ================ V_TestParticles ================
- 
- If cl_testparticles is set, create 4096 particles in the view
-
-}
testParticles :: Quake ()
testParticles = do
    vGlobals.vgNumParticles .= 0
    testParticlesValue <- liftM (^.cvValue) clTestParticlesCVar
    addTestParticles testParticlesValue 0 Constants.maxParticles

  where addTestParticles :: Float -> Int -> Int -> Quake ()
        addTestParticles testParticlesValue idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              cl' <- use $ globals.cl

              let d = fromIntegral idx * 0.25
                  r = 4 * (fromIntegral (idx .&. 7) - 3.5)
                  u = 4 * (fromIntegral ((idx `shiftR` 3) .&. 7) - 3.5)
                  origin = (cl'^.csRefDef.rdViewOrg) + fmap (* d) (cl'^.csVForward) + fmap (* r) (cl'^.csVRight) + fmap (* u) (cl'^.csVUp)

              addParticle origin 8 testParticlesValue
              addTestParticles testParticlesValue (idx + 1) maxIdx

testEntities :: Quake ()
testEntities = do
    io (putStrLn "V.testEntities") >> undefined -- TODO

testLights :: Quake ()
testLights = do
    io (putStrLn "V.testLights") >> undefined -- TODO

addParticle :: V3 Float -> Int -> Float -> Quake ()
addParticle org color alpha = do
    numParticles <- use $ vGlobals.vgNumParticles

    unless (numParticles >= Constants.maxParticles) $ do
      vGlobals.vgNumParticles += 1
      colorTable <- use $ particleTGlobals.pColorTable
      colorArray <- use $ particleTGlobals.pColorArray
      vertexArray <- use $ particleTGlobals.pVertexArray

      let c = (colorTable UV.! color) .|. (truncate (alpha * 255) `shiftL` 24)
          i = numParticles * 3

      io $ do
        MSV.write colorArray numParticles c
        MSV.write vertexArray (i + 0) (org^._x)
        MSV.write vertexArray (i + 1) (org^._y)
        MSV.write vertexArray (i + 2) (org^._z)

addEntity :: IORef EntityT -> Quake ()
addEntity entRef = do
    numEntities <- use $ vGlobals.vgNumEntities
    when (numEntities < Constants.maxEntities) $ do
      vGlobals.vgEntities.ix numEntities .= entRef
      vGlobals.vgNumEntities += 1

addLight :: V3 Float -> Float -> Float -> Float -> Float -> Quake ()
addLight org intensity r g b = do
    numDLights <- use $ vGlobals.vgNumDLights

    unless (numDLights >= Constants.maxDLights) $ do
      vGlobals.vgDLights.ix numDLights .= DLightT org (V3 r g b) intensity
      vGlobals.vgNumDLights += 1

addLightStyle :: Int -> Float -> Float -> Float -> Quake ()
addLightStyle style r g b = do
    when (style < 0 || style > Constants.maxLightStyles) $
      Com.comError Constants.errDrop ("Bad light style " `B.append` BC.pack (show style)) -- IMPROVE?

    vGlobals.vgLightStyles.ix style .= LightStyleT (V3 r g b) (r + g + b)
