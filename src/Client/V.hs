{-# LANGUAGE OverloadedStrings #-}
module Client.V where

import Control.Lens (use, (^.), (.=), (+=), zoom, ix, preuse, (%=))
import Control.Monad (void, unless, liftM, when)
import Data.Bits ((.|.), shiftL, shiftR, (.&.))
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef')
import Data.Maybe (isJust, fromJust)
import Linear (V3(..), V4(..), _x, _y, _z)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV

import Game.PlayerStateT
import Client.ClientInfoT
import Client.FrameT
import Types
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLEnts as CLEnts
import qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Sys.Timer as Timer
import qualified Util.Math3D as Math3D

gunNextF :: XCommandT
gunNextF =
  XCommandT "V.gunNextF" (do
    globals.gGunFrame += 1
    gunFrame' <- use $ globals.gGunFrame
    Com.printf ("frame " `B.append` BC.pack (show gunFrame') `B.append` "\n") -- IMPROVE
  )

gunPrevF :: XCommandT
gunPrevF =
  XCommandT "V.gunPrevF" (do
    globals.gGunFrame %= (\v -> if v - 1 < 0 then 0 else v - 1)
    gunFrame' <- use $ globals.gGunFrame
    Com.printf ("frame " `B.append` BC.pack (show gunFrame') `B.append` "\n") -- IMPROVE
  )

gunModelF :: XCommandT
gunModelF =
  XCommandT "V.gunModelF" (do
    c <- Cmd.argc

    if c /= 2
      then
        globals.gGunModel .= Nothing

      else do
        v <- Cmd.argv 1
        Just renderer <- use $ globals.gRenderer

        let name = "models/" `B.append` v `B.append` "/tris.md2"
            registerModel = renderer^.rRefExport.reRegisterModel

        registerModel name >>= (globals.gGunModel .=)
  )

viewPosF :: XCommandT
viewPosF =
  XCommandT "V.viewPosF" (do
    refDef <- use $ globals.gCl.csRefDef
    let line = printf "(%i %i %i) : %i\n" (truncate $ refDef^.rdViewOrg._x :: Int) (truncate $ refDef^.rdViewOrg._y :: Int) (truncate $ refDef^.rdViewOrg._z :: Int) (truncate $ refDef^.rdViewAngles._y :: Int) -- IMPROVE: use yaw instead of _y
    Com.printf (BC.pack line)
  )

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
      frameValid <- use $ globals.gCl.csFrame.fValid
      forceRefDef <- use $ globals.gCl.csForceRefDef
      pausedValue <- liftM (^.cvValue) clPausedCVar

      when (frameValid && (forceRefDef || pausedValue == 0)) $ do
        globals.gCl.csForceRefDef .= False

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
            globals.gCl.csRefDef.rdBlend .= V4 1.0 0.5 0.25 0.5

        -- offset vieworg appropriately if we're doing stereo separation
        when (stereoSeparation /= 0) $ do
          vright <- use $ globals.gCl.csVRight
          let tmp = fmap (* stereoSeparation) vright
          globals.gCl.csRefDef.rdViewOrg += tmp

        -- never let it sit exactly on a node line, because a water plane
        -- can dissapear when viewed with the eye exactly on it.
        -- the server protocol only specifies to 1/8 pixel, so add 1/16 in
        -- each axis
        vrect <- use $ globals.gScrVRect
        cl' <- use $ globals.gCl

        zoom (globals.gCl.csRefDef) $ do
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
            globals.gCl.csRefDef.rdBlend .= V4 0 0 0 0

        use (vGlobals.vgNumEntities) >>= \v ->
          globals.gCl.csRefDef.rdNumEntities .= v

        use (vGlobals.vgNumParticles) >>= \v ->
          globals.gCl.csRefDef.rdNumParticles .= v

        use (vGlobals.vgNumDLights) >>= \v ->
          globals.gCl.csRefDef.rdNumDLights .= v

        use (vGlobals.vgEntities) >>= \v ->
          globals.gCl.csRefDef.rdEntities .= v

        use (vGlobals.vgDLights) >>= \v ->
          globals.gCl.csRefDef.rdDLights .= v

        use (vGlobals.vgLightStyles) >>= \v ->
          globals.gCl.csRefDef.rdLightStyles .= v

        globals.gCl.csRefDef.rdRdFlags .= (cl'^.csFrame.fPlayerState.psRDFlags)

      Just renderer <- use $ globals.gRenderer
      use (globals.gCl.csRefDef) >>= \rd ->
        (renderer^.rRefExport.reRenderFrame) rd

      clStatsCVar >>= \c ->
        when ((c^.cvValue) /= 0) $
          Com.printf $ "ent: ??? lt: ??? part: ???" -- TODO: add info here!

      logFile <- use $ globals.gLogStatsFile
      logStatsCVar >>= \c ->
        when ((c^.cvValue) /= 0 && isJust logFile) $
          io (putStrLn "V.renderView: implement me!")

      finishRenderView
    
  where checkIfShouldSkip :: Quake Bool
        checkIfShouldSkip = do
          state <- use $ globals.gCls.csState
          refreshPrepped <- use $ globals.gCl.csRefreshPrepped
                                                  -- still loading
          return $ state /= Constants.caActive || not refreshPrepped

        checkTimeDemo :: Quake ()
        checkTimeDemo = do
          timeDemoValue <- liftM (^.cvValue) clTimeDemoCVar

          when (timeDemoValue /= 0) $ do
            timeDemoStart <- use $ globals.gCl.csTimeDemoStart

            when (timeDemoStart == 0) $ do
              ms <- Timer.milliseconds
              globals.gCl.csTimeDemoStart .= ms

            globals.gCl.csTimeDemoFrames += 1

        finishRenderView :: Quake ()
        finishRenderView = do
          vrect <- use $ globals.gScrVRect
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
              cl' <- use $ globals.gCl

              let d = fromIntegral idx * 0.25
                  r = 4 * (fromIntegral (idx .&. 7) - 3.5)
                  u = 4 * (fromIntegral ((idx `shiftR` 3) .&. 7) - 3.5)
                  origin = (cl'^.csRefDef.rdViewOrg) + fmap (* d) (cl'^.csVForward) + fmap (* r) (cl'^.csVRight) + fmap (* u) (cl'^.csVUp)

              addParticle origin 8 testParticlesValue
              addTestParticles testParticlesValue (idx + 1) maxIdx

{-
- ================ V_TestEntities ================
- 
- If cl_testentities is set, create 32 player models
-}
testEntities :: Quake ()
testEntities = do
    vGlobals.vgNumEntities .= 32
    entities <- use $ vGlobals.vgEntities
    V.mapM_ (\ref -> io $ writeIORef ref newEntityT) entities

    addTestEntities entities 0 32

  where addTestEntities :: V.Vector (IORef EntityT) -> Int -> Int -> Quake ()
        addTestEntities entities idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              cl' <- use $ globals.gCl

              let ref = entities V.! idx
                  r = 64 * (fromIntegral (idx `mod` 4) - 1.5)
                  f = 64 * (fromIntegral idx / 4) + 128
                  origin = (cl'^.csRefDef.rdViewOrg) + fmap (* f) (cl'^.csVForward) + fmap (* r) (cl'^.csVRight)

              io $ modifyIORef' ref (\v -> v { _eModel = (cl'^.csBaseClientInfo.ciModel)
                                             , _eSkin = (cl'^.csBaseClientInfo.ciSkin)
                                             , _eOrigin = origin
                                             })

              addTestEntities entities (idx + 1) maxIdx

{-
- ================ V_TestLights ================
- 
- If cl_testlights is set, create 32 lights models
-}
testLights :: Quake ()
testLights = do
    vGlobals.vgNumDLights .= 32
    vGlobals.vgDLights %= fmap (const newDLightT)

    addTestLights 0 32

  where addTestLights :: Int -> Int -> Quake ()
        addTestLights idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              cl' <- use $ globals.gCl

              let r = 64 * (fromIntegral (idx `mod` 4) - 1.5)
                  f = 64 * (fromIntegral idx / 4) + 128
                  origin = (cl'^.csRefDef.rdViewOrg) + fmap (* f) (cl'^.csVForward) + fmap (* r) (cl'^.csVRight)
                  c1 = ((idx `mod` 6) + 1) .&. 1
                  c2 = (((idx `mod` 6) + 1) .&. 2) `shiftR` 1
                  c3 = (((idx `mod` 6) + 1) .&. 4) `shiftR` 2

              vGlobals.vgDLights.ix idx .= newDLightT { _dlOrigin = origin
                                                      , _dlColor = fmap fromIntegral (V3 c1 c2 c3)
                                                      , _dlIntensity = 200
                                                      }

              addTestLights (idx + 1) maxIdx

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
