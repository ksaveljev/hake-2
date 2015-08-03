{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Mesh where

import Control.Lens ((^.), use, (+=), (.=))
import Control.Monad (when, liftM, unless)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Int (Int32)
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Maybe (fromJust)
import Data.Word (Word32)
import GHC.Float (float2Double)
import Linear (V3(..), _x, _y, _z, dot, normalize)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import CVarVariables
import Render.Anorms
import qualified Constants
import qualified Client.VID as VID
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Light as Light
import qualified Util.Math3D as Math3D

-- precalculated dot products for quantized angles
shadeDotQuant :: Int
shadeDotQuant = 16

colorArrayBuf :: MSV.IOVector Float
colorArrayBuf = unsafePerformIO $ MSV.new (Constants.maxVerts * 4)

textureArrayBuf :: MSV.IOVector Float
textureArrayBuf = unsafePerformIO $ MSV.new (Constants.maxVerts * 2)

rDrawAliasModel :: IORef EntityT -> Quake ()
rDrawAliasModel entRef = do
    e <- io $ readIORef entRef
    done <- checkIfDone e

    unless done $ do
      Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
      currentModel <- io $ readIORef currentModelRef

      let Just (AliasModelExtra pAliasHdr) = currentModel^.mExtraData

      -- get lighting information
      --
      -- PMM - rewrote, reordered to handle new shells & mixing
      -- PMM - 3.20 code .. replaced with original way of doing it to keep mod
      -- authors happy
      shadeLight <- buildShadeLight

      -- io $ print "SHADE LIGHT"
      -- io $ print shadeLight

      Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
      currentEntity <- io $ readIORef currentEntityRef

      let idx = truncate ((currentEntity^.eAngles._y) * (fromIntegral shadeDotQuant / 360)) .&. (shadeDotQuant - 1)
          shadeDots = vertexNormalDots V.! idx
          an = (currentEntity^.eAngles._y) / 180 * pi
          shadeVector = normalize (V3 (cos (-an)) (sin (-an)) 1)

      -- locate the proper data
      fastRenderAPIGlobals.frCAliasPolys += (pAliasHdr^.dmNumTris)

      -- draw all the triangles
      when ((currentEntity^.enFlags) .&. Constants.rfDepthHack /= 0) $ do
        -- hack the depth range to prevent view model
        -- from poking into walls
        depthMin <- use $ fastRenderAPIGlobals.frGLDepthMin
        depthMax <- use $ fastRenderAPIGlobals.frGLDepthMax
        GL.glDepthRange (realToFrac depthMin) (realToFrac $ depthMin + 0.3 * (depthMax - depthMin))

      handValue <- liftM (^.cvValue) handCVar

      when (((currentEntity^.enFlags) .&. Constants.rfWeaponModel /= 0) && handValue == 1) $ do
        newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef

        GL.glMatrixMode GL.gl_PROJECTION
        GL.glPushMatrix
        GL.glLoadIdentity
        GL.glScalef (-1) 1 1 
        myGLUPerspective (float2Double $ newRefDef^.rdFovY) (fromIntegral (newRefDef^.rdWidth) / fromIntegral (newRefDef^.rdWidth)) 4 4096
        GL.glMatrixMode GL.gl_MODELVIEW
        GL.glCullFace GL.gl_BACK

      GL.glPushMatrix
      rRotateForEntity e { _eAngles = (let V3 a b c = e^.eAngles in V3 (-a) b c) } -- TODO: jake2 uses PITCH instead of directly using _x

      let skinRef = case currentEntity^.eSkin of
                      Nothing -> if (currentEntity^.eSkinNum) >= Constants.maxMd2Skins
                                   then (currentModel^.mSkins) V.! 0
                                   else (currentModel^.mSkins) V.! (currentEntity^.eSkinNum) -- TODO: jake2 has some NULL check here as well
                      ref -> ref

      skinRef' <- case skinRef of
                    Nothing -> use $ fastRenderAPIGlobals.frNoTexture -- fallback...
                    Just ref -> return ref

      skin <- io $ readIORef skinRef'

      Image.glBind (skin^.iTexNum)

      -- draw it
      GL.glShadeModel GL.gl_SMOOTH

      Image.glTexEnv GL.gl_MODULATE
      when ((currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0) $
        GL.glEnable GL.gl_BLEND

      checkCurrentEntityFrame currentEntityRef pAliasHdr
      checkCurrentEntityOldFrame currentEntityRef pAliasHdr
      checkLerpModels currentEntityRef

      currentEntity' <- io $ readIORef currentEntityRef

      glDrawAliasFrameLerp pAliasHdr (currentEntity'^.eBackLerp) shadeLight shadeDots

      Image.glTexEnv GL.gl_REPLACE
      GL.glShadeModel GL.gl_FLAT

      GL.glPopMatrix

      when (((currentEntity'^.enFlags) .&. Constants.rfWeaponModel /= 0) && handValue == 1) $ do
        GL.glMatrixMode GL.gl_PROJECTION
        GL.glPopMatrix
        GL.glMatrixMode GL.gl_MODELVIEW
        GL.glCullFace GL.gl_FRONT

      when ((currentEntity'^.enFlags) .&. Constants.rfTranslucent /= 0) $
        GL.glDisable GL.gl_BLEND

      when ((currentEntity'^.enFlags) .&. Constants.rfDepthHack /= 0) $ do
        depthMin <- use $ fastRenderAPIGlobals.frGLDepthMin
        depthMax <- use $ fastRenderAPIGlobals.frGLDepthMax
        GL.glDepthRange (realToFrac depthMin) (realToFrac depthMax)

      shadowsValue <- liftM (^.cvValue) glShadowsCVar
      when (shadowsValue /= 0 && ((currentEntity'^.enFlags) .&. (Constants.rfTranslucent .|. Constants.rfWeaponModel) == 0)) $ do
        GL.glPushMatrix
        rRotateForEntity e
        GL.glDisable GL.gl_TEXTURE_2D
        GL.glEnable GL.gl_BLEND
        GL.glColor4f 0 0 0 0.5
        glDrawAliasShadow pAliasHdr (currentEntity'^.eFrame) shadeVector
        GL.glEnable GL.gl_TEXTURE_2D
        GL.glDisable GL.gl_BLEND
        GL.glPopMatrix

      GL.glColor4f 1 1 1 1

  where checkIfDone :: EntityT -> Quake Bool
        checkIfDone e = do
          if (e^.enFlags) .&. Constants.rfWeaponModel == 0
            then
              rCullAliasModel entRef
            else do
              handValue <- liftM (^.cvValue) handCVar
              return $ if handValue == 2 then True else False

        buildShadeLight :: Quake (V3 Float)
        buildShadeLight = do
          Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
          currentEntity <- io $ readIORef currentEntityRef

          shadeLight <- if | (currentEntity^.enFlags) .&. (Constants.rfShellHalfDam .|. Constants.rfShellGreen .|. Constants.rfShellRed .|. Constants.rfShellBlue .|. Constants.rfShellDouble) /= 0 -> do
                               let (a, b, c) = if (currentEntity^.enFlags) .&. Constants.rfShellHalfDam /= 0
                                                   then (0.56, 0.59, 0.45)
                                                   else (0, 0, 0)
                                   (a', b') = if (currentEntity^.enFlags) .&. Constants.rfShellDouble /= 0
                                                then (0.9, 0.7)
                                                else (a, b)
                                   a'' = if (currentEntity^.enFlags) .&. Constants.rfShellRed /= 0 then 1 else a'
                                   b'' = if (currentEntity^.enFlags) .&. Constants.rfShellGreen /= 0 then 1 else b'
                                   c' = if (currentEntity^.enFlags) .&. Constants.rfShellBlue /= 0 then 1 else c
                               return (V3 a'' b'' c')

                           | (currentEntity^.enFlags) .&. Constants.rfFullBright /= 0 ->
                               return (V3 1 1 1)

                           | otherwise -> do
                               shadeLight <- Light.rLightPoint (currentEntity^.eOrigin)

                               -- player lighting hack for communication back to server
                               -- big hack!
                               when ((currentEntity^.enFlags) .&. Constants.rfWeaponModel /= 0) $ do
                                 let v = if (shadeLight^._x) > (shadeLight^._y)
                                           then
                                             if (shadeLight^._x) > (shadeLight^._z)
                                               then shadeLight^._x
                                               else shadeLight^._z
                                           else
                                             if (shadeLight^._y) > (shadeLight^._z)
                                               then shadeLight^._y
                                               else shadeLight^._z

                                 clLightLevelCVar >>= \lightLevel -> CVar.update lightLevel { _cvValue = 150 * v }

                               monoLightmap <- liftM (^.cvString) glMonoLightMapCVar

                               return $ if monoLightmap `BC.index` 0 /= '0'
                                          then let s = shadeLight^._x
                                                   s' = if s < (shadeLight^._y) then shadeLight^._y else s
                                                   s'' = if s' < (shadeLight^._z) then shadeLight^._z else s'
                                               in V3 s'' s'' s''
                                          else shadeLight

          let shadeLight' = if (currentEntity^.enFlags) .&. Constants.rfMinLight /= 0
                              then let v = (shadeLight^._x) > 0.1 || (shadeLight^._y) > 0.1 || (shadeLight^._z) > 0.1
                                   in if v
                                        then shadeLight
                                        else V3 0.1 0.1 0.1
                              else
                                shadeLight

          newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef

          let shadeLight'' = if (currentEntity^.enFlags) .&. Constants.rfGlow /= 0 -- bonus items will pulse with time
                               then
                                 let scale = 0.1 * sin (7 * (newRefDef^.rdTime))
                                     smin = fmap (* 0.8) shadeLight'
                                     a = if (shadeLight'^._x) + scale < (smin^._x)
                                           then (smin^._x)
                                           else (shadeLight'^._x) + scale
                                     b = if (shadeLight'^._y) + scale < (smin^._y)
                                           then (smin^._y)
                                           else (shadeLight'^._y) + scale
                                     c = if (shadeLight'^._z) + scale < (smin^._z)
                                           then (smin^._z)
                                           else (shadeLight'^._z) + scale
                                 in V3 a b c
                               else
                                 shadeLight'

          let shadeLight''' = if ((newRefDef^.rdRdFlags) .&. Constants.rdfIrGoggles /= 0) && ((currentEntity^.enFlags) .&. Constants.rfIrVisible /= 0)
                                then V3 1 0 0
                                else shadeLight''

          return shadeLight'''

        checkCurrentEntityFrame :: IORef EntityT -> DMdlT -> Quake ()
        checkCurrentEntityFrame currentEntityRef pAliasHdr = do
          currentEntity <- io $ readIORef currentEntityRef

          when ((currentEntity^.eFrame) >= (pAliasHdr^.dmNumFrames) || (currentEntity^.eFrame) < 0) $ do
            Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
            currentModel <- io $ readIORef currentModelRef

            VID.printf Constants.printAll ("R_DrawAliasModel " `B.append` (currentModel^.mName) `B.append` ": no such frame " `B.append` BC.pack (show (currentEntity^.eFrame)) `B.append` "\n") -- IMPROVE?
            io $ modifyIORef' currentEntityRef (\v -> v { _eFrame = 0
                                                        , _eOldFrame = 0
                                                        })

        checkCurrentEntityOldFrame :: IORef EntityT -> DMdlT -> Quake ()
        checkCurrentEntityOldFrame currentEntityRef pAliasHdr = do
          currentEntity <- io $ readIORef currentEntityRef

          when ((currentEntity^.eOldFrame) >= (pAliasHdr^.dmNumFrames) || (currentEntity^.eOldFrame) < 0) $ do
            Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
            currentModel <- io $ readIORef currentModelRef

            VID.printf Constants.printAll ("R_DrawAliasModel " `B.append` (currentModel^.mName) `B.append` ": no such frame " `B.append` BC.pack (show (currentEntity^.eOldFrame)) `B.append` "\n") -- IMPROVE?
            io $ modifyIORef' currentEntityRef (\v -> v { _eFrame = 0
                                                        , _eOldFrame = 0
                                                        })

        checkLerpModels :: IORef EntityT -> Quake ()
        checkLerpModels currentEntityRef = do
          lerpModelsValue <- liftM (^.cvValue) lerpModelsCVar
          when (lerpModelsValue == 0) $
            io $ modifyIORef' currentEntityRef (\v -> v { _eBackLerp = 0 })

rCullAliasModel :: IORef EntityT -> Quake Bool
rCullAliasModel entRef = do
    e <- io $ readIORef entRef

    Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
    currentModel <- io $ readIORef currentModelRef

    let Just (AliasModelExtra pAliasHdr) = currentModel^.mExtraData

    when ((e^.eFrame) >= (pAliasHdr^.dmNumFrames) || (e^.eFrame) < 0) $ do
      VID.printf Constants.printAll ("R_CullAliasModel " `B.append` (currentModel^.mName) `B.append` ": no such frame " `B.append` BC.pack (show (e^.eFrame)) `B.append` "\n") -- IMPROVE?
      io $ modifyIORef' entRef (\v -> v { _eFrame = 0 })

    when ((e^.eOldFrame) >= (pAliasHdr^.dmNumFrames) || (e^.eOldFrame) < 0) $ do
      VID.printf Constants.printAll ("R_CullAliasModel " `B.append` (currentModel^.mName) `B.append` ": no such oldframe " `B.append` BC.pack (show (e^.eOldFrame)) `B.append` "\n") -- IMPROVE?
      io $ modifyIORef' entRef (\v -> v { _eOldFrame = 0})

    e' <- io $ readIORef entRef

    let pFrame = (fromJust $ pAliasHdr^.dmAliasFrames) V.! (e'^.eFrame)
        pOldFrame = (fromJust $ pAliasHdr^.dmAliasFrames) V.! (e'^.eOldFrame)

    -- compute axially aligned mins and maxs
    let (mins, maxs) = if pFrame == pOldFrame
                         then (pFrame^.dafTranslate, (pFrame^.dafTranslate) + fmap (* 255) (pFrame^.dafScale))
                         else let thismaxs = (pFrame^.dafTranslate) + fmap (* 255) (pFrame^.dafScale)
                                  oldmaxs = (pOldFrame^.dafTranslate) + fmap (* 255) (pOldFrame^.dafScale)
                                  mina = if (pFrame^.dafTranslate._x) < (pOldFrame^.dafTranslate._x) then pFrame^.dafTranslate._x else pOldFrame^.dafTranslate._x
                                  minb = if (pFrame^.dafTranslate._y) < (pOldFrame^.dafTranslate._y) then pFrame^.dafTranslate._y else pOldFrame^.dafTranslate._y
                                  minc = if (pFrame^.dafTranslate._z) < (pOldFrame^.dafTranslate._z) then pFrame^.dafTranslate._z else pOldFrame^.dafTranslate._z
                                  maxa = if (thismaxs^._x) > (oldmaxs^._x) then thismaxs^._x else oldmaxs^._x
                                  maxb = if (thismaxs^._y) > (oldmaxs^._y) then thismaxs^._y else oldmaxs^._y
                                  maxc = if (thismaxs^._z) > (oldmaxs^._z) then thismaxs^._z else oldmaxs^._z
                              in (V3 mina minb minc, V3 maxa maxb maxc)

    -- compute a full bounding box
    let bbox = V.generate 8 (computeBoundingBox mins maxs)
    -- rotate the bounding box
    let tmp = let V3 a b c = mins
              in V3 a (negate b) c -- TODO: jake2 code uses YAW from Constants
        (Just v0, Just v1, Just v2) = Math3D.angleVectors tmp True True True
        bbox' = fmap (rotateBoundingBox (e'^.eOrigin) v0 v1 v2) bbox
        aggregateMask = complement 0 :: Int32

    frustum <- use $ fastRenderAPIGlobals.frFrustum
    frustum' <- io $ V.mapM readIORef frustum
    let mask = computeAggregateMask frustum' bbox' aggregateMask 0 8

    return $ if mask /= 0 then True else False

  where computeBoundingBox :: V3 Float -> V3 Float -> Int -> V3 Float
        computeBoundingBox mins maxs idx =
          let a = if idx .&. 1 /= 0 then mins^._x else maxs^._x
              b = if idx .&. 2 /= 0 then mins^._y else maxs^._y
              c = if idx .&. 4 /= 0 then mins^._z else maxs^._z
          in V3 a b c

        rotateBoundingBox :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
        rotateBoundingBox origin v0 v1 v2 tmp =
          let a = v0 `dot` tmp
              b = v1 `dot` tmp
              c = v2 `dot` tmp
          in (V3 a b c) + origin

        computeAggregateMask :: V.Vector CPlaneT -> V.Vector (V3 Float) -> Int32 -> Int -> Int -> Int32
        computeAggregateMask frustum bbox aggregateMask idx maxIdx
          | idx >= maxIdx = aggregateMask
          | otherwise =
              let mask = computeMask frustum (bbox V.! idx) 0 0 4
              in computeAggregateMask frustum bbox (aggregateMask .&. mask) (idx + 1) maxIdx

        computeMask :: V.Vector CPlaneT -> V3 Float -> Int32 -> Int -> Int -> Int32
        computeMask frustum bbox mask idx maxIdx
          | idx >= maxIdx = mask
          | otherwise =
              let dp = ((frustum V.! idx)^.cpNormal) `dot` bbox
              in if dp - ((frustum V.! idx)^.cpDist) < 0
                   then computeMask frustum bbox (mask .|. (1 `shiftL` idx)) (idx + 1) maxIdx
                   else computeMask frustum bbox mask (idx + 1) maxIdx

myGLUPerspective :: Double -> Double -> Double -> Double -> Quake ()
myGLUPerspective fovY aspect zNear zFar = do
    glState <- use $ fastRenderAPIGlobals.frGLState

    let ymax = zNear * tan (fovY * pi / 360)
        ymin = negate ymax
        xmin = ymin * aspect
        xmax = ymax * aspect
        xmin' = xmin - (2 * float2Double (glState^.glsCameraSeparation)) / zNear
        xmax' = xmax - (2 * float2Double (glState^.glsCameraSeparation)) / zNear

    GL.glFrustum (realToFrac xmin)
                 (realToFrac xmax)
                 (realToFrac ymin)
                 (realToFrac ymax)
                 (realToFrac zNear)
                 (realToFrac zFar)

rRotateForEntity :: EntityT -> Quake ()
rRotateForEntity e = do
    let origin = fmap realToFrac (e^.eOrigin)
        angles = fmap realToFrac (e^.eAngles)

    GL.glTranslatef (origin^._x) (origin^._y) (origin^._z)

    GL.glRotatef          (angles^._y) 0 0 1
    GL.glRotatef (negate $ angles^._x) 0 1 0
    GL.glRotatef (negate $ angles^._z) 1 0 0

glDrawAliasShadow :: DMdlT -> Int -> V3 Float -> Quake ()
glDrawAliasShadow pAliasHdr poseNum shadeVector = do
    lightSpot <- use $ fastRenderAPIGlobals.frLightSpot
    Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
    currentEntity <- io $ readIORef currentEntityRef

    let lHeight = (currentEntity^.eOrigin._z) - (lightSpot^._z)
        height = 1 - lHeight
        order = fromJust (pAliasHdr^.dmGlCmds)

    drawShadow order 0 lHeight height

  where drawShadow :: UV.Vector Word32 -> Int -> Float -> Float -> Quake ()
        drawShadow order orderIndex lHeight height = do
          let count = order UV.! orderIndex

          unless (count == 0) $ do
            count' <- if count < 0
                        then do
                          GL.glBegin GL.gl_TRIANGLE_FAN
                          return (negate count)
                        else do
                          GL.glBegin GL.gl_TRIANGLE_STRIP
                          return count

            vertexArrayBuf <- use $ fastRenderAPIGlobals.frVertexArrayBuf
            orderIndex' <- io $ draw vertexArrayBuf order (orderIndex + 1) count' lHeight height

            GL.glEnd

            drawShadow order orderIndex' lHeight height

        draw :: MSV.IOVector Float -> UV.Vector Word32 -> Int -> Word32 -> Float -> Float -> IO Int
        draw vertexArrayBuf order orderIndex count lHeight height = do
          let index = fromIntegral $ (order UV.! (orderIndex + 2)) * 3

          a <- MSV.read vertexArrayBuf (index + 0)
          b <- MSV.read vertexArrayBuf (index + 1)
          c <- MSV.read vertexArrayBuf (index + 2)

          let a' = a - (shadeVector^._x) * (c + lHeight)
              b' = b - (shadeVector^._y) * (c + lHeight)
              c' = height

          GL.glVertex3f (realToFrac a') (realToFrac b') (realToFrac c')

          let orderIndex' = orderIndex + 3
              count' = count - 1

          if count' == 0
            then return orderIndex'
            else draw vertexArrayBuf order orderIndex' count' lHeight height

{-
- GL_DrawAliasFrameLerp
- 
- interpolates between two frames and origins
- FIXME: batch lerp all vertexes
-}
glDrawAliasFrameLerp :: DMdlT -> Float -> V3 Float -> UV.Vector Float -> Quake ()
glDrawAliasFrameLerp pAliasHdr backLerp shadeLight shadeDots = do
    Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
    currentEntity <- io $ readIORef currentEntityRef

    let frame = (fromJust $ pAliasHdr^.dmAliasFrames) V.! (currentEntity^.eFrame)
        oldFrame = (fromJust $ pAliasHdr^.dmAliasFrames) V.! (currentEntity^.eOldFrame)
        alpha = if (currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0
                  then currentEntity^.eAlpha
                  else 1.0
        frontLerp = 1.0 - backLerp
        -- move should be the delta back to the previous frame * backlerp
        frontV = (currentEntity^.eOldOrigin) - (currentEntity^.eOrigin)
        (Just v0, Just v1, Just v2) = Math3D.angleVectors (currentEntity^.eAngles) True True True
        move = (V3 (frontV `dot` v0) (negate $ frontV `dot` v1) (frontV `dot` v2)) + (oldFrame^.dafTranslate)
        move' = fmap (* backLerp) move + fmap (* frontLerp) (frame^.dafTranslate)
        frontV' = fmap (* frontLerp) (frame^.dafScale)
        backV' = fmap (* backLerp) (oldFrame^.dafScale)

    -- PMM - added double shell
    when ((currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0) $
      GL.glDisable GL.gl_TEXTURE_2D

    glLerpVerts (pAliasHdr^.dmNumXYZ) (oldFrame^.dafVerts) (frame^.dafVerts) move' frontV' backV'

    vertexArrayBuf <- use $ fastRenderAPIGlobals.frVertexArrayBuf
    io $ MSV.unsafeWith vertexArrayBuf $ \ptr ->
      GL.glVertexPointer 3 GL.gl_FLOAT 0 ptr

    -- PMM - added double damage shell
    if (currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0
      then
        GL.glColor4f (realToFrac $ shadeLight^._x) (realToFrac $ shadeLight^._y) (realToFrac $ shadeLight^._z) (realToFrac alpha)
      else do
        GL.glEnableClientState GL.gl_COLOR_ARRAY
        io $ MSV.unsafeWith colorArrayBuf $ \ptr ->
          GL.glColorPointer 4 GL.gl_FLOAT 0 ptr

        -- pre light everything
        io $ prelight (frame^.dafVerts) alpha 0 0 (pAliasHdr^.dmNumXYZ)

    texture0 <- use $ fastRenderAPIGlobals.frTexture0
    GL.glClientActiveTextureARB (fromIntegral texture0)
    io $ MSV.unsafeWith textureArrayBuf $ \ptr ->
      GL.glTexCoordPointer 2 GL.gl_FLOAT 0 ptr

    srcTextureCoords <- use $ fastRenderAPIGlobals.frModelTextureCoordBuf
    vertexIndexBuf <- use $ fastRenderAPIGlobals.frModelVertexIndexBuf
    io $ drawElements vertexIndexBuf srcTextureCoords 0 0 (UV.length $ pAliasHdr^.dmCounts)

    -- PMM - added double shell
    when ((currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0) $
      GL.glEnable GL.gl_TEXTURE_2D

    GL.glDisableClientState GL.gl_COLOR_ARRAY

  where prelight :: UV.Vector Int -> Float -> Int -> Int -> Int -> IO ()
        prelight verts alpha j idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let l = shadeDots UV.! (((verts UV.! idx) `shiftR` 24) .&. 0xFF)

              MSV.write colorArrayBuf (j + 0) (l * (shadeLight^._x))
              MSV.write colorArrayBuf (j + 1) (l * (shadeLight^._y))
              MSV.write colorArrayBuf (j + 2) (l * (shadeLight^._z))
              MSV.write colorArrayBuf (j + 3) alpha

              prelight verts alpha (j + 4) (idx + 1) maxIdx

        drawElements :: MSV.IOVector Int32 -> MSV.IOVector Float -> Int -> Int -> Int -> IO ()
        drawElements vertexIndexBuf srcTextureCoords pos idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let count = (pAliasHdr^.dmCounts) UV.! idx

              unless (count == 0) $ do
                let (indexStartIdx, indexLength) = (pAliasHdr^.dmIndexElements) V.! idx
                    (mode, count') = if count < 0
                                       then (GL.gl_TRIANGLE_FAN, negate count)
                                       else (GL.gl_TRIANGLE_STRIP, count)
                    srcIndex = (pos `shiftL` 1) - 1

                let buf = MSV.slice indexStartIdx indexLength vertexIndexBuf
                addTextureCoords buf srcTextureCoords srcIndex 0 (fromIntegral count')

                MSV.unsafeWith buf $ \ptr ->
                  GL.glDrawElements mode (fromIntegral indexLength) GL.gl_UNSIGNED_INT ptr

                drawElements vertexIndexBuf srcTextureCoords (pos + fromIntegral count') (idx + 1) maxIdx

        addTextureCoords :: MSV.IOVector Int32 -> MSV.IOVector Float -> Int -> Int -> Int -> IO ()
        addTextureCoords srcIndexBuf srcTextureCoords srcIndex idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              dstIndex <- MSV.read srcIndexBuf idx
              let dstIndex' = dstIndex `shiftL` 1
              a <- MSV.read srcTextureCoords (srcIndex + 1)
              b <- MSV.read srcTextureCoords (srcIndex + 2)
              MSV.write textureArrayBuf (fromIntegral dstIndex + 0) a
              MSV.write textureArrayBuf (fromIntegral dstIndex + 1) b
              addTextureCoords srcIndexBuf srcTextureCoords (srcIndex + 2) (idx + 1) maxIdx

glLerpVerts :: Int -> UV.Vector Int -> UV.Vector Int -> V3 Float -> V3 Float -> V3 Float -> Quake ()
glLerpVerts nVerts ov v move frontV backV = do
    vertexArrayBuf <- io $ MSV.new (nVerts * 3)
    fastRenderAPIGlobals.frVertexArrayBuf .= vertexArrayBuf

    Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
    currentEntity <- io $ readIORef currentEntityRef

    if (currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0
      then io $ updateLerp vertexArrayBuf 0 0 nVerts
      else io $ updateLerp2 vertexArrayBuf 0 0 nVerts

  where updateLerp :: MSV.IOVector Float -> Int -> Int -> Int -> IO ()
        updateLerp vertexArrayBuf j idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let vv = v UV.! idx
                  ovv = ov UV.! idx
                  normal = vertexNormals V.! ((vv `shiftR` 24) .&. 0xFF)

              MSV.write vertexArrayBuf (j + 0) ((move^._x) + fromIntegral (ovv .&. 0xFF) * (backV^._x) + fromIntegral (vv .&. 0xFF) * (frontV^._x) + (normal^._x) * Constants.powersuitScale)
              MSV.write vertexArrayBuf (j + 1) ((move^._y) + fromIntegral ((ovv `shiftR` 8) .&. 0xFF) * (backV^._y) + fromIntegral ((vv `shiftR` 8) .&. 0xFF) * (frontV^._y) + (normal^._y) * Constants.powersuitScale)
              MSV.write vertexArrayBuf (j + 2) ((move^._z) + fromIntegral ((ovv `shiftR` 16) .&. 0xFF) * (backV^._z) + fromIntegral ((vv `shiftR` 16) .&. 0xFF) * (frontV^._z) + (normal^._z) * Constants.powersuitScale)

              updateLerp vertexArrayBuf (j + 3) (idx + 1) maxIdx

        updateLerp2 :: MSV.IOVector Float -> Int -> Int -> Int -> IO ()
        updateLerp2 vertexArrayBuf j idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ovv = ov UV.! idx
                  vv = v UV.! idx

              MSV.write vertexArrayBuf (j + 0) ((move^._x) + fromIntegral (ovv .&. 0xFF) * (backV^._x) + fromIntegral (vv .&. 0xFF) * (frontV^._x))
              MSV.write vertexArrayBuf (j + 1) ((move^._y) + fromIntegral ((ovv `shiftR` 8) .&. 0xFF) * (backV^._y) + fromIntegral ((vv `shiftR` 8) .&. 0xFF) * (frontV^._y))
              MSV.write vertexArrayBuf (j + 2) ((move^._z) + fromIntegral ((ovv `shiftR` 16) .&. 0xFF) * (backV^._z) + fromIntegral ((vv `shiftR` 16) .&. 0xFF) * (frontV^._z))

              updateLerp2 vertexArrayBuf (j + 3) (idx + 1) maxIdx
