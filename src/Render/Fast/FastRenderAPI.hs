{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.FastRenderAPI
  ( fastRenderAPI
  ) where

import           Client.RefDefT
import           Client.VidDefT
import {-# SOURCE #-} qualified Client.VID as VID
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CPlaneT
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QuakeIOState
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Draw as Draw
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Light as Light
import qualified Render.Fast.Mesh as Mesh
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Surf as Surf
import qualified Render.Fast.Warp as Warp
import           Render.GLConfigT
import           Render.GLStateT
import           Render.ImageT
import           Render.MLeafT
import           Render.OpenGL.GLDriver
import           Types
import qualified Util.Math3D as Math3D

import           Control.Applicative (liftA2)
import           Control.Exception (handle, IOException)
import           Control.Lens (use, (^.), (.=), (%=), (+=), (&), (.~), (+~), (-~))
import           Control.Monad (void, when, unless, join)
import           Data.Bits (shiftL, shiftR, (.|.), (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower, toUpper)
import           Data.Int (Int8, Int32)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import           Data.Word (Word8)
import           Foreign.Ptr (nullPtr, castPtr)
import           Foreign.Marshal.Array (withArray, allocaArray, peekArray)
import           GHC.Float (float2Double)
import qualified Graphics.GL as GL
import           Linear (V3, dot, _x, _y, _z, _w)
import           Text.Read (readMaybe)

fastRenderAPI :: RenderAPI
fastRenderAPI =
    RenderAPI { _rInit              = fastInit
              , _rInit2             = fastInit2
              , _rShutdown          = fastShutdown
              , _rBeginRegistration = const Model.rBeginRegistration
              , _rRegisterModel     = const Model.rRegisterModel
              , _rRegisterSkin      = const Image.rRegisterSkin
              , _rDrawFindPic       = const Draw.findPic
              , _rSetSky            = const Warp.rSetSky
              , _rEndRegistration   = const Model.rEndRegistration
              , _rRenderFrame       = const fastRenderFrame
              , _rDrawGetPicSize    = const Draw.getPicSize
              , _rDrawPic           = const Draw.drawPic
              , _rDrawStretchPic    = const Draw.stretchPic
              , _rDrawChar          = const Draw.drawChar
              , _rDrawTileClear     = error "FastRenderAPI.rDrawTileClear" -- TODO
              , _rDrawFill          = const Draw.fill
              , _rDrawFadeScreen    = const Draw.fadeScreen
              , _rDrawStretchRaw    = const Draw.stretchRaw
              , _rSetPalette        = const fastSetPalette
              , _rBeginFrame        = fastBeginFrame
              , _glScreenShotF      = error "FastRenderAPI.glScreenShotF" -- TODO
              }

fastInit :: GLDriver -> Int -> Int -> Quake Bool
fastInit glDriver _ _ =
  do VID.printf Constants.printAll (B.concat ["ref_gl version: ", Constants.refVersion, "\n"])
     Image.getPalette
     rRegister
     fastRenderAPIGlobals.frGLState.glsPrevMode .= 3
     rSetMode glDriver >>= initResult
  where initResult True = return True
        initResult False =
          do VID.printf Constants.printAll "ref_gl::R_Init() - could not R_SetMode()\n"
             return False

fastInit2 :: GLDriver -> Quake Bool
fastInit2 glDriver =
  do VID.menuInit
     initGLStrings
     identifyGLRenderer
     rendererSpecificSettings =<< use (fastRenderAPIGlobals.frGLConfig.glcRenderer)
     initExtensions =<< use (fastRenderAPIGlobals.frGLConfig.glcExtensionsString)
     finishInit glDriver =<< use (fastRenderAPIGlobals.frActiveTextureARB)

initGLStrings :: Quake ()
initGLStrings =
  do vendor <- request (io (getGLString GL.GL_VENDOR))
     renderer <- request (io (getGLString GL.GL_RENDERER))
     version <- request (io (getGLString GL.GL_VERSION))
     extensions <- request (io (getGLString GL.GL_EXTENSIONS))
     fastRenderAPIGlobals.frGLConfig.glcVendorString .= vendor
     fastRenderAPIGlobals.frGLConfig.glcRendererString .= renderer
     fastRenderAPIGlobals.frGLConfig.glcVersionString .= version
     fastRenderAPIGlobals.frGLConfig.glcExtensionsString .= extensions
     fastRenderAPIGlobals.frGLConfig.glcVersion .= versionString version
     VID.printf Constants.printAll (B.concat ["GL_VENDOR: ", vendor, "\n"])
     VID.printf Constants.printAll (B.concat ["GL_RENDERER: ", renderer, "\n"])
     VID.printf Constants.printAll (B.concat ["GL_VERSION: ", version, "\n"])
     VID.printf Constants.printAll (B.concat ["GL_EXTENSIONS: ", extensions, "\n"])
  where versionString version =
          fromMaybe 1.1 (readMaybe (BC.unpack (B.take 3 version)))

identifyGLRenderer :: Quake ()
identifyGLRenderer =
  do renderer <- fmap (BC.map toLower) (use (fastRenderAPIGlobals.frGLConfig.glcRendererString))
     vendor <- fmap (BC.map toLower) (use (fastRenderAPIGlobals.frGLConfig.glcVendorString))
     fastRenderAPIGlobals.frGLConfig.glcRenderer .= identifyRenderer renderer vendor
  where identifyRenderer renderer vendor
          | "voodoo" `BC.isInfixOf` renderer = pickVoodoo renderer
          | "sgi" `BC.isInfixOf` vendor = Constants.glRendererSGI
          | "permedia" `BC.isInfixOf` renderer = Constants.glRendererPerMedia2
          | "glint" `BC.isInfixOf` renderer = Constants.glRendererGlintMX
          | "glzicd" `BC.isInfixOf` renderer = Constants.glRendererRealizm
          | "gdi" `BC.isInfixOf` renderer = Constants.glRendererMCD
          | "pcx2" `BC.isInfixOf` renderer = Constants.glRendererPCX2
          | "verite" `BC.isInfixOf` renderer = Constants.glRendererRendition
          | otherwise = Constants.glRendererOther
        pickVoodoo renderer
          | "rush" `BC.isInfixOf` renderer = Constants.glRendererVoodooRush
          | otherwise = Constants.glRendererVoodoo

rendererSpecificSettings :: Int -> Quake ()
rendererSpecificSettings rendererId =
  do monoLightSettings rendererId =<< glMonoLightMapCVar
     void (CVar.set "scr_drawall" setDrawAll)
     when (rendererId == Constants.glRendererMCD) (CVar.setValueI "gl_finish" 1)
     setAllowCds rendererId
  where setDrawAll | rendererId .&. Constants.glRendererPowerVR /= 0 = "1"
                   | otherwise = "0"

monoLightSettings :: Int -> CVarT -> Quake ()
monoLightSettings rendererId monoLightMap
  | useMlm && rendererId == Constants.glRendererPerMedia2 =
      do void (CVar.set "gl_monolightmap" "A")
         VID.printf Constants.printAll "...using gl_monolightmap 'a'\n"
  | useMlm = void (CVar.set "gl_monolightmap" "0")
  | otherwise = return ()
  where mlm = BC.map toUpper (monoLightMap^.cvString)
        useMlm = B.length mlm < 2 || mlm `BC.index` 1 /= 'F'

setAllowCds :: Int -> Quake ()
setAllowCds rendererId =
  do allow <- checkAllow
     fastRenderAPIGlobals.frGLConfig.glcAllowCds .= allow
     VID.printf Constants.printAll (allowStr allow)
  where checkAllow | rendererId .&. Constants.glRenderer3DLabs /= 0 =
                       fmap ((== 0) . (^.cvValue)) gl3DLabsBrokenCVar
                   | otherwise = return True
        allowStr True = "...allowing CDS\n"
        allowStr False = "...disabling CDS\n"

initExtensions :: B.ByteString -> Quake ()
initExtensions extensions =
  do checkCompiledVertexArray extensions
     checkSwapControl extensions
     checkPointParameters extensions
     checkColorTable extensions
     checkMultiTexture extensions

checkCompiledVertexArray :: B.ByteString -> Quake ()
checkCompiledVertexArray extensions =
  do enabled <- checkEnabled
     fastRenderAPIGlobals.frLockArraysEXT .= enabled
  where checkEnabled
          | extensionAvailable =
              do VID.printf Constants.printAll "...enabling GL_EXT_compiled_vertex_array\n"
                 fmap ((/= 0) . (^.cvValue)) glExtCompiledVertexArrayCVar
          | otherwise =
              do VID.printf Constants.printAll "...GL_EXT_compiled_vertex_array not found\n"
                 return False
        extensionAvailable =
          or (fmap (`BC.isInfixOf` extensions) ["GL_EXT_compiled_vertex_array", "GL_SGI_compiled_vertex_array"])

checkSwapControl :: B.ByteString -> Quake ()
checkSwapControl extensions =
  do enabled <- checkEnabled
     fastRenderAPIGlobals.frSwapIntervalEXT .= enabled
  where checkEnabled
          | extensionAvailable =
              do VID.printf Constants.printAll "...enabling WGL_EXT_swap_control\n"
                 return True
          | otherwise =
              do VID.printf Constants.printAll "...WGL_EXT_swap_control not found\n"
                 return False
        extensionAvailable = "WGL_EXT_swap_control" `BC.isInfixOf` extensions

checkPointParameters :: B.ByteString -> Quake ()
checkPointParameters extensions =
  do enabled <- checkEnabled
     fastRenderAPIGlobals.frPointParameterEXT .= enabled
  where checkEnabled
          | extensionAvailable =
              checkIgnore =<< glExtPointParametersCVar
          | otherwise =
              do VID.printf Constants.printAll "...GL_EXT_point_parameters not found\n"
                 return False
        extensionAvailable = "GL_EXT_point_parameters" `BC.isInfixOf` extensions
        checkIgnore pointParameters
          | (pointParameters^.cvValue) /= 0 =
              do VID.printf Constants.printAll "...using GL_EXT_point_parameters\n"
                 return True
          | otherwise =
              do VID.printf Constants.printAll "...ignoring GL_EXT_point_parameters\n"
                 return False

checkColorTable :: B.ByteString -> Quake ()
checkColorTable extensions =
  do colorTable <- use (fastRenderAPIGlobals.frColorTableEXT)
     enabled <- checkEnabled colorTable
     fastRenderAPIGlobals.frColorTableEXT .= enabled
  where checkEnabled colorTable
          | extensionAvailable colorTable =
              checkIgnore =<< glExtPalettedTextureCVar
          | otherwise =
              do VID.printf Constants.printAll "...GL_EXT_shared_texture_palette not found\n"
                 return False
        extensionAvailable colorTable =
          not colorTable && or (fmap (`BC.isInfixOf` extensions) ["GL_EXT_paletted_texture", "GL_EXT_shared_texture_palette"])
        checkIgnore palettedTexture
          | (palettedTexture^.cvValue) /= 0 =
              do VID.printf Constants.printAll "...using GL_EXT_shared_texture_palette\n"
                 return True
          | otherwise =
              do VID.printf Constants.printAll "...ignoring GL_EXT_shared_texture_palette\n"
                 return False

checkMultiTexture :: B.ByteString -> Quake ()
checkMultiTexture extensions =
  do enabled <- checkEnabled =<< checkExtensionWorks
     fastRenderAPIGlobals.frActiveTextureARB .= enabled
  where checkEnabled ok
          | extensionAvailable && ok =
              do VID.printf Constants.printAll "...using GL_ARB_multitexture\n"
                 fastRenderAPIGlobals.frTexture0 .= Constants.glTexture0ARB
                 fastRenderAPIGlobals.frTexture1 .= Constants.glTexture1ARB
                 return True
          | extensionAvailable = return False
          | otherwise =
              do VID.printf Constants.printAll "...GL_ARB_multitexture not found\n"
                 return False
        checkExtensionWorks = request (io execActiveTexture)
        execActiveTexture = handle (\(_ :: IOException) -> return False) $
          do GL.glClientActiveTextureARB (fromIntegral Constants.glTexture0ARB)
             return True
        extensionAvailable = "GL_ARB_multitexture" `BC.isInfixOf` extensions

finishInit :: GLDriver -> Bool -> Quake Bool
finishInit glDriver enabled
  | not enabled && not True = -- TODO: remove me, used for testing with dummy gldriver
      do VID.printf Constants.printAll "Missing multi-texturing!\n"
         return False
  | otherwise =
      do glSetDefaultState glDriver
         Image.glInitImages
         Model.modInit
         rInitParticleTexture
         Draw.initLocal
         checkGLError
         glDriver^.gldEndFrame
         return True

checkGLError :: Quake ()
checkGLError =
  do err <- request (io GL.glGetError)
     unless (err == GL.GL_NO_ERROR) $
       VID.printf Constants.printAll "gl.glGetError() = TODO" -- TODO: add error information

getGLString :: GL.GLenum -> IO B.ByteString
getGLString n =
  do str <- GL.glGetString n
     maybeNullPtr (return B.empty) (B.packCString . castPtr) str
  where maybeNullPtr nothing f ptr | ptr == nullPtr = nothing
                                   | otherwise = f ptr

shutdownCommands :: [B.ByteString]
shutdownCommands = ["modellist", "screenshot", "imagelist", "gl_strings"]

fastShutdown :: GLDriver -> Quake ()
fastShutdown glDriver =
  do Cmd.removeCommands shutdownCommands
     Model.freeAll
     Image.glShutdownImages
     glDriver^.gldShutdown

fastRenderFrame :: RefDefT -> Quake ()
fastRenderFrame refDef =
  do renderView refDef
     setLightLevel =<< use (fastRenderAPIGlobals.frNewRefDef)
     setGL2D =<< use (fastRenderAPIGlobals.frVid)

renderView :: RefDefT -> Quake ()
renderView refDef =
  do checkRefresh =<< noRefreshCVar
     checkWorldModel =<< use (fastRenderAPIGlobals.frWorldModel)
     checkSpeeds =<< speedsCVar
     Light.rPushDLights
     checkGLFinish =<< glFinishCVar
     rSetupFrame
     rSetFrustum
     rSetupGL
     Surf.rMarkLeaves -- done here so we know if we're in water
     Surf.rDrawWorld
     rDrawEntitiesOnList
     Light.rRenderDLights
     rDrawParticles
     Surf.rDrawAlphaSurfaces
     rFlash
  where checkRefresh noRefresh =
          when ((noRefresh^.cvValue) == 0) $
            fastRenderAPIGlobals.frNewRefDef .= refDef
        checkWorldModel (Just _) = return ()
        checkWorldModel Nothing =
          when ((refDef^.rdRdFlags) .&. Constants.rdfNoWorldModel == 0) $
            Com.comError Constants.errDrop "R_RenderView: NULL worldmodel"
        checkSpeeds speeds =
          when ((speeds^.cvValue) /= 0) $
            do fastRenderAPIGlobals.frCBrushPolys .= 0
               fastRenderAPIGlobals.frCAliasPolys .= 0
        checkGLFinish glFinish =
          when ((glFinish^.cvValue) /= 0) $
            request GL.glFinish

setLightLevel :: RefDefT -> Quake ()
setLightLevel newRefDef
  | (newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel == 0 =
      do light <- Light.rLightPoint (newRefDef^.rdViewOrg)
         lightLevel <- clLightLevelCVar
         CVar.update (lightLevel & cvValue .~ 150 * (maximum light))
  | otherwise = return ()

fastSetPalette :: Maybe B.ByteString -> Quake ()
fastSetPalette maybePalette =
  do maybe noPalette generatePalette maybePalette
     rawPalette <- use (fastRenderAPIGlobals.frRawPalette)
     Image.glSetTexturePalette rawPalette
     request glClearColor
  where noPalette =
          do d8to24table <- use (fastRenderAPIGlobals.frd8to24table)
             fastRenderAPIGlobals.frRawPalette .= UV.map (.|. 0xFF000000) d8to24table
        generatePalette palette =
          fastRenderAPIGlobals.frRawPalette .= UV.generate 256 (fromPalette palette)
        fromPalette palette idx =
          let j = idx * 3
              a = (fromIntegral (palette `B.index` (j + 0)) .&. 0xFF) `shiftL` 0
              b = (fromIntegral (palette `B.index` (j + 1)) .&. 0xFF) `shiftL` 8
              c = (fromIntegral (palette `B.index` (j + 2)) .&. 0xFF) `shiftL` 16
          in 0xFF000000 .|. a .|. b .|. c
        glClearColor =
          do GL.glClearColor 0 0 0 0
             GL.glClear GL.GL_COLOR_BUFFER_BIT
             GL.glClearColor 1.0 0.0 0.5 0.5

fastBeginFrame :: GLDriver -> Float -> Quake ()
fastBeginFrame glDriver cameraSeparation =
  do updateVidDef =<< use (fastRenderAPIGlobals.frVid)
     fastRenderAPIGlobals.frGLState.glsCameraSeparation .= cameraSeparation
     join (liftA2 changeModeIfNeeded vidFullScreenCVar glModeCVar)
     checkGLLog glDriver =<< glLogCVar
     checkVidGamma =<< vidGammaCVar
     (glDriver^.gldBeginFrame) cameraSeparation
     setGL2D =<< use (fastRenderAPIGlobals.frVid)
     drawBufferStuff =<< glDrawBufferCVar
     textureModeStuff
     glUpdateSwapInterval glDriver
     join (liftA2 rClear glZTrickCVar glClearCVar)

updateVidDef :: VidDefT -> Quake ()
updateVidDef vid =
  fastRenderAPIGlobals.frVid %= (\v -> v & vdWidth .~ (vid^.vdNewWidth)
                                         & vdHeight .~ (vid^.vdNewHeight))

changeModeIfNeeded :: CVarT -> CVarT -> Quake ()
changeModeIfNeeded fullScreen glMode
  | (fullScreen^.cvModified) || (glMode^.cvModified) =
      do ref <- CVar.get "vid_ref" "GLFWb" 0
         maybe vidRefError updateVar ref
  | otherwise = return ()
  where vidRefError = Com.fatalError "FastRenderAPI.changeModeIfNeeded ref is Nothing"
        updateVar ref = CVar.update (ref & cvModified .~ True)

checkGLLog :: GLDriver -> CVarT -> Quake ()
checkGLLog glDriver glLog =
  do when (glLog^.cvModified) $
       do (glDriver^.gldEnableLogging) logEnabled
          CVar.update (glLog & cvModified .~ False)
     when logEnabled (glDriver^.gldLogNewFrame)
  where logEnabled = (glLog^.cvValue) /= 0

checkVidGamma :: CVarT -> Quake ()
checkVidGamma vidGamma
  | vidGamma^.cvModified =
      do CVar.update (vidGamma & cvModified .~ False)
         r <- use (fastRenderAPIGlobals.frGLConfig.glcRenderer)
         when (r .&. Constants.glRendererVoodoo /= 0) $
           VID.printf Constants.printDeveloper "gamma anpassung fuer VOODOO nicht gesetzt"
  | otherwise = return ()

drawBufferStuff :: CVarT -> Quake ()
drawBufferStuff drawBuffer
  | drawBuffer^.cvModified =
      do CVar.update (drawBuffer & cvModified .~ False)
         glState <- use (fastRenderAPIGlobals.frGLState)
         when ((glState^.glsCameraSeparation) == 0 || not (glState^.glsStereoEnabled)) $
           request (io (drawFrontOrBackBuffer (BC.map toUpper (drawBuffer^.cvString))))
  | otherwise = return ()
  where drawFrontOrBackBuffer buf
          | buf == "GL_FRONT" = GL.glDrawBuffer GL.GL_FRONT
          | otherwise = GL.glDrawBuffer GL.GL_BACK

textureModeStuff :: Quake ()
textureModeStuff =
  do checkTextureMode =<< glTextureModeCVar
     checkTextureAlphaMode =<< glTextureAlphaModeCVar
     checkTextureSolidMode =<< glTextureSolidModeCVar

checkTextureMode :: CVarT -> Quake ()
checkTextureMode textureMode
  | textureMode^.cvModified =
      do Image.glTextureMode (textureMode^.cvString)
         CVar.update (textureMode & cvModified .~ False)
  | otherwise = return ()


checkTextureAlphaMode :: CVarT -> Quake ()
checkTextureAlphaMode textureAlphaMode
  | textureAlphaMode^.cvModified =
      do Image.glTextureAlphaMode (textureAlphaMode^.cvString)
         CVar.update (textureAlphaMode & cvModified .~ False)
  | otherwise = return ()

checkTextureSolidMode :: CVarT -> Quake ()
checkTextureSolidMode textureSolidMode
  | textureSolidMode^.cvModified =
      do Image.glTextureSolidMode (textureSolidMode^.cvString)
         CVar.update (textureSolidMode & cvModified .~ False)
  | otherwise = return ()

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
  [ ("hand", "0", Constants.cvarUserInfo .|. Constants.cvarArchive)
  , ("r_norefresh", "0", 0), ("r_fullbright", "0", 0), ("r_drawentities", "1", 0)
  , ("r_drawworld", "1", 0), ("r_novis", "0", 0), ("r_nocull", "0", 0)
  , ("r_lerpmodels", "1", 0), ("r_speeds", "0", 0), ("r_lightlevel", "1", 0)
  , ("gl_nosubimage", "0", 0), ("gl_allow_software", "0", 0)
  , ("gl_particle_min_size", "2", Constants.cvarArchive)
  , ("gl_particle_max_size", "40", Constants.cvarArchive)
  , ("gl_particle_size", "40", Constants.cvarArchive)
  , ("gl_particle_att_a", "0.01", Constants.cvarArchive)
  , ("gl_particle_att_b", "0.0", Constants.cvarArchive)
  , ("gl_particle_att_c", "0.01", Constants.cvarArchive)
  , ("gl_modulate", "1.5", Constants.cvarArchive)
  , ("gl_log", "0", 0), ("gl_bitdepth", "0", 0)
  , ("gl_mode", "3", Constants.cvarArchive) -- 640x480
  , ("gl_lightmap", "0", 0), ("gl_shadows", "0", Constants.cvarArchive)
  , ("gl_dynamic", "1", 0), ("gl_nobind", "0", 0), ("gl_round_down", "1", 0)
  , ("gl_picmip", "0", 0), ("gl_skymip", "0", 0), ("gl_showtris", "0", 0)
  , ("gl_ztrick", "0", 0), ("gl_finish", "0", Constants.cvarArchive)
  , ("gl_clear", "0", 0), ("gl_cull", "1", 0), ("gl_polyblend", "1", 0)
  , ("gl_flashblend", "0", 0), ("gl_playermip", "0", 0)
  , ("gl_monolightmap", "0", 0), ("gl_driver", "opengl32", Constants.cvarArchive)
  , ("gl_texturemode", "GL_LINEAR_MIPMAP_NEAREST", Constants.cvarArchive)
  , ("gl_texturealphamode", "default", Constants.cvarArchive)
  , ("gl_texturesolidmode", "default", Constants.cvarArchive)
  , ("gl_lockpvs", "0", 0), ("gl_vertex_arrays", "1", Constants.cvarArchive)
  , ("gl_ext_swapinterval", "1", Constants.cvarArchive)
  , ("gl_ext_palettedtexture", "0", Constants.cvarArchive)
  , ("gl_ext_multitexture", "1", Constants.cvarArchive)
  , ("gl_ext_pointparameters", "1", Constants.cvarArchive)
  , ("gl_ext_compiled_vertex_array", "1", Constants.cvarArchive)
  , ("gl_drawbuffer", "GL_BACK", 0), ("gl_saturatelighting", "0", 0)
  , ("gl_swapinterval", "0", Constants.cvarArchive)
  , ("gl_3dlabs_broken", "1", Constants.cvarArchive)
  , ("vid_fullscreen", "0", Constants.cvarArchive)
  , ("vid_gamma", "1.0", Constants.cvarArchive)
  , ("vid_ref", "GLFWb", Constants.cvarArchive)
  ]

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("imagelist", Just Image.glImageListF)
  , ("screenshot", Just fastScreenShotF)
  , ("modellist", Just Model.modelListF)
  , ("gl_strings", Just glStringsF)
  ]

rRegister :: Quake ()
rRegister =
  do CVar.initializeCVars initialCVars
     Cmd.addInitialCommands initialCommands

rSetMode :: GLDriver -> Quake Bool
rSetMode glDriver =
  do fullScreen <- vidFullScreenCVar
     glMode <- glModeCVar
     vid <- use (fastRenderAPIGlobals.frVid)
     setMode glDriver fullScreen glMode vid

setMode :: GLDriver -> CVarT -> CVarT -> VidDefT -> Quake Bool
setMode glDriver fullScreen glMode vid =
  do CVar.update (fullScreen & cvModified .~ False)
     CVar.update (glMode & cvModified .~ False)
     err <- (glDriver^.gldSetMode) dim (truncate (glMode^.cvValue)) isFullscreen
     setModeResult glDriver dim glMode err
  where isFullscreen = (fullScreen^.cvValue) > 0
        dim = (vid^.vdWidth, vid^.vdHeight)

setModeResult :: GLDriver -> (Int, Int) -> CVarT -> Int -> Quake Bool
setModeResult glDriver dim glMode err
  | err == Constants.rsErrOk =
      do fastRenderAPIGlobals.frGLState.glsPrevMode .= truncate (glMode^.cvValue)
         return True
  | otherwise = do
      done <- checkFullScreenMode glDriver dim glMode err
      revertModeIfNeeded glDriver dim err done

checkFullScreenMode :: GLDriver -> (Int, Int) -> CVarT -> Int -> Quake Bool
checkFullScreenMode glDriver dim glMode err
  | err == Constants.rsErrInvalidFullscreen =
      do CVar.setValueI "vid_fullscreen" 0
         fullScreen <- vidFullScreenCVar
         CVar.update (fullScreen & cvModified .~ False)
         VID.printf Constants.printAll "ref_gl::R_SetMode() - fullscreen unavailable in this mode\n"
         tryWithoutFullScreen glDriver dim glMode
  | otherwise = return False

tryWithoutFullScreen :: GLDriver -> (Int, Int) -> CVarT -> Quake Bool
tryWithoutFullScreen glDriver dim glMode =
  do err <- (glDriver^.gldSetMode) dim (truncate $ glMode^.cvValue) False
     return (err == Constants.rsErrOk)

revertModeIfNeeded :: GLDriver -> (Int, Int) -> Int -> Bool -> Quake Bool
revertModeIfNeeded _ _ _ True = return True
revertModeIfNeeded glDriver dim err False =
  do prevMode <- use (fastRenderAPIGlobals.frGLState.glsPrevMode)
     checkInvalidMode prevMode err
     tryPrevMode glDriver dim prevMode

checkInvalidMode :: Int -> Int -> Quake ()
checkInvalidMode prevMode err
  | err == Constants.rsErrInvalidMode =
      do CVar.setValueI "gl_mode" prevMode
         glMode <- glModeCVar
         CVar.update (glMode & cvModified .~ False)
         VID.printf Constants.printAll "ref_gl::R_SetMode() - invalid mode\n"
  | otherwise = return ()

tryPrevMode :: GLDriver -> (Int, Int) -> Int -> Quake Bool
tryPrevMode glDriver dim prevMode =
  do err <- (glDriver^.gldSetMode) dim prevMode False
     checkResult err
  where checkResult err
          | err == Constants.rsErrOk = return True
          | otherwise =
              do VID.printf Constants.printAll "ref_gl::R_SetMode() - could not revert to safe mode\n"
                 return False

fastScreenShotF :: XCommandT
fastScreenShotF = error "FastRenderAPI.fastScreenShotF" -- TODO

glStringsF :: XCommandT
glStringsF = error "FastRenderAPI.glStringsF" -- TODO

glSetDefaultState :: GLDriver -> Quake ()
glSetDefaultState glDriver =
  do request glDefaultState
     setImageTextureModes
     setTextureFilterWrapBlend
     Image.glTexEnv GL.GL_REPLACE
     pointParameterSetting =<< use (fastRenderAPIGlobals.frPointParameterEXT)
     palettedTextureSettings
     glUpdateSwapInterval glDriver
     vertexArraySettings =<< use (fastRenderAPIGlobals.frTexture0)

glDefaultState :: QuakeIO ()
glDefaultState =
  do GL.glClearColor 1 0 0.5 0.5
     GL.glCullFace GL.GL_FRONT
     GL.glEnable GL.GL_TEXTURE_2D
     GL.glEnable GL.GL_ALPHA_TEST
     GL.glAlphaFunc GL.GL_GREATER 0.666
     GL.glDisable GL.GL_DEPTH_TEST
     GL.glDisable GL.GL_CULL_FACE
     GL.glDisable GL.GL_BLEND
     GL.glColor4f 1 1 1 1
     GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_FILL
     GL.glShadeModel GL.GL_FLAT

setImageTextureModes :: Quake ()
setImageTextureModes =
  do Image.glTextureMode =<< fmap (^.cvString) glTextureModeCVar
     Image.glTextureAlphaMode =<< fmap (^.cvString) glTextureAlphaModeCVar
     Image.glTextureSolidMode =<< fmap (^.cvString) glTextureSolidModeCVar

setTextureFilterWrapBlend :: Quake ()
setTextureFilterWrapBlend =
  do minFilter <- use (fastRenderAPIGlobals.frGLFilterMin)
     maxFilter <- use (fastRenderAPIGlobals.frGLFilterMax)
     request (setGLParams minFilter maxFilter)
  where setGLParams minFilter maxFilter =
          do GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral minFilter)
             GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral maxFilter)
             GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (fromIntegral GL.GL_REPEAT)
             GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (fromIntegral GL.GL_REPEAT)
             GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA

pointParameterSetting :: Bool -> Quake ()
pointParameterSetting False = return ()
pointParameterSetting True =
  do a <- fmap (^.cvValue) glParticleAttACVar
     b <- fmap (^.cvValue) glParticleAttBCVar
     c <- fmap (^.cvValue) glParticleAttCCVar
     minSize <- fmap (^.cvValue) glParticleMinSizeCVar
     maxSize <- fmap (^.cvValue) glParticleMaxSizeCVar
     request (ppExt a b c minSize maxSize)
  where ppExt a b c minSize maxSize =
          do GL.glEnable GL.GL_POINT_SMOOTH
             GL.glPointParameterfEXT GL.GL_POINT_SIZE_MIN_EXT (realToFrac minSize)
             GL.glPointParameterfEXT GL.GL_POINT_SIZE_MAX_EXT (realToFrac maxSize)
             io (withArray (fmap realToFrac [a, b, c] :: [GL.GLfloat]) $ \ptr ->
               GL.glPointParameterfvEXT GL.GL_DISTANCE_ATTENUATION_EXT ptr)

palettedTextureSettings :: Quake ()
palettedTextureSettings =
  do colorTable <- use (fastRenderAPIGlobals.frColorTableEXT)
     palettedTexture <- fmap (^.cvValue) glExtPalettedTextureCVar
     when (colorTable && palettedTexture /= 0) $ do
       request (GL.glEnable GL.GL_SHARED_TEXTURE_PALETTE_EXT)
       d8to24table <- use (fastRenderAPIGlobals.frd8to24table)
       Image.glSetTexturePalette d8to24table

vertexArraySettings :: Int -> Quake ()
vertexArraySettings texture0 =
  request (
    do GL.glEnableClientState GL.GL_VERTEX_ARRAY
       GL.glClientActiveTextureARB (fromIntegral texture0)
       GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY)
       -- perspective correction (commented out in jake2 source)
       -- gl.glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

rInitParticleTexture :: Quake ()
rInitParticleTexture =
  do pt <- Image.glLoadPic "***particle***" particleTexture 8 8 Constants.itSprite 32
     nt <- Image.glLoadPic "***r_notexture***" noTexture 8 8 Constants.itWall 32
     fastRenderAPIGlobals.frParticleTexture .= pt
     fastRenderAPIGlobals.frNoTexture .= nt

glUpdateSwapInterval :: GLDriver -> Quake ()
glUpdateSwapInterval glDriver =
  updateSwapInterval =<< glSwapIntervalCVar
  where updateSwapInterval swapInterval
          | swapInterval^.cvModified =
              do CVar.update (swapInterval & cvModified .~ False)
                 setSwapInterval swapInterval =<< use (fastRenderAPIGlobals.frGLState.glsStereoEnabled)
          | otherwise = return ()
        setSwapInterval swapInterval False =
          (glDriver^.gldSetSwapInterval) (truncate (swapInterval^.cvValue))
        setSwapInterval _ True = return ()

setGL2D :: VidDefT -> Quake ()
setGL2D vid = request (io set2D)
  where set2D =
          do GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
             GL.glMatrixMode GL.GL_PROJECTION
             GL.glLoadIdentity
             GL.glOrtho 0 (fromIntegral width) (fromIntegral height) 0 (-99999) 99999
             GL.glMatrixMode GL.GL_MODELVIEW
             GL.glLoadIdentity
             GL.glDisable GL.GL_DEPTH_TEST
             GL.glDisable GL.GL_CULL_FACE
             GL.glDisable GL.GL_BLEND
             GL.glEnable GL.GL_ALPHA_TEST
             GL.glColor4f 1 1 1 1
        width = vid^.vdWidth
        height = vid^.vdHeight

rClear :: CVarT -> CVarT -> Quake ()
rClear ztrick clear
  | (ztrick^.cvValue) /= 0 =
      do when ((clear^.cvValue) /= 0) $
           request (io (GL.glClear GL.GL_COLOR_BUFFER_BIT))
         fastRenderAPIGlobals.frTrickFrame += 1
         checkTrickFrame =<< use (fastRenderAPIGlobals.frTrickFrame)
         setDepthRange
  | otherwise =
      do checkClear clear
         fastRenderAPIGlobals.frGLDepthMin .= 0
         fastRenderAPIGlobals.frGLDepthMax .= 1
         request (io (GL.glDepthFunc GL.GL_LEQUAL))
         setDepthRange

checkTrickFrame :: Int -> Quake ()
checkTrickFrame trickFrame
  | trickFrame .&. 1 /= 0 =
      do fastRenderAPIGlobals.frGLDepthMin .= 0
         fastRenderAPIGlobals.frGLDepthMax .= 0.49999
         request (io (GL.glDepthFunc GL.GL_LEQUAL))
  | otherwise =
      do fastRenderAPIGlobals.frGLDepthMin .= 1
         fastRenderAPIGlobals.frGLDepthMax .= 0.5
         request (io (GL.glDepthFunc GL.GL_GEQUAL))

checkClear :: CVarT -> Quake ()
checkClear clear
  | (clear^.cvValue) /= 0 =
      request (io (GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)))
  | otherwise =
      request (io (GL.glClear GL.GL_DEPTH_BUFFER_BIT))

setDepthRange :: Quake ()
setDepthRange =
  do depthMin <- use (fastRenderAPIGlobals.frGLDepthMin)
     depthMax <- use (fastRenderAPIGlobals.frGLDepthMax)
     request (io (GL.glDepthRange (realToFrac depthMin) (realToFrac depthMax)))

rFlash :: Quake ()
rFlash = rPolyBlend

rPolyBlend :: Quake ()
rPolyBlend =
  do polyBlend <- glPolyBlendCVar
     vBlend <- use (fastRenderAPIGlobals.frVBlend)
     unless ((polyBlend^.cvValue) == 0 || (vBlend^._w) == 0) $
       request (doPolyBlend vBlend)
  where doPolyBlend vBlend =
          do GL.glDisable GL.GL_ALPHA_TEST
             GL.glEnable GL.GL_BLEND
             GL.glDisable GL.GL_DEPTH_TEST
             GL.glDisable GL.GL_TEXTURE_2D
             GL.glLoadIdentity
             GL.glRotatef (-90) 1 0 0 -- put Z going up
             GL.glRotatef 90 0 0 1 -- put Z going up
             GL.glColor4f (realToFrac (vBlend^._x)) (realToFrac (vBlend^._y)) (realToFrac (vBlend^._z)) (realToFrac (vBlend^._w))
             GL.glBegin GL.GL_QUADS
             GL.glVertex3f 10 100 100
             GL.glVertex3f 10 (-100) 100
             GL.glVertex3f 10 (-100) (-100)
             GL.glVertex3f 10 100 (-100)
             GL.glEnd
             GL.glDisable GL.GL_BLEND
             GL.glEnable GL.GL_TEXTURE_2D
             GL.glEnable GL.GL_ALPHA_TEST
             GL.glColor4f 1 1 1 1

rSetFrustum :: Quake ()
rSetFrustum =
  do vUp <- use (fastRenderAPIGlobals.frVUp)
     vRight <- use (fastRenderAPIGlobals.frVRight)
     vPn <- use (fastRenderAPIGlobals.frVPn)
     newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
     origin <- use (fastRenderAPIGlobals.frOrigin)
     fastRenderAPIGlobals.frFrustum %= V.imap (updateFrustum (calcNormals vUp vRight vPn newRefDef) origin)
  where calcNormals vUp vRight vPn newRefDef = V.fromList
          [ Math3D.rotatePointAroundVector vUp vPn (0 - (90 - (newRefDef^.rdFovX) / 2))
          , Math3D.rotatePointAroundVector vUp vPn (90 - (newRefDef^.rdFovX) / 2)
          , Math3D.rotatePointAroundVector vRight vPn (90 - (newRefDef^.rdFovY) / 2)
          , Math3D.rotatePointAroundVector vRight vPn (0 - (90 - (newRefDef^.rdFovY) / 2))
          ]
        updateFrustum normals origin idx plane =
          plane & cpNormal .~ normals V.! idx
                & cpType .~ Constants.planeAnyZ
                & cpDist .~ origin `dot` (normals V.! idx)
                & cpSignBits .~ signbitsForPlane (normals V.! idx)

signbitsForPlane :: V3 Float -> Int8
signbitsForPlane normal =
    let a = if (normal^._x) < 0 then 1 else 0
        b = if (normal^._y) < 0 then 2 else 0
        c = if (normal^._z) < 0 then 4 else 0
    in a .|. b .|. c

rSetupGL :: Quake ()
rSetupGL =
  do newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
     vid <- use (fastRenderAPIGlobals.frVid)
     request $
       do GL.glViewport (fromIntegral (newRefDef^.rdX))
                        (fromIntegral ((vid^.vdHeight) - ((newRefDef^.rdY) + (newRefDef^.rdHeight))))
                        (fromIntegral ((newRefDef^.rdX) + (newRefDef^.rdWidth) - (newRefDef^.rdX)))
                        (fromIntegral ((vid^.vdHeight) - (newRefDef^.rdY) - ((vid^.vdHeight) - ((newRefDef^.rdY) + (newRefDef^.rdHeight)))))
          GL.glMatrixMode GL.GL_PROJECTION
          GL.glLoadIdentity
     Mesh.myGLUPerspective (float2Double (newRefDef^.rdFovY)) 
                           (float2Double (fromIntegral (newRefDef^.rdWidth) / fromIntegral (newRefDef^.rdHeight)))
                           4
                           4096
     request $
       do GL.glCullFace GL.GL_FRONT
          GL.glMatrixMode GL.GL_MODELVIEW
          GL.glLoadIdentity
          GL.glRotatef (-90) 1 0 0 -- put Z going up
          GL.glRotatef 90 0 0 1 -- put Z going up
          GL.glRotatef (realToFrac (negate (newRefDef^.rdViewAngles._z))) 1 0 0
          GL.glRotatef (realToFrac (negate (newRefDef^.rdViewAngles._x))) 0 1 0
          GL.glRotatef (realToFrac (negate (newRefDef^.rdViewAngles._y))) 0 0 1
          GL.glTranslatef (realToFrac (negate (newRefDef^.rdViewOrg._x)))
                          (realToFrac (negate (newRefDef^.rdViewOrg._y)))
                          (realToFrac (negate (newRefDef^.rdViewOrg._z)))
     worldMatrix <- request (io (allocaArray 16 $ \ptr ->
       do GL.glGetFloatv GL.GL_MODELVIEW_MATRIX ptr
          peekArray 16 ptr))
     fastRenderAPIGlobals.frWorldMatrix .= worldMatrix
     setCullFace =<< glCullCVar
     request $
       do GL.glDisable GL.GL_BLEND
          GL.glDisable GL.GL_ALPHA_TEST
          GL.glEnable GL.GL_DEPTH_TEST
  where setCullFace glCull
          | (glCull^.cvValue) /= 0 = request (GL.glEnable GL.GL_CULL_FACE)
          | otherwise = request (GL.glDisable GL.GL_CULL_FACE)

rDrawEntitiesOnList :: Quake ()
rDrawEntitiesOnList = do
    drawEntities <- fmap (^.cvValue) drawEntitiesCVar
    unless (drawEntities == 0) $ do
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        -- draw non-transparent first
        mapM_ (drawNonTransparentEntity newRefDef) (fmap Ref [0..(newRefDef^.rdNumEntities)-1])
        -- draw transparent entities
        -- we could sort these if it ever becomes a problem
        request (GL.glDepthMask (fromIntegral GL.GL_FALSE)) -- no z writes
        mapM_ (drawTransparentEntity newRefDef) (fmap Ref [0..(newRefDef^.rdNumEntities)-1])
        request (GL.glDepthMask (fromIntegral GL.GL_TRUE)) -- back to writing

drawNonTransparentEntity :: RefDefT -> Ref EntityT -> Quake ()
drawNonTransparentEntity newRefDef entityRef = do
    error "FastRenderAPI.drawNonTransparentEntity" -- TODO

{-
  where drawNonTransparentEntities :: RefDefT -> Int -> Int -> Quake ()
        drawNonTransparentEntities newRefDef idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let currentEntityRef = (newRefDef^.rdEntities) V.! idx
              currentEntity <- io $ readIORef currentEntityRef
              fastRenderAPIGlobals.frCurrentEntity .= Just currentEntityRef

              if | (currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0 ->
                     drawNonTransparentEntities newRefDef (idx + 1) maxIdx

                 | (currentEntity^.enFlags) .&. Constants.rfBeam /= 0 -> do
                     rDrawBeam currentEntity

                 | otherwise -> do
                     let currentModel = currentEntity^.eModel
                     fastRenderAPIGlobals.frCurrentModel .= currentModel

                     case currentModel of
                       Nothing ->
                         rDrawNullModel
                       Just modelRef -> do
                         model <- io $ readIORef modelRef

                         --io (print ("nontrans model: = " `B.append` (model^.mName)))

                         if | (model^.mType) == RenderAPIConstants.modAlias ->
                                Mesh.rDrawAliasModel currentEntityRef

                            | (model^.mType) == RenderAPIConstants.modBrush ->
                                Surf.rDrawBrushModel currentEntityRef

                            | (model^.mType) == RenderAPIConstants.modSprite ->
                                rDrawSpriteModel currentEntityRef

                            | otherwise ->
                                Com.comError Constants.errDrop "Bad modeltype"

                     drawNonTransparentEntities newRefDef (idx + 1) maxIdx

        drawTransparentEntities :: RefDefT -> Int -> Int -> Quake ()
        drawTransparentEntities newRefDef idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let currentEntityRef = (newRefDef^.rdEntities) V.! idx
              currentEntity <- io $ readIORef currentEntityRef
              fastRenderAPIGlobals.frCurrentEntity .= Just currentEntityRef

              if | (currentEntity^.enFlags) .&. Constants.rfTranslucent == 0 ->
                     drawTransparentEntities newRefDef (idx + 1) maxIdx

                 | (currentEntity^.enFlags) .&. Constants.rfBeam /= 0 -> do
                     rDrawBeam currentEntity

                 | otherwise -> do
                     let currentModel = currentEntity^.eModel
                     fastRenderAPIGlobals.frCurrentModel .= currentModel

                     case currentModel of
                       Nothing ->
                         rDrawNullModel
                       Just modelRef -> do
                         model <- io $ readIORef modelRef

                         --io (print ("trans model: = " `B.append` (model^.mName)))

                         if | (model^.mType) == RenderAPIConstants.modAlias ->
                                Mesh.rDrawAliasModel currentEntityRef

                            | (model^.mType) == RenderAPIConstants.modBrush ->
                                Surf.rDrawBrushModel currentEntityRef

                            | (model^.mType) == RenderAPIConstants.modSprite ->
                                rDrawSpriteModel currentEntityRef

                            | otherwise ->
                                Com.comError Constants.errDrop "Bad modeltype"

                     drawTransparentEntities newRefDef (idx + 1) maxIdx
                     -}
        
drawTransparentEntity newRefDef entityRef = error "FastRenderAPI.drawTransparentEntity" -- TODO

rDrawParticles :: Quake ()
rDrawParticles =
  do extPointParameters <- glExtPointParametersCVar
     pointParameterEXT <- use (fastRenderAPIGlobals.frPointParameterEXT)
     proceedDrawParticles extPointParameters pointParameterEXT
  where proceedDrawParticles extPointParameters pointParameterEXT
          | (extPointParameters^.cvValue) /= 0 && pointParameterEXT =
              error "FastRenderAPI.rDrawParticles" -- TODO
          | otherwise =
              do newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
                 glDrawParticles (newRefDef^.rdNumParticles)

glDrawParticles :: Int -> Quake ()
glDrawParticles numParticles =
  do vUp <- use (fastRenderAPIGlobals.frVUp)
     vRight <- use (fastRenderAPIGlobals.frVRight)
     vPn <- use (fastRenderAPIGlobals.frVPn)
     origin <- use (fastRenderAPIGlobals.frOrigin)
     sourceVertices <- request (use pVertexArray)
     sourceColors <- request (use pColorArray)
     particleTextureRef <- use (fastRenderAPIGlobals.frParticleTexture)
     particle <- readRef particleTextureRef
     Image.glBind (particle^.iTexNum)
     request $
       do GL.glDepthMask GL.GL_FALSE -- no z buffering
          GL.glEnable GL.GL_BLEND
     Image.glTexEnv GL.GL_MODULATE
     request $ io $
       do GL.glBegin GL.GL_TRIANGLES
          mapM_ (drawParticle sourceVertices sourceColors (fmap (* 1.5) vUp) (fmap (* 1.5) vRight) vPn origin) [0..numParticles-1]
          GL.glEnd
          GL.glDisable GL.GL_BLEND
          GL.glColor4f 1 1 1 1
          GL.glDepthMask GL.GL_TRUE -- back to normal z buffering
     Image.glTexEnv GL.GL_REPLACE

drawParticle :: MSV.IOVector Float -> MSV.IOVector Int32 -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> Int -> IO ()
drawParticle sourceVertices sourceColors up right vpn origin idx =
  do originX <- MSV.read sourceVertices (j + 0)
     originY <- MSV.read sourceVertices (j + 1)
     originZ <- MSV.read sourceVertices (j + 2)
     color <- MSV.read sourceColors idx
     let scale = (originX - (origin^._x)) * (vpn^._x)
               + (originY - (origin^._y)) * (vpn^._y)
               + (originZ - (origin^._z)) * (vpn^._z)
         scale' = if scale < 20 then 1 else 1 + scale * 0.004
     GL.glColor4ub (fromIntegral (color .&. 0xFF))
                   (fromIntegral ((color `shiftR` 8) .&. 0xFF))
                   (fromIntegral ((color `shiftR` 16) .&. 0xFF))
                   (fromIntegral ((color `shiftR` 24) .&. 0xFF))
     GL.glTexCoord2f 0.0625 0.0625
     GL.glVertex3f (realToFrac originX) (realToFrac originY) (realToFrac originZ)
     GL.glTexCoord2f 1.0625 0.0625
     GL.glVertex3f (realToFrac (originX + (up^._x) * scale')) (realToFrac (originY + (up^._y) * scale')) (realToFrac (originZ + (up^._z) * scale'))
     GL.glTexCoord2f 0.0625 1.0625
     GL.glVertex3f (realToFrac (originX + (right^._x) * scale')) (realToFrac (originY + (right^._y) * scale')) (realToFrac (originZ + (right^._z) * scale'))
  where j = idx * 3

rSetupFrame :: Quake ()
rSetupFrame = do
    fastRenderAPIGlobals.frFrameCount += 1
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    setAngleVectors newRefDef
    when ((newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel == 0) $ do
        worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
        setCurrentViewCluster newRefDef worldModelRef
    fastRenderAPIGlobals %= (\v -> v & frVBlend .~ (newRefDef^.rdBlend)
                                     & frCBrushPolys .~ 0
                                     & frCAliasPolys .~ 0)
    when ((newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel /= 0) $ do
        vid <- use (fastRenderAPIGlobals.frVid)
        request $ do
            GL.glEnable GL.GL_SCISSOR_TEST
            GL.glClearColor 0.3 0.3 0.3 1.0
            GL.glScissor (fromIntegral (newRefDef^.rdX))
                         (fromIntegral ((vid^.vdHeight) - (newRefDef^.rdHeight) - (newRefDef^.rdY)))
                         (fromIntegral (newRefDef^.rdWidth))
                         (fromIntegral (newRefDef^.rdHeight))
            GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)
            GL.glClearColor 1.0 0.0 0.5 0.5
            GL.glDisable GL.GL_SCISSOR_TEST

setAngleVectors :: RefDefT -> Quake ()
setAngleVectors newRefDef =
    fastRenderAPIGlobals %= (\v -> v & frOrigin .~ (newRefDef^.rdViewOrg)
                                     & frVPn .~ vpn
                                     & frVRight .~ vright
                                     & frVUp .~ vup)
  where
    (vpn, vright, vup) = Math3D.angleVectors (newRefDef^.rdViewAngles) True True True

setCurrentViewCluster :: RefDefT -> Maybe (Ref ModelT) -> Quake ()
setCurrentViewCluster _ Nothing = Com.fatalError "FastRenderAPI.rSetupFrame worldModelRef is Nothing"
setCurrentViewCluster newRefDef (Just worldModelRef) = do
    viewCluster <- use (fastRenderAPIGlobals.frViewCluster)
    viewCluster2 <- use (fastRenderAPIGlobals.frViewCluster2)
    fastRenderAPIGlobals.frOldViewCluster .= viewCluster
    fastRenderAPIGlobals.frOldViewCluster2 .= viewCluster2
    worldModel <- readRef worldModelRef
    leaf <- Model.pointInLeaf (newRefDef^.rdViewOrg) worldModel
    fastRenderAPIGlobals.frViewCluster .= (leaf^.mlCluster)
    fastRenderAPIGlobals.frViewCluster2 .= (leaf^.mlCluster)
    proceedSetCurrentViewCluster worldModel leaf
  where
    proceedSetCurrentViewCluster worldModel leaf
        | (leaf^.mlContents) == 0 = do
            leaf' <- Model.pointInLeaf ((newRefDef^.rdViewOrg) & _z -~ 16) worldModel
            when ((leaf'^.mlContents) .&. Constants.contentsSolid == 0 && (leaf'^.mlCluster) /= (leaf^.mlCluster)) $
                fastRenderAPIGlobals.frViewCluster2 .= (leaf'^.mlCluster)
        | otherwise = do
            leaf' <- Model.pointInLeaf ((newRefDef^.rdViewOrg) & _z +~ 16) worldModel
            when ((leaf'^.mlContents) .&. Constants.contentsSolid == 0 && (leaf'^.mlCluster) /= (leaf^.mlCluster)) $
                fastRenderAPIGlobals.frViewCluster2 .= (leaf'^.mlCluster)

particleTexture :: SV.Vector Word8
particleTexture =
  SV.fromList [ 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255, 255, 255, 255, 255, 255
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255, 255
              , 255, 255, 255, 255, 255, 255, 255, 255
              , 255, 255, 255, 255, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255, 255
              , 255, 255, 255, 255, 255, 255, 255, 255
              , 255, 255, 255, 255, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255, 255, 255, 255, 255, 255
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              , 255, 255, 255,   0, 255, 255, 255,   0
              ]

noTexture :: SV.Vector Word8
noTexture =
  SV.fromList [   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255,   0, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ,   0, 0, 0, 255, 255, 0, 0, 255
              , 255, 0, 0, 255, 255, 0, 0, 255
              ]
