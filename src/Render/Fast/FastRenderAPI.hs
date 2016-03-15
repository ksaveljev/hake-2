{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.FastRenderAPI
  ( fastRenderAPI
  ) where

import           Client.VidDefT
import {-# SOURCE #-} qualified Client.VID as VID
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QuakeState
import qualified Render.Fast.Draw as Draw
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Warp as Warp
import           Render.GLConfigT
import           Render.GLStateT
import           Render.OpenGL.GLDriver
import           Types

import           Control.Exception (handle, IOException)
import           Control.Lens (use, (^.), (.=), (&), (.~))
import           Control.Monad (void, when, unless)
import           Data.Bits ((.|.), (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower, toUpper)
import           Data.Maybe (fromMaybe)
import           Foreign.Ptr (nullPtr, castPtr)
import           Foreign.Marshal.Array (withArray)
import qualified Graphics.GL as GL
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
fastRenderFrame = error "FastRenderAPI.fastRenderFrame" -- TODO

fastSetPalette :: Maybe B.ByteString -> Quake ()
fastSetPalette = error "FastRenderAPI.fastSetPalette" -- TODO

fastBeginFrame :: GLDriver -> Float -> Quake ()
fastBeginFrame = error "FastRenderAPI.fastBeginFrame" -- TODO

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

particleTexture :: B.ByteString
particleTexture =
    B.pack [ 255, 255, 255,   0, 255, 255, 255,   0
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

noTexture :: B.ByteString
noTexture =
    B.pack [   0, 0, 0, 255,   0, 0, 0, 255
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
