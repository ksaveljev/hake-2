{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.FastRenderAPI where

import Control.Exception (handle, IOException)
import Control.Lens ((.=), (^.), use, zoom, (+=), preuse, ix)
import Control.Monad (void, when, liftM, unless)
import Data.Bits ((.|.), (.&.), shiftR, shiftL)
import Data.Char (toLower, toUpper)
import Data.Int (Int8, Int32)
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Foreign.Marshal.Array (withArray, allocaArray, peekArray)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import GHC.Float (float2Double)
import Linear (V3(..), _x, _y, _z, _w, dot)
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Debug.Trace as DT
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Types
import Game.CPlaneT
import QuakeState
import CVarVariables
import QCommon.XCommandT
import Render.OpenGL.GLDriver
import qualified Constants
import qualified Client.VID as VID
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Render.Fast.Draw as Draw
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Light as Light
import qualified Render.Fast.Mesh as Mesh
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Surf as Surf
import qualified Render.Fast.Warp as Warp
import qualified Render.OpenGL.QGLConstants as QGLConstants
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Util.Math3D as Math3D

fastRenderAPI :: RenderAPI
fastRenderAPI =
    RenderAPI { _rInit              = fastInit
              , _rInit2             = fastInit2
              , _rShutdown          = fastShutdown
              , _rBeginRegistration = \_ -> Model.rBeginRegistration
              , _rRegisterModel     = \_ -> Model.rRegisterModel
              , _rRegisterSkin      = \_ -> Image.rRegisterSkin
              , _rDrawFindPic       = \_ -> Draw.findPic
              , _rSetSky            = \_ -> Warp.rSetSky
              , _rEndRegistration   = \_ -> Model.rEndRegistration
              , _rRenderFrame       = \_ -> fastRenderFrame
              , _rDrawGetPicSize    = \_ -> Draw.getPicSize
              , _rDrawPic           = \_ -> Draw.drawPic
              , _rDrawStretchPic    = \_ -> Draw.stretchPic
              , _rDrawChar          = \_ -> Draw.drawChar
              , _rDrawTileClear     = DT.trace "FastRenderAPI.rDrawTileClear" undefined -- TODO
              , _rDrawFill          = \_ -> Draw.fill
              , _rDrawFadeScreen    = \_ -> Draw.fadeScreen
              , _rDrawStretchRaw    = \_ -> Draw.stretchRaw
              , _rSetPalette        = \_ -> fastSetPalette
              , _rBeginFrame        = fastBeginFrame
              , _glScreenShotF      = DT.trace "FastRenderAPI.glScreenShotF" undefined -- TODO
              }

fastInit :: GLDriver -> Int -> Int -> Quake Bool
fastInit glDriver _ _ = do
    VID.printf Constants.printAll ("ref_gl version: " `B.append` RenderAPIConstants.refVersion `B.append` "\n")

    Image.getPalette

    rRegister

    -- set our "safe" modes
    fastRenderAPIGlobals.frGLState.glsPrevMode .= 3

    ok <- rSetMode glDriver -- (glDriver^.gldSetMode)

    -- create the window and set up the context
    if not ok
      then do
        VID.printf Constants.printAll "ref_gl::R_Init() - could not R_SetMode()\n"
        return False
      else
        return True

rRegister :: Quake ()
rRegister = do
    void $ CVar.get "hand" "0" (Constants.cvarUserInfo .|. Constants.cvarArchive)
    void $ CVar.get "r_norefresh" "0" 0
    void $ CVar.get "r_fullbright" "0" 0
    void $ CVar.get "r_drawentities" "1" 0
    void $ CVar.get "r_drawworld" "1" 0
    void $ CVar.get "r_novis" "0" 0
    void $ CVar.get "r_nocull" "0" 0
    void $ CVar.get "r_lerpmodels" "1" 0
    void $ CVar.get "r_speeds" "0" 0

    void $ CVar.get "r_lightlevel" "1" 0

    void $ CVar.get "gl_nosubimage" "0" 0
    void $ CVar.get "gl_allow_software" "0" 0

    void $ CVar.get "gl_particle_min_size" "2" Constants.cvarArchive
    void $ CVar.get "gl_particle_max_size" "40" Constants.cvarArchive
    void $ CVar.get "gl_particle_size" "40" Constants.cvarArchive
    void $ CVar.get "gl_particle_att_a" "0.01" Constants.cvarArchive
    void $ CVar.get "gl_particle_att_b" "0.0" Constants.cvarArchive
    void $ CVar.get "gl_particle_att_c" "0.01" Constants.cvarArchive

    void $ CVar.get "gl_modulate" "1.5" Constants.cvarArchive
    void $ CVar.get "gl_log" "0" 0
    void $ CVar.get "gl_bitdepth" "0" 0
    void $ CVar.get "gl_mode" "3" Constants.cvarArchive -- 640x480
    void $ CVar.get "gl_lightmap" "0" 0
    void $ CVar.get "gl_shadows" "0" Constants.cvarArchive
    void $ CVar.get "gl_dynamic" "1" 0
    void $ CVar.get "gl_nobind" "0" 0
    void $ CVar.get "gl_round_down" "1" 0
    void $ CVar.get "gl_picmip" "0" 0
    void $ CVar.get "gl_skymip" "0" 0
    void $ CVar.get "gl_showtris" "0" 0
    void $ CVar.get "gl_ztrick" "0" 0
    void $ CVar.get "gl_finish" "0" Constants.cvarArchive
    void $ CVar.get "gl_clear" "0" 0
    void $ CVar.get "gl_cull" "1" 0
    void $ CVar.get "gl_polyblend" "1" 0
    void $ CVar.get "gl_flashblend" "0" 0
    void $ CVar.get "gl_playermip" "0" 0
    void $ CVar.get "gl_monolightmap" "0" 0
    void $ CVar.get "gl_driver" "opengl32" Constants.cvarArchive
    void $ CVar.get "gl_texturemode" "GL_LINEAR_MIPMAP_NEAREST" Constants.cvarArchive
    void $ CVar.get "gl_texturealphamode" "default" Constants.cvarArchive
    void $ CVar.get "gl_texturesolidmode" "default" Constants.cvarArchive
    void $ CVar.get "gl_lockpvs" "0" 0

    void $ CVar.get "gl_vertex_arrays" "1" Constants.cvarArchive

    void $ CVar.get "gl_ext_swapinterval" "1" Constants.cvarArchive
    void $ CVar.get "gl_ext_palettedtexture" "0" Constants.cvarArchive
    void $ CVar.get "gl_ext_multitexture" "1" Constants.cvarArchive
    void $ CVar.get "gl_ext_pointparameters" "1" Constants.cvarArchive
    void $ CVar.get "gl_ext_compiled_vertex_array" "1" Constants.cvarArchive

    void $ CVar.get "gl_drawbuffer" "GL_BACK" 0
    void $ CVar.get "gl_swapinterval" "0" Constants.cvarArchive

    void $ CVar.get "gl_saturatelighting" "0" 0

    void $ CVar.get "gl_3dlabs_broken" "1" Constants.cvarArchive

    void $ CVar.get "vid_fullscreen" "0" Constants.cvarArchive
    void $ CVar.get "vid_gamma" "1.0" Constants.cvarArchive
    void $ CVar.get "vid_ref" "GLFWb" Constants.cvarArchive 

    Cmd.addCommand "imagelist" (Just Image.glImageListF)
    Cmd.addCommand "screenshot" (Just fastScreenShotF)
    Cmd.addCommand "modellist" (Just Model.modelListF)
    Cmd.addCommand "gl_strings" (Just glStringsF)

rSetMode :: GLDriver -> Quake Bool
rSetMode glDriver = do
    fullScreen <- vidFullScreenCVar
    glMode <- glModeCVar

    let isFullscreen = (fullScreen^.cvValue) > 0

    CVar.update fullScreen { _cvModified = False }
    CVar.update glMode { _cvModified = False }

    vid <- use $ fastRenderAPIGlobals.frVid
    let dim = (vid^.vdWidth, vid^.vdHeight)

    err <- (glDriver^.gldSetMode) dim (truncate $ glMode^.cvValue) isFullscreen

    if err == RenderAPIConstants.rsErrOk
      then do
        fastRenderAPIGlobals.frGLState.glsPrevMode .= truncate (glMode^.cvValue)
        return True
      else do
        done <- if err == RenderAPIConstants.rsErrInvalidFullscreen
                  then do
                    CVar.setValueI "vid_fullscreen" 0
                    vidFullScreenCVar >>= \v -> CVar.update v { _cvModified = False }
                    VID.printf Constants.printAll "ref_gl::R_SetMode() - fullscreen unavailable in this mode\n"
                    err' <- (glDriver^.gldSetMode) dim (truncate $ glMode^.cvValue) False
                    if err' == RenderAPIConstants.rsErrOk
                      then return True
                      else return False
                  else return False

        if done
          then return True
          else do
            prevMode <- use $ fastRenderAPIGlobals.frGLState.glsPrevMode

            when (err == RenderAPIConstants.rsErrInvalidMode) $ do
              CVar.setValueI "gl_mode" prevMode
              glModeCVar >>= \v -> CVar.update v { _cvModified = False }
              VID.printf Constants.printAll "ref_gl::R_SetMode() - invalid mode\n"

            -- try setting it back to something safe
            err' <- (glDriver^.gldSetMode) dim prevMode False

            if err' /= RenderAPIConstants.rsErrOk
              then do
                VID.printf Constants.printAll "ref_gl::R_SetMode() - could not revert to safe mode\n"
                return False
              else return True

glStringsF :: XCommandT
glStringsF =
  XCommandT "FastRenderAPI.glStringsF" (do
    io (putStrLn "FastRenderAPI.glStringsF") >> undefined -- TODO
  )

fastScreenShotF :: XCommandT
fastScreenShotF =
  XCommandT "FastRenderAPI.fastScreenShotF" (do
    io (putStrLn "FastRenderAPI.fastScreenShotF") >> undefined -- TODO
  )

fastInit2 :: GLDriver -> Quake Bool
fastInit2 glDriver = do
    VID.menuInit

    -- get our various GL strings
    vendor <- io $ getGLString GL.GL_VENDOR
    renderer <- io $ getGLString GL.GL_RENDERER
    version <- io $ getGLString GL.GL_VERSION
    extensions <- io $ getGLString GL.GL_EXTENSIONS

    zoom (fastRenderAPIGlobals.frGLConfig) $ do
      glcVendorString .= vendor
      glcRendererString .= renderer
      glcVersionString .= version
      glcExtensionsString .= extensions
      glcVersion .= (let v = B.take 3 version
                     in fromMaybe 1.1 (readMaybe (BC.unpack v))) -- IMPROVE?

    VID.printf Constants.printAll $ "GL_VENDOR: " `B.append` vendor `B.append` "\n"
    VID.printf Constants.printAll $ "GL_RENDERER: " `B.append` renderer `B.append` "\n"
    VID.printf Constants.printAll $ "GL_VERSION: " `B.append` version `B.append` "\n"
    VID.printf Constants.printAll $ "GL_EXTENSIONS: " `B.append` extensions `B.append` "\n"

    let rendererBuffer = BC.map toLower renderer
        vendorBuffer = BC.map toLower vendor

    let rendererInt = if | "voodoo"   `BC.isInfixOf` rendererBuffer ->
                             if "rush" `BC.isInfixOf`rendererBuffer 
                               then RenderAPIConstants.glRendererVoodooRush
                               else RenderAPIConstants.glRendererVoodoo
                         | "sgi"      `BC.isInfixOf` vendorBuffer -> RenderAPIConstants.glRendererSGI
                         | "permedia" `BC.isInfixOf` rendererBuffer -> RenderAPIConstants.glRendererPerMedia2
                         | "glint"    `BC.isInfixOf` rendererBuffer -> RenderAPIConstants.glRendererGlintMX
                         | "glzicd"   `BC.isInfixOf` rendererBuffer -> RenderAPIConstants.glRendererRealizm
                         | "gdi"      `BC.isInfixOf` rendererBuffer -> RenderAPIConstants.glRendererMCD
                         | "pcx2"     `BC.isInfixOf` rendererBuffer -> RenderAPIConstants.glRendererPCX2
                         | "verite"   `BC.isInfixOf` rendererBuffer -> RenderAPIConstants.glRendererRendition
                         | otherwise -> RenderAPIConstants.glRendererOther

    fastRenderAPIGlobals.frGLConfig.glcRenderer .= rendererInt

    monoLightMapValue <- liftM (BC.map toUpper . (^.cvString)) glMonoLightMapCVar
    when (B.length monoLightMapValue < 2 || BC.index monoLightMapValue 1 /= 'F') $ do
      if rendererInt == RenderAPIConstants.glRendererPerMedia2
        then do
          void $ CVar.set "gl_monolightmap" "A"
          VID.printf Constants.printAll "...using GL_monolightmap 'a'\n"
        else
          void $ CVar.set "gl_monolightmap" "0"

    -- power vr can't have anything stay in the framebuffer, so
    -- the screen needs to redraw the tiled background every frame
    if rendererInt .&. RenderAPIConstants.glRendererPowerVR /= 0
      then void $ CVar.set "scr_drawall" "1"
      else void $ CVar.set "scr_drawall" "0"

    -- MCD has buffering issues
    when (rendererInt == RenderAPIConstants.glRendererMCD) $
      CVar.setValueI "gl_finish" 1

    allow <- if rendererInt .&. RenderAPIConstants.glRenderer3DLabs /= 0
               then do
                 brokenValue <- liftM (^.cvValue) gl3DLabsBrokenCVar
                 if brokenValue /= 0
                   then return False
                   else return True
             else return True
                   
    fastRenderAPIGlobals.frGLConfig.glcAllowCds .= allow

    VID.printf Constants.printAll (if allow
                                     then "...allowing CDS\n"
                                     else "...disabling CDS\n")

    -- grab extensions
    cva <- if "GL_EXT_compiled_vertex_array" `BC.isInfixOf` extensions ||
             "GL_SGI_compiled_vertex_array" `BC.isInfixOf` extensions
             then do
               VID.printf Constants.printAll "...enabling GL_EXT_compiled_vertex_array\n"
               liftM (^.cvValue) glExtCompiledVertexArrayCVar >>= \v ->
                if v /= 0 then return True else return False
             else do
               VID.printf Constants.printAll "...GL_EXT_compiled_vertex_array not found\n"
               return False

    fastRenderAPIGlobals.frLockArraysEXT .= cva

    si <- if "WGL_EXT_swap_control" `BC.isInfixOf` extensions
            then do
              VID.printf Constants.printAll "...enabling WGL_EXT_swap_control\n"
              return True
            else do
              VID.printf Constants.printAll "...WGL_EXT_swap_control not found\n"
              return False

    fastRenderAPIGlobals.frSwapIntervalEXT .= si

    pp <- if "GL_EXT_point_parameters" `BC.isInfixOf` extensions
            then do
              liftM (^.cvValue) glExtPointParametersCVar >>= \v ->
                if v /= 0
                  then do
                    VID.printf Constants.printAll "...using GL_EXT_point_parameters\n"
                    return True
                  else do
                    VID.printf Constants.printAll "...ignoring GL_EXT_point_parameters\n"
                    return False
            else do
              VID.printf Constants.printAll "...GL_EXT_point_parameters not found\n"
              return False

    fastRenderAPIGlobals.frPointParameterEXT .= pp

    colorTable <- use $ fastRenderAPIGlobals.frColorTableEXT
    ct <- if not colorTable &&
             "GL_EXT_paletted_texture" `BC.isInfixOf` extensions &&
             "GL_EXT_shared_texture_palette" `BC.isInfixOf` extensions
            then do
              liftM (^.cvValue) glExtPalettedTextureCVar >>= \v ->
                if v /= 0
                  then do
                    VID.printf Constants.printAll "...using GL_EXT_shared_texture_palette\n"
                    return True
                  else do
                    VID.printf Constants.printAll "...ignoring GL_EXT_shared_texture_palette\n"
                    return False
            else do
              VID.printf Constants.printAll "...GL_EXT_shared_texture_palette not found\n"
              return False

    fastRenderAPIGlobals.frColorTableEXT .= ct

    cat <- if "GL_ARB_multitexture" `BC.isInfixOf` extensions
             then do
               -- check if the extension really exists
               ok <- io $ handle (\(_ :: IOException) -> return False) $ do
                 GL.glClientActiveTextureARB (fromIntegral QGLConstants.glTexture0ARB)
                 -- seems to work correctly
                 return True

               if ok
                 then do
                   VID.printf Constants.printAll "...using GL_ARB_multitexture\n"
                   fastRenderAPIGlobals.frTexture0 .= QGLConstants.glTexture0ARB
                   fastRenderAPIGlobals.frTexture1 .= QGLConstants.glTexture1ARB
                   return True
                 else
                   return False
             else do
               VID.printf Constants.printAll "...GL_ARB_multitexture not found\n"
               return False

    fastRenderAPIGlobals.frActiveTextureARB .= cat

    if not cat
      then do
        VID.printf Constants.printAll "Missing multi-texturing!\n"
        return False
      else do
        glSetDefaultState glDriver
        Image.glInitImages
        Model.modInit
        rInitParticleTexture
        Draw.initLocal

        err <- io $ GL.glGetError
        unless (err == GL.GL_NO_ERROR) $
          VID.printf Constants.printAll "gl.glGetError() = TODO" -- TODO: add error information

        glDriver^.gldEndFrame
        return True

  where getGLString :: GL.GLenum -> IO B.ByteString
        getGLString n = GL.glGetString n >>= maybeNullPtr (return "") (B.packCString . castPtr)

        maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
        maybeNullPtr n f ptr | ptr == nullPtr = n
                             | otherwise      = f ptr

glSetDefaultState :: GLDriver -> Quake ()
glSetDefaultState glDriver = do
    GL.glClearColor 1 0 0.5 0.5
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

    liftM (^.cvString) glTextureModeCVar >>= Image.glTextureMode
    liftM (^.cvString) glTextureAlphaModeCVar >>= Image.glTextureAlphaMode
    liftM (^.cvString) glTextureSolidModeCVar >>= Image.glTextureSolidMode

    minFilter <- use $ fastRenderAPIGlobals.frGLFilterMin
    maxFilter <- use $ fastRenderAPIGlobals.frGLFilterMax

    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral minFilter)
    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral maxFilter)

    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (fromIntegral GL.GL_REPEAT)
    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (fromIntegral GL.GL_REPEAT)

    GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA

    Image.glTexEnv GL.GL_REPLACE

    ppExt <- use $ fastRenderAPIGlobals.frPointParameterEXT

    when ppExt $ do
      a <- liftM (^.cvValue) glParticleAttACVar
      b <- liftM (^.cvValue) glParticleAttBCVar
      c <- liftM (^.cvValue) glParticleAttCCVar
      minSize <- liftM (^.cvValue) glParticleMinSizeCVar
      maxSize <- liftM (^.cvValue) glParticleMaxSizeCVar

      let arr :: [GL.GLfloat] = fmap realToFrac [a, b, c]

      GL.glEnable GL.GL_POINT_SMOOTH
      GL.glPointParameterfEXT GL.GL_POINT_SIZE_MIN_EXT (realToFrac minSize)
      GL.glPointParameterfEXT GL.GL_POINT_SIZE_MAX_EXT (realToFrac maxSize)
      io $ withArray arr $ \ptr ->
        GL.glPointParameterfvEXT GL.GL_DISTANCE_ATTENUATION_EXT ptr

    ctExt <- use $ fastRenderAPIGlobals.frColorTableEXT
    pt <- liftM (^.cvValue) glExtPalettedTextureCVar

    when (ctExt && pt /= 0) $ do
      io $ GL.glEnable GL.GL_SHARED_TEXTURE_PALETTE_EXT
      d8to24table <- use $ fastRenderAPIGlobals.frd8to24table
      Image.glSetTexturePalette d8to24table

    glUpdateSwapInterval glDriver

    -- vertex array extension
    t0 <- use $ fastRenderAPIGlobals.frTexture0

    GL.glEnableClientState GL.GL_VERTEX_ARRAY
    GL.glClientActiveTextureARB (fromIntegral t0)
    GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY

    -- perspective correction (commented out in jake2 source)
    -- gl.glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

dotTexture :: B.ByteString
dotTexture =
    B.pack [ 0,0,0,0,0,0,0,0,
             0,0,1,1,0,0,0,0,
             0,1,1,1,1,0,0,0,
             0,1,1,1,1,0,0,0,
             0,0,1,1,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0
           ]

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

-- here we skipped the generation of particleTexture and noTexture
-- and simply precalculated it beforehand
rInitParticleTexture :: Quake ()
rInitParticleTexture = do
    -- particle texture
    pt <- Image.glLoadPic "***particle***" particleTexture 8 8 RenderAPIConstants.itSprite 32
    -- also use this for bad textures, but without alpha
    nt <- Image.glLoadPic "***r_notexture***" noTexture 8 8 RenderAPIConstants.itWall 32

    fastRenderAPIGlobals.frParticleTexture .= pt
    fastRenderAPIGlobals.frNoTexture .= nt

glUpdateSwapInterval :: GLDriver -> Quake ()
glUpdateSwapInterval glDriver = do
    glSwapInterval <- glSwapIntervalCVar

    when (glSwapInterval^.cvModified) $ do
      CVar.update glSwapInterval { _cvModified = False }
      stereoEnabled <- use $ fastRenderAPIGlobals.frGLState.glsStereoEnabled
      unless stereoEnabled $
        (glDriver^.gldSetSwapInterval) (truncate $ glSwapInterval^.cvValue)

fastBeginFrame :: GLDriver -> Float -> Quake ()
fastBeginFrame glDriver cameraSeparation = do
    use (fastRenderAPIGlobals.frVid) >>= \vid ->
      zoom (fastRenderAPIGlobals.frVid) $ do
        vdWidth .= (vid^.vdNewWidth)
        vdHeight .= (vid^.vdNewHeight)

    fastRenderAPIGlobals.frGLState.glsCameraSeparation .= cameraSeparation

    -- change modes if necessary
    vidFullScreenModified <- liftM (^.cvModified) vidFullScreenCVar
    glModeModified <- liftM (^.cvModified) glModeCVar

    when (glModeModified || vidFullScreenModified) $ do
      -- FIXME: only restart if CDS is required
      Just ref <- CVar.get "vid_ref" "GLFWb" 0
      CVar.update ref { _cvModified = True }

    glLog <- glLogCVar

    when (glLog^.cvModified) $ do
      (glDriver^.gldEnableLogging) ((glLog^.cvValue) /= 0)
      CVar.update glLog { _cvModified = False }

    when ((glLog^.cvValue) /= 0) $
      glDriver^.gldLogNewFrame

    -- update 3Dfx gamma -- it is expected that a user will do a vid_restart
    -- after tweaking this value
    vidGamma <- vidGammaCVar

    when (vidGamma^.cvModified) $ do
      CVar.update vidGamma { _cvModified = False }

      r <- use $ fastRenderAPIGlobals.frGLConfig.glcRenderer
      when (r .&. RenderAPIConstants.glRendererVoodoo /= 0) $ do
        -- jake2 skips implementing this?
        VID.printf Constants.printDeveloper "gamma anpassung fuer VOODOO nicht gesetzt"

    (glDriver^.gldBeginFrame) cameraSeparation

    -- go into 2D mode
    setGL2D

    -- draw buffer stuff
    drawBufferStuff

    -- texturemode stuff
    textureModeStuff

    -- swapinterval stuff
    glUpdateSwapInterval glDriver

    -- clear screen if desired
    rClear

  where drawBufferStuff :: Quake ()
        drawBufferStuff = do
          drawBuffer <- glDrawBufferCVar

          when (drawBuffer^.cvModified) $ do
            CVar.update drawBuffer { _cvModified = False }

            glState <- use $ fastRenderAPIGlobals.frGLState

            when ((glState^.glsCameraSeparation) == 0 || not (glState^.glsStereoEnabled)) $ do
              if BC.map toUpper (drawBuffer^.cvString) == "GL_FRONT"
                then GL.glDrawBuffer GL.GL_FRONT
                else GL.glDrawBuffer GL.GL_BACK

        textureModeStuff :: Quake ()
        textureModeStuff = do
          textureMode <- glTextureModeCVar

          when (textureMode^.cvModified) $ do
            Image.glTextureMode (textureMode^.cvString)
            CVar.update textureMode { _cvModified = False }

          textureAlphaMode <- glTextureAlphaModeCVar

          when (textureAlphaMode^.cvModified) $ do
            Image.glTextureAlphaMode (textureAlphaMode^.cvString)
            CVar.update textureAlphaMode { _cvModified = False }

          textureSolidMode <- glTextureSolidModeCVar

          when (textureSolidMode^.cvModified) $ do
            Image.glTextureSolidMode (textureSolidMode^.cvString)
            CVar.update textureSolidMode { _cvModified = False }

rClear :: Quake ()
rClear = do
    ztrickValue <- liftM (^.cvValue) glZTrickCVar
    clearValue <- liftM (^.cvValue) glClearCVar

    if ztrickValue /= 0
      then do
        when (clearValue /= 0) $
          GL.glClear GL.GL_COLOR_BUFFER_BIT

        fastRenderAPIGlobals.frTrickFrame += 1
        trickFrame <- use $ fastRenderAPIGlobals.frTrickFrame

        if trickFrame .&. 1 /= 0
          then do
            fastRenderAPIGlobals.frGLDepthMin .= 0
            fastRenderAPIGlobals.frGLDepthMax .= 0.49999
            GL.glDepthFunc GL.GL_LEQUAL
          else do
            fastRenderAPIGlobals.frGLDepthMin .= 1
            fastRenderAPIGlobals.frGLDepthMax .= 0.5
            GL.glDepthFunc GL.GL_GEQUAL
      else do
        if clearValue /= 0
          then GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)
          else GL.glClear GL.GL_DEPTH_BUFFER_BIT

        fastRenderAPIGlobals.frGLDepthMin .= 0
        fastRenderAPIGlobals.frGLDepthMax .= 1
        GL.glDepthFunc GL.GL_LEQUAL

    depthMin <- use $ fastRenderAPIGlobals.frGLDepthMin
    depthMax <- use $ fastRenderAPIGlobals.frGLDepthMax
    GL.glDepthRange (realToFrac depthMin) (realToFrac depthMax)

setGL2D :: Quake ()
setGL2D = do
    vid <- use $ fastRenderAPIGlobals.frVid
    let width = vid^.vdWidth
        height = vid^.vdHeight

    -- set 2D virtual screen size
    GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
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

fastRenderFrame :: RefDefT -> Quake ()
fastRenderFrame fd = do
    renderView fd
    setLightLevel
    setGL2D

{-
- R_RenderView
- r_newrefdef must be set before the first call
-}
renderView :: RefDefT -> Quake ()
renderView fd = do
    noRefreshValue <- liftM (^.cvValue) noRefreshCVar

    when (noRefreshValue == 0) $ do
      fastRenderAPIGlobals.frNewRefDef .= fd

      {-
      when (isNothing fd) $
        Com.comError Constants.errDrop "R_RenderView: refdef_t fd is null"
        -}

      worldModel <- use $ fastRenderAPIGlobals.frWorldModel

      when (isNothing worldModel && ((fd^.rdRdFlags) .&. Constants.rdfNoWorldModel) == 0) $
        Com.comError Constants.errDrop "R_RenderView: NULL worldmodel"

      speedsValue <- liftM (^.cvValue) speedsCVar
      when (speedsValue /= 0) $ do
        fastRenderAPIGlobals.frCBrushPolys .= 0
        fastRenderAPIGlobals.frCAliasPolys .= 0

      Light.rPushDLights

      glFinishValue <- liftM (^.cvValue) glFinishCVar
      when (glFinishValue /= 0) $
        GL.glFinish

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

      -- TODO: add VID.printf like in jake2 to print some info

setLightLevel :: Quake ()
setLightLevel = do
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef

    when ((newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel == 0) $ do
      -- save off light value for server to look at (BIG HACK!)
      light <- Light.rLightPoint (newRefDef^.rdViewOrg)

      -- pick the greatest component, which should be the same
      -- as the mono value returned by software
      let v = if (light^._x) > (light^._y)
                then do
                  if (light^._x) > (light^._z)
                    then 150 * (light^._x)
                    else 150 * (light^._z)
                else do
                  if (light^._y) > (light^._z)
                    then 150 * (light^._y)
                    else 150 * (light^._z)

      lightLevel <- clLightLevelCVar
      CVar.update lightLevel { _cvValue = v }

rSetupFrame :: Quake ()
rSetupFrame = do
    fastRenderAPIGlobals.frFrameCount += 1

    -- build the transformation matrix for the given view angles
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
    -- io $ print "SETUP FRAME"
    -- io $ print ("viewAngles = " ++ show (newRefDef^.rdViewAngles))
    let (Just vpn, Just vright, Just vup) = Math3D.angleVectors (newRefDef^.rdViewAngles) True True True

    zoom fastRenderAPIGlobals $ do
      frOrigin .= (newRefDef^.rdViewOrg)
      frVPn .= vpn
      frVRight .= vright
      frVUp .= vup

    -- current viewcluster
    when ((newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel == 0) $ do
      viewCluster <- use $ fastRenderAPIGlobals.frViewCluster
      viewCluster2 <- use $ fastRenderAPIGlobals.frViewCluster2

      fastRenderAPIGlobals.frOldViewCluster .= viewCluster
      fastRenderAPIGlobals.frOldViewCluster2 .= viewCluster2

      Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
      worldModel <- io $ readIORef worldModelRef

      leaf <- Model.pointInLeaf (newRefDef^.rdViewOrg) worldModel
      let newViewCluster = leaf^.mlCluster

      fastRenderAPIGlobals.frViewCluster .= newViewCluster
      fastRenderAPIGlobals.frViewCluster2 .= newViewCluster

      -- check above and below so crossing solid water doesn't draw wrong
      if (leaf^.mlContents) == 0 -- look down a bit
        then do
          let V3 a b c = newRefDef^.rdViewOrg
              temp = V3 a b (c - 16)

          leaf' <- Model.pointInLeaf temp worldModel

          when ((leaf'^.mlContents) .&. Constants.contentsSolid == 0 && (leaf'^.mlCluster) /= newViewCluster) $
            fastRenderAPIGlobals.frViewCluster2 .= (leaf'^.mlCluster)

        else do -- look up a bit
          let V3 a b c = newRefDef^.rdViewOrg
              temp = V3 a b (c + 16)

          leaf' <- Model.pointInLeaf temp worldModel

          when ((leaf'^.mlContents) .&. Constants.contentsSolid == 0 && (leaf'^.mlCluster) /= newViewCluster) $
            fastRenderAPIGlobals.frViewCluster2 .= (leaf'^.mlCluster)

    zoom fastRenderAPIGlobals $ do
      frVBlend .= (newRefDef^.rdBlend)
      frCBrushPolys .= 0
      frCAliasPolys .= 0

    -- clear out the portion of the screen that the NOWORLDMODEL defines
    when ((newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel /= 0) $ do
      vid <- use $ fastRenderAPIGlobals.frVid

      GL.glEnable GL.GL_SCISSOR_TEST
      GL.glClearColor 0.3 0.3 0.3 1.0
      GL.glScissor (fromIntegral $ newRefDef^.rdX)
                   (fromIntegral $ (vid^.vdHeight) - (newRefDef^.rdHeight) - (newRefDef^.rdY))
                   (fromIntegral $ newRefDef^.rdWidth)
                   (fromIntegral $ newRefDef^.rdHeight)
      GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)
      GL.glClearColor 1.0 0.0 0.5 0.5
      GL.glDisable GL.GL_SCISSOR_TEST

rSetFrustum :: Quake ()
rSetFrustum = do
    vup <- use $ fastRenderAPIGlobals.frVUp
    vright <- use $ fastRenderAPIGlobals.frVRight
    vpn <- use $ fastRenderAPIGlobals.frVPn

    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
    frustum <- use $ fastRenderAPIGlobals.frFrustum

                               -- rotate VPN right by FOV_X/2 degrees
    let normals = V.fromList [ Math3D.rotatePointAroundVector vup vpn (0 - (90 - (newRefDef^.rdFovX) / 2))
                               -- rotate VPN left by FOV_x/2 degrees
                             , Math3D.rotatePointAroundVector vup vpn (90 - (newRefDef^.rdFovX) / 2)
                               -- rotate VPN up by FOV_Y/2 degrees
                             , Math3D.rotatePointAroundVector vright vpn (90 - (newRefDef^.rdFovY) / 2)
                               -- rotate VPN down by FOV_Y/2 degrees
                             , Math3D.rotatePointAroundVector vright vpn (0 - (90 - (newRefDef^.rdFovY) / 2))
                             ]

    -- io $ print "SET FRUSTUM"
    -- io $ print ("fovX = " ++ show (newRefDef^.rdFovX))
    -- io $ print ("fovY = " ++ show (newRefDef^.rdFovY))
    -- io $ print ("vup = " ++ show vup)
    -- io $ print ("vright = " ++ show vright)
    -- io $ print ("vpn = " ++ show vpn)
    -- io $ print ("normals0 = " ++ show (normals V.! 0))
    -- io $ print ("normals1 = " ++ show (normals V.! 1))
    -- io $ print ("normals2 = " ++ show (normals V.! 2))
    -- io $ print ("normals3 = " ++ show (normals V.! 3))

    origin <- use $ fastRenderAPIGlobals.frOrigin
    io $ V.imapM_ (updateFrustum normals origin) frustum

  where updateFrustum :: V.Vector (V3 Float) -> V3 Float -> Int -> IORef CPlaneT -> IO ()
        updateFrustum normals origin idx planeRef = do
          plane <- readIORef planeRef
          modifyIORef' planeRef (\v -> v { _cpNormal = normals V.! idx
                                         , _cpType = Constants.planeAnyZ
                                         , _cpDist = origin `dot` (normals V.! idx)
                                         , _cpSignBits = signbitsForPlane (normals V.! idx)
                                         })

signbitsForPlane :: V3 Float -> Int8
signbitsForPlane normal =
    let a = if (normal^._x) < 0 then 1 else 0
        b = if (normal^._y) < 0 then 2 else 0
        c = if (normal^._z) < 0 then 4 else 0
    in a .|. b .|. c

rSetupGL :: Quake ()
rSetupGL = do
    -- set up viewport
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
    vid <- use $ fastRenderAPIGlobals.frVid

    let x = newRefDef^.rdX
        x2 = (newRefDef^.rdX) + (newRefDef^.rdWidth)
        y = (vid^.vdHeight) - (newRefDef^.rdY)
        y2 = (vid^.vdHeight) - ((newRefDef^.rdY) + (newRefDef^.rdHeight))
        w = x2 - x
        h = y - y2

    GL.glViewport (fromIntegral x) (fromIntegral y2) (fromIntegral w) (fromIntegral h)

    -- set up projection matrix
    let screenAspect = fromIntegral (newRefDef^.rdWidth) / fromIntegral (newRefDef^.rdHeight) :: Float

    GL.glMatrixMode GL.GL_PROJECTION
    GL.glLoadIdentity

    Mesh.myGLUPerspective (float2Double $ newRefDef^.rdFovY) (float2Double screenAspect) 4 4096

    GL.glCullFace GL.GL_FRONT

    GL.glMatrixMode GL.GL_MODELVIEW
    GL.glLoadIdentity

    GL.glRotatef (-90) 1 0 0 -- put Z going up
    GL.glRotatef 90 0 0 1 -- put Z going up
    GL.glRotatef (realToFrac $ negate (newRefDef^.rdViewAngles._z)) 1 0 0
    GL.glRotatef (realToFrac $ negate (newRefDef^.rdViewAngles._x)) 0 1 0
    GL.glRotatef (realToFrac $ negate (newRefDef^.rdViewAngles._y)) 0 0 1
    GL.glTranslatef (realToFrac $ negate (newRefDef^.rdViewOrg._x))
                    (realToFrac $ negate (newRefDef^.rdViewOrg._y))
                    (realToFrac $ negate (newRefDef^.rdViewOrg._z))

    worldMatrix <- io $ allocaArray 16 $ \ptr -> do
      GL.glGetFloatv GL.GL_MODELVIEW_MATRIX ptr
      peekArray 16 ptr
      -- TODO: clear array in jake2 but no clear in quake2 original
    
    fastRenderAPIGlobals.frWorldMatrix .= worldMatrix

    glCullValue <- liftM (^.cvValue) glCullCVar
    if glCullValue /= 0
      then GL.glEnable GL.GL_CULL_FACE
      else GL.glDisable GL.GL_CULL_FACE

    GL.glDisable GL.GL_BLEND
    GL.glDisable GL.GL_ALPHA_TEST
    GL.glEnable GL.GL_DEPTH_TEST

rDrawEntitiesOnList :: Quake ()
rDrawEntitiesOnList = do
    drawEntitiesValue <- liftM (^.cvValue) drawEntitiesCVar

    unless (drawEntitiesValue == 0) $ do
      newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef

      -- draw non-transparent first
      drawNonTransparentEntities newRefDef 0 (newRefDef^.rdNumEntities)

      -- draw transparent entities
      -- we could sort these if it ever becomes a problem
      GL.glDepthMask (fromIntegral GL.GL_FALSE) -- no z writes

      drawTransparentEntities newRefDef 0 (newRefDef^.rdNumEntities)

      GL.glDepthMask (fromIntegral GL.GL_TRUE) -- back to writing

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

rDrawParticles :: Quake ()
rDrawParticles = do
    extPointParametersValue <- liftM (^.cvValue) glExtPointParametersCVar
    pointParameterEXT <- use $ fastRenderAPIGlobals.frPointParameterEXT

    if extPointParametersValue /= 0 && pointParameterEXT
      then do
        io (putStrLn "IMPLEMENT ME! FastRenderAPI.rDrawParticles") >> return () -- TODO
      else do
        newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
        glDrawParticles (newRefDef^.rdNumParticles)

rFlash :: Quake ()
rFlash = rPolyBlend

rDrawBeam :: EntityT -> Quake ()
rDrawBeam _ = do
    io (putStrLn "FastRenderAPI.rDrawBeam") >> undefined -- TODO

rDrawNullModel :: Quake ()
rDrawNullModel = do
    Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
    currentEntity <- io $ readIORef currentEntityRef

    shadeLight <- if (currentEntity^.enFlags) .&. Constants.rfFullBright /= 0
                    then return (V3 0 0 0.8)
                    else Light.rLightPoint (currentEntity^.eOrigin)

    let shadeLight' = fmap realToFrac shadeLight

    GL.glPushMatrix
    Mesh.rRotateForEntity currentEntity

    GL.glDisable GL.GL_TEXTURE_2D
    GL.glColor3f (shadeLight'^._x) (shadeLight'^._y) (shadeLight'^._z)

    GL.glBegin GL.GL_TRIANGLE_FAN
    GL.glVertex3f 0 0 (-16)
    mapM_ (\i -> GL.glVertex3f (16 * cos(i * pi / 2)) (16 * sin(i * pi / 2)) 0) [0..4]
    GL.glEnd

    GL.glBegin GL.GL_TRIANGLE_FAN
    GL.glVertex3f 0 0 16
    mapM_ (\i -> GL.glVertex3f (16 * cos(i * pi / 2)) (16 * sin(i * pi / 2)) 0) [4,3..0]
    GL.glEnd

    GL.glColor3f 1 1 1
    GL.glPopMatrix
    GL.glEnable GL.GL_TEXTURE_2D

rDrawSpriteModel :: IORef EntityT -> Quake ()
rDrawSpriteModel _ = do
    io (putStrLn "FastRenderAPI.rDrawSpriteModel") >> undefined -- TODO

rPolyBlend :: Quake ()
rPolyBlend = do
    polyBlendValue <- liftM (^.cvValue) glPolyBlendCVar
    vblend <- use $ fastRenderAPIGlobals.frVBlend
    
    unless (polyBlendValue == 0 || (vblend^._w) == 0) $ do
      GL.glDisable GL.GL_ALPHA_TEST
      GL.glEnable GL.GL_BLEND
      GL.glDisable GL.GL_DEPTH_TEST
      GL.glDisable GL.GL_TEXTURE_2D

      GL.glLoadIdentity

      -- FIXME: get rid of these
      GL.glRotatef (-90) 1 0 0 -- put Z going up
      GL.glRotatef 90 0 0 1 -- put Z going up

      GL.glColor4f (realToFrac $ vblend^._x) (realToFrac $ vblend^._y) (realToFrac $ vblend^._z) (realToFrac $ vblend^._w)

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

glDrawParticles :: Int -> Quake ()
glDrawParticles numParticles = do
    vup <- use $ fastRenderAPIGlobals.frVUp
    vright <- use $ fastRenderAPIGlobals.frVRight
    vpn <- use $ fastRenderAPIGlobals.frVPn
    origin <- use $ fastRenderAPIGlobals.frOrigin

    let up = fmap (* 1.5) vup
        right = fmap (* 1.5) vright

    particleTextureRef <- use $ fastRenderAPIGlobals.frParticleTexture
    particleTexture <- io $ readIORef particleTextureRef

    Image.glBind (particleTexture^.iTexNum)
    GL.glDepthMask (fromIntegral GL.GL_FALSE) -- no z buffering
    GL.glEnable GL.GL_BLEND
    Image.glTexEnv GL.GL_MODULATE

    GL.glBegin GL.GL_TRIANGLES

    sourceVertices <- use $ particleTGlobals.pVertexArray
    sourceColors <- use $ particleTGlobals.pColorArray

    io $ drawParticles sourceVertices sourceColors up right vpn origin 0 0 numParticles

    GL.glEnd

    GL.glDisable GL.GL_BLEND
    GL.glColor4f 1 1 1 1
    GL.glDepthMask (fromIntegral GL.GL_TRUE) -- back to normal z buffering
    Image.glTexEnv GL.GL_REPLACE

  where drawParticles :: MSV.IOVector Float -> MSV.IOVector Int32 -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> IO ()
        drawParticles sourceVertices sourceColors up right vpn origin j idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              originX <- MSV.read sourceVertices (j + 0)
              originY <- MSV.read sourceVertices (j + 1)
              originZ <- MSV.read sourceVertices (j + 2)

              -- hack a scale up to keep particles from disappearing
              let scale = (originX - (origin^._x)) * (vpn^._x)
                        + (originY - (origin^._y)) * (vpn^._y)
                        + (originZ - (origin^._z)) * (vpn^._z)

                  scale' = if scale < 20 then 1 else 1 + scale * 0.004

              color <- MSV.read sourceColors idx

              GL.glColor4ub (fromIntegral $ color .&. 0xFF)
                            (fromIntegral $ (color `shiftR` 8) .&. 0xFF)
                            (fromIntegral $ (color `shiftR` 16) .&. 0xFF)
                            (fromIntegral $ (color `shiftR` 24) .&. 0xFF)

              -- first vertex
              GL.glTexCoord2f 0.0625 0.0625
              GL.glVertex3f (realToFrac originX) (realToFrac originY) (realToFrac originZ)
              -- second vertex
              GL.glTexCoord2f 1.0625 0.0625
              GL.glVertex3f (realToFrac $ originX + (up^._x) * scale') (realToFrac $ originY + (up^._y) * scale') (realToFrac $ originZ + (up^._z) * scale')
              -- third vertex
              GL.glTexCoord2f 0.0625 1.0625
              GL.glVertex3f (realToFrac $ originX + (right^._x) * scale') (realToFrac $ originY + (right^._y) * scale') (realToFrac $ originZ + (right^._z) * scale')

              drawParticles sourceVertices sourceColors up right vpn origin (j + 3) (idx + 1) maxIdx

fastSetPalette :: Maybe B.ByteString -> Quake ()
fastSetPalette maybePalette = do
    -- 256 RGB values (768 bytes)
    -- or null
    case maybePalette of
      Nothing -> do
        d8to24table <- use $ fastRenderAPIGlobals.frd8to24table
        fastRenderAPIGlobals.frRawPalette .= UV.map (.|. 0xFF000000) d8to24table

      Just palette ->
        fastRenderAPIGlobals.frRawPalette .= UV.generate 256 (fromPalette palette)

    rawPalette <- use $ fastRenderAPIGlobals.frRawPalette
    Image.glSetTexturePalette rawPalette

    GL.glClearColor 0 0 0 0
    GL.glClear GL.GL_COLOR_BUFFER_BIT
    GL.glClearColor 1.0 0.0 0.5 0.5

  where fromPalette :: B.ByteString -> Int -> Int
        fromPalette palette idx =
          let j = idx * 3
              a = (fromIntegral (palette `B.index` (j + 0)) .&. 0xFF) `shiftL` 0
              b = (fromIntegral (palette `B.index` (j + 1)) .&. 0xFF) `shiftL` 8
              c = (fromIntegral (palette `B.index` (j + 2)) .&. 0xFF) `shiftL` 16
          in 0xFF000000 .|. a .|. b .|. c

fastShutdown :: GLDriver -> Quake ()
fastShutdown glDriver = do
    Cmd.removeCommand "modellist"
    Cmd.removeCommand "screenshot"
    Cmd.removeCommand "imagelist"
    Cmd.removeCommand "gl_strings"

    Model.freeAll

    Image.glShutdownImages

    -- shut down OS specific OpenGL stuff like contexts, etc
    glDriver^.gldShutdown
