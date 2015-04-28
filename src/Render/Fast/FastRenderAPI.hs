{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.FastRenderAPI where

import Control.Exception (handle, IOException)
import Control.Lens ((.=), (^.), use, zoom)
import Control.Monad (void, when, liftM, unless)
import Data.Bits ((.|.), (.&.))
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV
import qualified Debug.Trace as DT

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import Render.OpenGL.GLDriver
import qualified Constants
import qualified Client.VID as VID
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified QCommon.CVar as CVar
import qualified Render.Fast.Draw as Draw
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Warp as Warp
import qualified Render.OpenGL.QGLConstants as QGLConstants
import qualified Render.RenderAPIConstants as RenderAPIConstants

fastRenderAPI :: RenderAPI
fastRenderAPI =
    RenderAPI { _rInit              = fastInit
              , _rInit2             = fastInit2
              , _rShutdown          = DT.trace "FastRenderAPI.rShutdown" undefined -- TODO
              , _rBeginRegistration = DT.trace "FastRenderAPI.rBeginRegistration" undefined -- TODO
              , _rRegisterModel     = DT.trace "FastRenderAPI.rRegisterModel" undefined -- TODO
              , _rRegisterSkin      = DT.trace "FastRenderAPI.rRegisterSkin" undefined -- TODO
              , _rDrawFindPic       = DT.trace "FastRenderAPI.rDrawFindPic" undefined -- TODO
              , _rSetSky            = DT.trace "FastRenderAPI.rSetSky" undefined -- TODO
              , _rEndRegistration   = DT.trace "FastRenderAPI.rEndRegistration" undefined -- TODO
              , _rRenderFrame       = DT.trace "FastRenderAPI.rRenderFrame" undefined -- TODO
              , _rDrawGetPicSize    = DT.trace "FastRenderAPI.rDrawGetPicSize" undefined -- TODO
              , _rDrawPic           = DT.trace "FastRenderAPI.rDrawPic" undefined -- TODO
              , _rDrawStretchPic    = DT.trace "FastRenderAPI.rDrawStretchPic" undefined -- TODO
              , _rDrawChar          = DT.trace "FastRenderAPI.rDrawChar" undefined -- TODO
              , _rDrawTileClear     = DT.trace "FastRenderAPI.rDrawTileClear" undefined -- TODO
              , _rDrawFill          = DT.trace "FastRenderAPI.rDrawFill" undefined -- TODO
              , _rDrawFadeScreen    = DT.trace "FastRenderAPI.rDrawFadeScreen" undefined -- TODO
              , _rDrawStretchRaw    = DT.trace "FastRenderAPI.rDrawStretchRaw" undefined -- TODO
              , _rSetPalette        = DT.trace "FastRenderAPI.rSetPalette" undefined -- TODO
              , _rBeginFrame        = fastBeginFrame
              , _glScreenShotF      = DT.trace "FastRenderAPI.glScreenShotF" undefined -- TODO
              }

-- fill r_turbsin
turbSin :: UV.Vector Float
turbSin = UV.generate 256 (\idx -> (Warp.sinV UV.! idx) * 0.5)

fastInit :: GLDriver -> Int -> Int -> Quake Bool
fastInit glDriver _ _ = do
    VID.printf Constants.printAll ("ref_gl version: " `B.append` RenderAPIConstants.refVersion `B.append` "\n")

    Image.getPalette

    rRegister

    -- set our "safe" modes
    fastRenderAPIGlobals.frGLState.glsPrevMode .= 3

    ok <- rSetMode (glDriver^.gldSetMode)

    -- create the window and set up the context
    if not ok
      then do
        VID.printf Constants.printAll "ref_gl::R_Init() - could not R_SetMode()\n"
        return False
      else
        return True

fastInit2 :: GLDriver -> Quake Bool
fastInit2 glDriver = do
    VID.menuInit

    -- get our various GL strings
    vendor <- io $ getGLString GL.gl_VENDOR
    renderer <- io $ getGLString GL.gl_RENDERER
    version <- io $ getGLString GL.gl_VERSION
    extensions <- io $ getGLString GL.gl_EXTENSIONS

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
          VID.printf Constants.printAll "...using gl_monolightmap 'a'\n"
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
        unless (err == GL.gl_NO_ERROR) $
          VID.printf Constants.printAll "gl.glGetError() = TODO" -- TODO: add error information

        glDriver^.gldEndFrame
        return True

  where getGLString :: GL.GLenum -> IO B.ByteString
        getGLString n = GL.glGetString n >>= maybeNullPtr (return "") (B.packCString . castPtr)

        maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
        maybeNullPtr n f ptr | ptr == nullPtr = n
                             | otherwise      = f ptr

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
    void $ CVar.get "vid_ref" "lwjgl" Constants.cvarArchive

    Cmd.addCommand "imagelist" (Just Image.glImageListF)
    Cmd.addCommand "screenshot" (Just fastScreenShotF)
    Cmd.addCommand "modellist" (Just Model.modelListF)
    Cmd.addCommand "gl_strings" (Just glStringsF)

rSetMode :: ((Int, Int) -> Int -> Bool -> Quake Int) -> Quake Bool
rSetMode glImplSetMode = do
    fullScreen <- vidFullScreenCVar
    glMode <- glModeCVar

    let isFullscreen = (fullScreen^.cvValue) > 0

    CVar.update fullScreen { _cvModified = False }
    CVar.update glMode { _cvModified = False }

    vid <- use $ fastRenderAPIGlobals.frVid
    let dim = (vid^.vdWidth, vid^.vdHeight)

    err <- glImplSetMode dim (truncate $ glMode^.cvValue) isFullscreen

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
                    err' <- glImplSetMode dim (truncate $ glMode^.cvValue) False
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
            err' <- glImplSetMode dim prevMode False

            if err' /= RenderAPIConstants.rsErrOk
              then do
                VID.printf Constants.printAll "ref_gl::R_SetMode() - could not revert to safe mode\n"
                return False
              else return True

glStringsF :: XCommandT
glStringsF = io (putStrLn "FastRenderAPI.glStringsF") >> undefined -- TODO

glSetDefaultState :: GLDriver -> Quake ()
glSetDefaultState glDriver = do
    GL.glClearColor 1 0 0.5 0.5
    GL.glCullFace GL.gl_FRONT
    GL.glEnable GL.gl_TEXTURE_2D

    GL.glEnable GL.gl_ALPHA_TEST
    GL.glAlphaFunc GL.gl_GREATER 0.666

    GL.glDisable GL.gl_DEPTH_TEST
    GL.glDisable GL.gl_CULL_FACE
    GL.glDisable GL.gl_BLEND

    GL.glColor4f 1 1 1 1

    GL.glPolygonMode GL.gl_FRONT_AND_BACK GL.gl_FILL
    GL.glShadeModel GL.gl_FLAT

    liftM (^.cvString) glTextureModeCVar >>= Image.glTextureMode
    liftM (^.cvString) glTextureAlphaModeCVar >>= Image.glTextureAlphaMode
    liftM (^.cvString) glTextureSolidModeCVar >>= Image.glTextureSolidMode

    minFilter <- use $ fastRenderAPIGlobals.frGLFilterMin
    maxFilter <- use $ fastRenderAPIGlobals.frGLFilterMax

    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral minFilter)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral maxFilter)

    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_WRAP_S (fromIntegral GL.gl_REPEAT)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_WRAP_T (fromIntegral GL.gl_REPEAT)

    GL.glBlendFunc GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA

    Image.glTexEnv GL.gl_REPLACE

    ppExt <- use $ fastRenderAPIGlobals.frPointParameterEXT

    when ppExt $ do
      a <- liftM (^.cvValue) glParticleAttACVar
      b <- liftM (^.cvValue) glParticleAttBCVar
      c <- liftM (^.cvValue) glParticleAttCCVar
      minSize <- liftM (^.cvValue) glParticleMinSizeCVar
      maxSize <- liftM (^.cvValue) glParticleMaxSizeCVar

      let arr :: [GL.GLfloat] = fmap realToFrac [a, b, c]

      GL.glEnable GL.gl_POINT_SMOOTH
      GL.glPointParameterfEXT GL.gl_POINT_SIZE_MIN_EXT (realToFrac minSize)
      GL.glPointParameterfEXT GL.gl_POINT_SIZE_MAX_EXT (realToFrac maxSize)
      io $ withArray arr $ \ptr ->
        GL.glPointParameterfvEXT GL.gl_DISTANCE_ATTENUATION_EXT ptr

    ctExt <- use $ fastRenderAPIGlobals.frColorTableEXT
    pt <- liftM (^.cvValue) glExtPalettedTextureCVar

    when (ctExt && pt /= 0) $ do
      io $ GL.glEnable GL.gl_SHARED_TEXTURE_PALETTE_EXT
      d8to24table <- use $ fastRenderAPIGlobals.frd8to24table
      Image.glSetTexturePalette d8to24table

    glUpdateSwapInterval glDriver

    -- vertex array extension
    t0 <- use $ fastRenderAPIGlobals.frTexture0

    GL.glEnableClientState GL.gl_VERTEX_ARRAY
    GL.glClientActiveTextureARB (fromIntegral t0)
    GL.glEnableClientState GL.gl_TEXTURE_COORD_ARRAY

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
    goInto2DMode

    -- draw buffer stuff
    drawBufferStuff

    -- texturemode stuff
    textureModeStuff

    -- swapinterval stuff
    glUpdateSwapInterval glDriver

    -- clear screen if desired
    rClear

  where goInto2DMode :: Quake ()
        goInto2DMode = do
          vid <- use $ fastRenderAPIGlobals.frVid
          let width = vid^.vdWidth
              height = vid^.vdHeight
          GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
          GL.glMatrixMode GL.gl_PROJECTION
          GL.glLoadIdentity
          GL.glOrtho 0 (fromIntegral width) (fromIntegral height) 0 (-99999) 99999
          GL.glMatrixMode GL.gl_MODELVIEW
          GL.glLoadIdentity
          GL.glDisable GL.gl_DEPTH_TEST
          GL.glDisable GL.gl_CULL_FACE
          GL.glDisable GL.gl_BLEND
          GL.glEnable GL.gl_ALPHA_TEST
          GL.glColor4f 1 1 1 1

        drawBufferStuff :: Quake ()
        drawBufferStuff = do
          drawBuffer <- glDrawBufferCVar

          when (drawBuffer^.cvModified) $ do
            CVar.update drawBuffer { _cvModified = False }

            glState <- use $ fastRenderAPIGlobals.frGLState

            when ((glState^.glsCameraSeparation) == 0 || not (glState^.glsStereoEnabled)) $ do
              if BC.map toUpper (drawBuffer^.cvString) == "GL_FRONT"
                then GL.glDrawBuffer GL.gl_FRONT
                else GL.glDrawBuffer GL.gl_BACK

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

fastScreenShotF :: XCommandT
fastScreenShotF = io (putStrLn "FastRenderAPI.fastScreenShotF") >> undefined -- TODO

rClear :: Quake ()
rClear = do
    io (putStrLn "FastRenderAPI.rClear") >> undefined -- TODO
