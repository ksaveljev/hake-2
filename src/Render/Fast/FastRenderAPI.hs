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
import           Render.GLStateT
import           Render.OpenGL.GLDriver
import           Types

import           Control.Lens (use, (^.), (.=), (&), (.~))
import           Data.Bits ((.|.))
import qualified Data.ByteString as B

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
     error "FastRenderAPI.fastInit2" -- TODO
{-
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
                             -}

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