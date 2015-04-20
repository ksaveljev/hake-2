{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.FastRenderAPI where

import Control.Lens ((.=), (^.), use, zoom)
import Control.Monad (void, when, liftM)
import Data.Bits ((.|.), (.&.))
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV
import qualified Debug.Trace as DT

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Client.VID as VID
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified QCommon.CVar as CVar
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Warp as Warp

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
              , _rBeginFrame        = DT.trace "FastRenderAPI.rBeginFrame" undefined -- TODO
              , _glScreenShotF      = DT.trace "FastRenderAPI.glScreenShotF" undefined -- TODO
              }

-- fill r_turbsin
turbSin :: UV.Vector Float
turbSin = UV.generate 256 (\idx -> (Warp.sinV UV.! idx) * 0.5)

fastInit :: Quake () -> ((Int, Int) -> Int -> Bool -> Quake Int) -> Int -> Int -> Quake Bool
fastInit glImplScreenshot glImplSetMode _ _ = do
    VID.printf Constants.printAll ("ref_gl version: " `B.append` RenderAPIConstants.refVersion `B.append` "\n")

    Image.getPalette

    rRegister glImplScreenshot

    -- set our "safe" modes
    fastRenderAPIGlobals.frGLState.glsPrevMode .= 3

    ok <- rSetMode glImplSetMode

    -- create the window and set up the context
    if not ok
      then do
        VID.printf Constants.printAll "ref_gl::R_Init() - could not R_SetMode()\n"
        return False
      else
        return True

fastInit2 :: Quake Bool
fastInit2 = do
    VID.menuInit

    -- get our various GL strings
    vendor <- io $ GL.get GL.vendor >>= return . BC.pack
    renderer <- io $ GL.get GL.renderer >>= return . BC.pack
    version <- io $ GL.get GL.glVersion >>= return . BC.pack
    extensions <- io $ GL.get GL.glExtensions >>= return . BC.pack . show

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

    io (putStrLn "FastRenderAPI.fastInit2") >> undefined

rRegister :: Quake () -> Quake ()
rRegister glImplScreenshot = do
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
    Cmd.addCommand "screenshot" (Just glImplScreenshot)
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
