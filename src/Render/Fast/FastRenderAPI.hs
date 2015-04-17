{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.FastRenderAPI where

import Control.Lens ((.=))
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV
import qualified Debug.Trace as DT

import Quake
import QuakeState
import qualified Constants
import qualified Client.VID as VID
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Warp as Warp

refVersion :: B.ByteString
refVersion = "GL 0.01"

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

fastInit :: Int -> Int -> Quake Bool
fastInit _ _ = do
    VID.printf Constants.printAll ("ref_gl version: " `B.append` refVersion `B.append` "\n")

    Image.getPalette

    rRegister

    -- set our "safe" modes
    fastRenderAPIGlobals.frGLState.glsPrevMode .= 3

    ok <- rSetMode

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

    io (putStrLn "FastRenderAPI.fastInit2") >> undefined

rRegister :: Quake ()
rRegister = io (putStrLn "FastRenderAPI.rRegister") >> undefined -- TODO

rSetMode :: Quake Bool
rSetMode = io (putStrLn "FastRenderAPI.rSetMode") >> undefined -- TODO
