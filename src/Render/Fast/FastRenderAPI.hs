{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.FastRenderAPI where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV
import qualified Debug.Trace as DT

import Quake
import Render.RenderAPI
import qualified Constants
import qualified Client.VID as VID
import qualified Render.Fast.Warp as Warp

refVersion :: B.ByteString
refVersion = "GL 0.01"

fastRenderAPI :: RenderAPI
fastRenderAPI =
    RenderAPI { _rInit              = DT.trace "FastRenderAPI.rInit" undefined -- TODO
              , _rInit2             = DT.trace "FastRenderAPI.rInit2" undefined -- TODO
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
fastInit vidXPos vidYPos = do
    VID.printf Constants.printAll ("ref_gl version: " `B.append` refVersion `B.append` "\n")

    undefined -- TODO
