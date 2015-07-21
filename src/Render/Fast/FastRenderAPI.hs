module Render.Fast.FastRenderAPI where

import qualified Debug.Trace as DT

import Quake
import QuakeState

fastRenderAPI :: RenderAPI
fastRenderAPI =
    RenderAPI { _rInit              = DT.trace "fastInit" undefined -- fastInit
              , _rInit2             = DT.trace "fastInit2" undefined -- fastInit2
              , _rShutdown          = DT.trace "FastRenderAPI.rShutdown" undefined -- TODO
              , _rBeginRegistration = DT.trace "rBeginRegistration" undefined -- \_ -> Model.rBeginRegistration
              , _rRegisterModel     = DT.trace "rRegisterModel" undefined -- \_ -> Model.rRegisterModel
              , _rRegisterSkin      = DT.trace "rRegisterSkin" undefined -- \_ -> Image.rRegisterSkin
              , _rDrawFindPic       = DT.trace "rDrawFindPic" undefined -- \_ -> Draw.findPic
              , _rSetSky            = DT.trace "rSetSky" undefined -- \_ -> Warp.rSetSky
              , _rEndRegistration   = DT.trace "rEndRegistration" undefined -- \_ -> Model.rEndRegistration
              , _rRenderFrame       = DT.trace "rRenderFrame" undefined -- \_ -> fastRenderFrame
              , _rDrawGetPicSize    = DT.trace "rDrawGetPicSize" undefined -- \_ -> Draw.getPicSize
              , _rDrawPic           = DT.trace "rDrawPic" undefined -- \_ -> Draw.drawPic
              , _rDrawStretchPic    = DT.trace "rDrawStretchPic" undefined -- \_ -> Draw.stretchPic
              , _rDrawChar          = DT.trace "rDrawChar" undefined -- \_ -> Draw.drawChar
              , _rDrawTileClear     = DT.trace "FastRenderAPI.rDrawTileClear" undefined -- TODO
              , _rDrawFill          = DT.trace "rDrawFill" undefined -- \_ -> Draw.fill
              , _rDrawFadeScreen    = DT.trace "FastRenderAPI.rDrawFadeScreen" undefined -- TODO
              , _rDrawStretchRaw    = DT.trace "FastRenderAPI.rDrawStretchRaw" undefined -- TODO
              , _rSetPalette        = DT.trace "FastRenderAPI.rSetPalette" undefined -- TODO
              , _rBeginFrame        = DT.trace "rBeginFrame" undefined -- fastBeginFrame
              , _glScreenShotF      = DT.trace "FastRenderAPI.glScreenShotF" undefined -- TODO
              }
