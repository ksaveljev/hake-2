module Render.Fast.FastRenderAPI
  ( fastRenderAPI
  ) where

import qualified Game.Cmd as Cmd
import qualified Render.Fast.Draw as Draw
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Warp as Warp
import           Render.OpenGL.GLDriver
import           Types

import           Control.Lens ((^.))
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
fastInit = error "FastRenderAPI.fastInit" -- TODO

fastInit2 :: GLDriver -> Quake Bool
fastInit2 = error "FastRenderAPI.fastInit2" -- TODO

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