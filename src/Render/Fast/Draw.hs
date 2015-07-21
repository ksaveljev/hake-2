{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Draw where

import Control.Lens (preuse, (^.), ix, use, (.=))
import Control.Monad (when, unless)
import Data.Bits ((.&.), shiftR)
import Data.IORef (readIORef)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.Com as Com
import qualified Render.Fast.Image as Image
import qualified Render.RenderAPIConstants as RenderAPIConstants

initLocal :: Quake ()
initLocal = do
    -- load console characters (don't bilerp characters)
    Just imageRef <- Image.glFindImage "pics/conchars.pcx" RenderAPIConstants.itPic
    fastRenderAPIGlobals.frDrawChars .= Just imageRef

    image <- io $ readIORef imageRef
    Image.glBind (image^.iTexNum)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral GL.gl_NEAREST)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral GL.gl_NEAREST)
