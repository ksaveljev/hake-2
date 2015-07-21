{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Draw where

import Control.Lens (preuse, (^.), ix, use, (.=))
import Control.Monad (when, unless)
import Data.Bits ((.&.), shiftR)
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
