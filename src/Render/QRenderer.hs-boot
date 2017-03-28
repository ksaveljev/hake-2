module Render.QRenderer where

import qualified Data.ByteString as B
import qualified Data.Vector as V

import Render.Renderer

drivers :: V.Vector Renderer

getDefaultName :: B.ByteString

getPreferredName :: B.ByteString

getDriver :: B.ByteString -> Bool -> Maybe Renderer

getDriverNames :: V.Vector B.ByteString
