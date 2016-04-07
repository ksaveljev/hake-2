{-# LANGUAGE TemplateHaskell #-}
module Render.MVertexT
  ( module Render.MVertexT
  ) where

import qualified Constants
import           Types
import           Util.Binary (getV3Float)

import           Control.Lens (makeLenses)
import           Data.Binary.Get (Get)

makeLenses ''MVertexT

mVertexDiskSize :: Int
mVertexDiskSize = 3 * Constants.sizeOfFloat

getMVertexT :: Get MVertexT
getMVertexT = MVertexT <$> getV3Float