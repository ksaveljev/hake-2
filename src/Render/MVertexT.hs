{-# LANGUAGE TemplateHaskell #-}
module Render.MVertexT where

import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Linear (V3(..))
import qualified Data.ByteString.Lazy as BL

import Util.Binary
import qualified Constants

diskSize :: Int
diskSize = 3 * Constants.sizeOfFloat

memSize :: Int
memSize = 3 * Constants.sizeOfFloat

data MVertexT =
  MVertexT { _mvPosition :: V3 Float
           }

makeLenses ''MVertexT

newMVertexT :: BL.ByteString -> MVertexT
newMVertexT = runGet getMVertexT

getMVertexT :: Get MVertexT
getMVertexT = MVertexT <$> getV3Float
