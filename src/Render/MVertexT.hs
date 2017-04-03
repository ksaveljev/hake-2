{-# LANGUAGE TemplateHaskell #-}
module Render.MVertexT where

import           Control.Lens         (makeLenses)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor         ((<$>))
import           Linear               (V3(..))

import qualified Constants
import           Types
import           Util.Binary

mVertexDiskSize :: Int
mVertexDiskSize = 3 * Constants.sizeOfFloat

memSize :: Int
memSize = 3 * Constants.sizeOfFloat

makeLenses ''MVertexT

newMVertexT :: BL.ByteString -> MVertexT
newMVertexT = runGet getMVertexT

getMVertexT :: Get MVertexT
getMVertexT = MVertexT <$> getV3Float