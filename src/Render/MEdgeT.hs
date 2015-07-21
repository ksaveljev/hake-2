{-# LANGUAGE TemplateHaskell #-}
module Render.MEdgeT where

import Data.Word (Word16)
import Control.Lens (makeLenses)
import qualified Data.ByteString.Lazy as BL

import Util.Binary
import qualified Constants

mEdgeDiskSize :: Int
mEdgeDiskSize = 2 * Constants.sizeOfShort

data MEdgeT =
  MEdgeT { _meV                :: (Word16, Word16)
         , _meCachedEdgeOffset :: Int
         }

makeLenses ''MEdgeT

newMEdgeT :: BL.ByteString -> MEdgeT
newMEdgeT = runGet getMEdgeT

getMEdgeT :: Get MEdgeT
getMEdgeT = do
    v <- getWord162 
    return MEdgeT { _meV = v
                  , _meCachedEdgeOffset = 0
                  }
