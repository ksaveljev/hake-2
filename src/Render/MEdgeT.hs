{-# LANGUAGE TemplateHaskell #-}
module Render.MEdgeT
  ( module Render.MEdgeT
  ) where

import qualified Constants
import           Types
import           Util.Binary (getWord162)

import           Control.Lens (makeLenses)
import           Data.Binary.Get (Get)

makeLenses ''MEdgeT

mEdgeDiskSize :: Int
mEdgeDiskSize = 2 * Constants.sizeOfShort

getMEdgeT :: Get MEdgeT
getMEdgeT =
  do v <- getWord162 
     return MEdgeT { _meV = v
                   , _meCachedEdgeOffset = 0
                   }