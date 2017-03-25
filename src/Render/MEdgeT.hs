{-# LANGUAGE TemplateHaskell #-}
module Render.MEdgeT
    ( module Render.MEdgeT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get)

import qualified Constants
import           Types
import           Util.Binary     (getWord162)

makeLenses ''MEdgeT

mEdgeDiskSize :: Int
mEdgeDiskSize = 2 * Constants.sizeOfShort

getMEdgeT :: Get MEdgeT
getMEdgeT = do
    v <- getWord162 
    return MEdgeT
        { _meV = v
        , _meCachedEdgeOffset = 0
        }