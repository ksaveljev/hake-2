{-# LANGUAGE TemplateHaskell #-}
module Render.MEdgeT where

import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import Data.Word (Word16)
import Control.Lens (makeLenses)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

data MEdgeT =
  MEdgeT { _meV                :: (Word16, Word16)
         , _meCachedEdgeOffset :: Int
         }

makeLenses ''MEdgeT

newMEdgeT :: BL.ByteString -> MEdgeT
newMEdgeT = runGet getMEdgeT

getMEdgeT :: Get MEdgeT
getMEdgeT = MEdgeT <$> getWord162 <*> getInt
