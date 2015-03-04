{-# LANGUAGE TemplateHaskell #-}
module Game.MMoveT where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.MFrameT

data MMoveT =
  MMoveT { _mmFirstFrame :: Int
         , _mmLastFrame  :: Int
         , _mmFrame      :: UV.Vector MFrameT
         , _mmEndFunc    :: IO () -- TODO: ???
         }

makeLenses ''MMoveT
