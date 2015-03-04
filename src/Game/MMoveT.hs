module Game.MMoveT where

import qualified Data.Vector.Unboxed as UV

import Game.MFrameT

data MMoveT =
  MMoveT { mmFirstFrame :: Int
         , mmLastFrame  :: Int
         , mmFrame      :: UV.Vector MFrameT
         , mmEndFunc    :: IO () -- TODO: ???
         }
