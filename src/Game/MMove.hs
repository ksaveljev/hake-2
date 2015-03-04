module Game.MMove where

import qualified Data.Vector.Unboxed as UV

import Game.MFrame

data MMove = MMove { mMoveFirstFrame :: Int
                   , mMoveLastFrame  :: Int
                   , mMoveFrame      :: UV.Vector MFrame
                   , mMoveEndFunc    :: IO () -- TODO: ???
                   }
