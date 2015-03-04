{-# LANGUAGE TemplateHaskell #-}
module Game.MFrameT where

import Control.Lens (makeLenses)

data MFrameT =
  MFrameT { _mfAI    :: IO () -- TODO: ???
          , _mfDist  :: Float
          , _mfThink :: IO () -- TODO: ???
          }

makeLenses ''MFrameT
