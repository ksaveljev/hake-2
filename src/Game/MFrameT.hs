module Game.MFrameT where

data MFrameT =
  MFrameT { mfAI    :: IO () -- TODO: ???
          , mfDist  :: Float
          , mfThink :: IO () -- TODO: ???
          }
