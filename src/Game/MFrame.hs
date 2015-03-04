module Game.MFrame where

data MFrame =
  MFrame { mFrameAI    :: IO () -- TODO: ???
         , mFrameDist  :: Float
         , mFrameThink :: IO () -- TODO: ???
         }
