{-# LANGUAGE TemplateHaskell #-}
module Client.VidDefT where

import Control.Lens (makeLenses)

data VidDefT =
  VidDefT { _vdWidth     :: Int
          , _vdHeight    :: Int
          , _vdNewWidth  :: Int
          , _vdNewHeight :: Int
          }

makeLenses ''VidDefT

newVidDefT :: VidDefT
newVidDefT =
  VidDefT { _vdWidth     = 0
          , _vdHeight    = 0
          , _vdNewWidth  = 0
          , _vdNewHeight = 0
          }
