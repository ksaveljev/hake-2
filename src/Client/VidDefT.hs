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
