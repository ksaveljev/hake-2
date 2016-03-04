{-# LANGUAGE TemplateHaskell #-}
module Client.VidDefT
  ( module Client.VidDefT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''VidDefT

newVidDefT :: VidDefT
newVidDefT =
  VidDefT { _vdWidth     = 0
          , _vdHeight    = 0
          , _vdNewWidth  = 0
          , _vdNewHeight = 0
          }