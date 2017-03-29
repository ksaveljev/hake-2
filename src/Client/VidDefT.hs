{-# LANGUAGE TemplateHaskell #-}
module Client.VidDefT where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''VidDefT

newVidDefT :: VidDefT
newVidDefT = VidDefT
    { _vdWidth     = 0
    , _vdHeight    = 0
    , _vdNewWidth  = 0
    , _vdNewHeight = 0
    }
