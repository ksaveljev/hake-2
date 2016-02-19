{-# LANGUAGE TemplateHaskell #-}
module Server.ClientFrameT
  ( module Server.ClientFrameT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''ClientFrameT