{-# LANGUAGE TemplateHaskell #-}
module Render.MNodeT
  ( module Render.MNodeT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''MNodeT