{-# LANGUAGE TemplateHaskell #-}
module Render.MModelT
  ( module Render.MModelT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''MModelT