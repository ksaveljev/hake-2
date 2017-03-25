{-# LANGUAGE TemplateHaskell #-}
module Render.MModelT
    ( module Render.MModelT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MModelT