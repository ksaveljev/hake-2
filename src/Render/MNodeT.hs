{-# LANGUAGE TemplateHaskell #-}
module Render.MNodeT
    ( module Render.MNodeT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MNodeT