{-# LANGUAGE TemplateHaskell #-}
module Client.CheatVarT
  ( module Client.CheatVarT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CheatVarT
