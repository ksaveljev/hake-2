{-# LANGUAGE TemplateHaskell #-}
module Client.CheatVarT
    ( module Client.CheatVarT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''CheatVarT
