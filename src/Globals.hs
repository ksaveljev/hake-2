{-# LANGUAGE TemplateHaskell #-}
module Globals where

import Control.Lens (makeLenses)

import Game.CVarT

data Globals = Globals { _dedicated :: CVarT
                       }

makeLenses ''Globals
