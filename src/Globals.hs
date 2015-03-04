{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals where

import Control.Lens (makeLenses)

import Game.CVarT

data Globals = Globals { _dedicated :: CVarT
                       }

makeLenses ''Globals

defaultGlobals :: Globals
defaultGlobals =
  Globals { _dedicated = CVarT "" "" "" 0 True 0.0 Nothing -- doesn't matter as it gets set in Main
          }
