module Globals where

import Game.CVarT

data Globals = Globals { _dedicated :: CVarT
                       }
