module Globals where

import Game.CVarT

data Globals = Globals { dedicated :: CVarT
                       }
