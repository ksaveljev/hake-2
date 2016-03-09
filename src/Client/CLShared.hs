module Client.CLShared
  ( quitF
  ) where

import Types

quitF :: XCommandT
quitF = error "CL.quitF" -- TODO