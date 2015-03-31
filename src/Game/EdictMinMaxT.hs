{-# LANGUAGE TemplateHaskell #-}
module Game.EdictMinMaxT ( EdictMinMaxT(..)
                         , module Game.EdictMinMaxT
                         )where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Internal

makeLenses ''EdictMinMaxT

newEdictMinMaxT :: EdictMinMaxT
newEdictMinMaxT =
  EdictMinMaxT { _eMins   = V3 0 0 0
               , _eMaxs   = V3 0 0 0
               , _eAbsMin = V3 0 0 0
               , _eAbsMax = V3 0 0 0
               , _eSize   = V3 0 0 0
               }
