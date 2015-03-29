{-# LANGUAGE TemplateHaskell #-}
module Game.EdictTimingT ( EdictTimingT(..)
                         , module Game.EdictTimingT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictTimingT

newEdictTimingT :: EdictTimingT
newEdictTimingT =
  EdictTimingT { _etTouchDebounceTime    = 0
               , _etPainDebounceTime     = 0
               , _etDamageDebounceTime   = 0
               , _etFlySoundDebounceTime = 0
               , _etLastMoveTime         = 0
               }
