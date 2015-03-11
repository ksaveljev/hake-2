{-# LANGUAGE TemplateHaskell #-}
module Game.EdictTimingT ( EdictTimingT(..)
                         , module Game.EdictTimingT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictTimingT
