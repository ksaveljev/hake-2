{-# LANGUAGE TemplateHaskell #-}
module Game.TraceT ( TraceT(..)
                   , module Game.TraceT
                   ) where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Internal

makeLenses ''TraceT
