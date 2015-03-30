{-# LANGUAGE TemplateHaskell #-}
module Game.TraceT ( TraceT(..)
                   , module Game.TraceT
                   ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''TraceT

newTraceT :: TraceT
newTraceT = undefined -- TODO
