{-# LANGUAGE TemplateHaskell #-}
module Game.EdictStatusT ( EdictStatusT(..)
                         , module Game.EdictStatusT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictStatusT
