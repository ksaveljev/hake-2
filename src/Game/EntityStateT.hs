{-# LANGUAGE TemplateHaskell #-}
module Game.EntityStateT ( EntityStateT(..)
                         , module Game.EntityStateT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EntityStateT
