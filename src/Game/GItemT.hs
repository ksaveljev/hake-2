{-# LANGUAGE TemplateHaskell #-}
module Game.GItemT ( GItemT(..)
                   , module Game.GItemT
                   ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''GItemT
