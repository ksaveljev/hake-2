{-# LANGUAGE TemplateHaskell #-}
module Game.EdictT ( module Game.EdictT
                   ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictT
