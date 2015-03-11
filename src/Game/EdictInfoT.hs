{-# LANGUAGE TemplateHaskell #-}
module Game.EdictInfoT ( EdictInfoT(..)
                       , module Game.EdictInfoT
                       ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictInfoT
