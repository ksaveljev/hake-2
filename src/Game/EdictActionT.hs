{-# LANGUAGE TemplateHaskell #-}
module Game.EdictActionT ( EdictActionT(..)
                         , module Game.EdictActionT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictActionT
