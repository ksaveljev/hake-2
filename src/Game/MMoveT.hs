{-# LANGUAGE TemplateHaskell #-}
module Game.MMoveT ( MMoveT(..)
                   , module Game.MMoveT
                   , module Game.MFrameT
                   ) where

import Control.Lens (makeLenses)

import Internal
import Game.MFrameT

makeLenses ''MMoveT
