{-# LANGUAGE TemplateHaskell #-}
module Game.MMoveT ( MMoveT(..)
                   , module Game.MMoveT
                   ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MMoveT
