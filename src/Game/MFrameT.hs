{-# LANGUAGE TemplateHaskell #-}
module Game.MFrameT ( MFrameT(..)
                    , module Game.MFrameT
                    ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MFrameT
