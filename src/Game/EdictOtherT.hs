{-# LANGUAGE TemplateHaskell #-}
module Game.EdictOtherT ( EdictOtherT(..)
                        , module Game.EdictOtherT
                        ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictOtherT
