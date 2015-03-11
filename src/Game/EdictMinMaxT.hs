{-# LANGUAGE TemplateHaskell #-}
module Game.EdictMinMaxT ( EdictMinMaxT(..)
                         , module Game.EdictMinMaxT
                         )where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictMinMaxT
