{-# LANGUAGE TemplateHaskell #-}
module Game.LinkT where

import Control.Lens (makeLenses)

data LinkT =
  LinkT { _lPrev :: Maybe LinkT
        , _lNext :: Maybe LinkT
        --, object :: ??? -- TODO: is it needed?
        }

makeLenses ''LinkT
