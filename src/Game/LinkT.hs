{-# LANGUAGE TemplateHaskell #-}
module Game.LinkT ( LinkT(..)
                  , module Game.LinkT
                  ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''LinkT

newLinkT :: Int -> LinkT
newLinkT idx =
  LinkT { _lIndex = idx
        , _lPrev  = Nothing
        , _lNext  = Nothing
        , _lEdict = Nothing
        }
