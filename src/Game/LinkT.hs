{-# LANGUAGE TemplateHaskell #-}
module Game.LinkT where

import Control.Lens (makeLenses)

data LinkT =
  LinkT { _lIndex :: Int
        , _lPrev  :: Maybe Int
        , _lNext  :: Maybe Int
        , _lEdict :: Maybe Int
        }

makeLenses ''LinkT

newLinkT :: Int -> LinkT
newLinkT idx =
  LinkT { _lIndex = idx
        , _lPrev  = Nothing
        , _lNext  = Nothing
        , _lEdict = Nothing
        }
