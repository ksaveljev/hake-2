{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntInteractAdapter ( EntInteractAdapter(..)
                               , module Game.EntInteractAdapter
                               ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EntInteractAdapter
