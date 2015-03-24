{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.ItemUseAdapter ( ItemUseAdapter(..)
                           , module Game.ItemUseAdapter
                           ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ItemUseAdapter
