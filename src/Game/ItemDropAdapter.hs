{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.ItemDropAdapter ( ItemDropAdapter(..)
                            , module Game.ItemDropAdapter
                            ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ItemDropAdapter
