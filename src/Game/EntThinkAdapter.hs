{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntThinkAdapter ( EntThinkAdapter(..)
                            , module Game.EntThinkAdapter
                            ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EntThinkAdapter
