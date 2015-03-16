{-# LANGUAGE TemplateHaskell #-}
module Server.ServerT ( ServerT(..)
                      , module Server.ServerT
                      ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ServerT

-- configstrings must be initialized to full vector of values
newServerT :: ServerT
newServerT = undefined -- TODO
