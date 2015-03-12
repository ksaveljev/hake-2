{-# LANGUAGE TemplateHaskell #-}
module Server.ServerT ( ServerT(..)
                      , module Server.ServerT
                      ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ServerT

newServerT :: ServerT
newServerT = undefined -- TODO
