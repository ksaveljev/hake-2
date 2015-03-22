{-# LANGUAGE TemplateHaskell #-}
module Client.EntityT ( EntityT(..)
                      , module Client.EntityT
                      ) where

import Control.Lens (makeLenses)

makeLenses ''EntityT

newEntityT :: EntityT
newEntityT = undefined -- TODO
