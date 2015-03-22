{-# LANGUAGE TemplateHaskell #-}
module Client.ClientStateT ( ClientStateT(..)
                           , module Client.ClientStateT
                           ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ClientStateT

newClientStateT :: ClientStateT
newClientStateT = undefined -- TODO
