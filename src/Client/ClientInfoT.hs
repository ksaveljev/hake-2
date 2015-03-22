{-# LANGUAGE TemplateHaskell #-}
module Client.ClientInfoT ( ClientInfoT(..)
                          , module Client.ClientInfoT
                          ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ClientInfoT

newClientInfoT :: ClientInfoT
newClientInfoT = undefined -- TODO
