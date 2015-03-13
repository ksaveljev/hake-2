{-# LANGUAGE TemplateHaskell #-}
module Server.ServerStaticT ( ServerStaticT(..)
                            , module Server.ServerStaticT
                            , module Server.ClientT
                            ) where

import Control.Lens (makeLenses)

import Internal
import Server.ClientT

makeLenses ''ServerStaticT

newServerStaticT :: ServerStaticT
newServerStaticT = undefined -- TODO
