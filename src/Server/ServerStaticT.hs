{-# LANGUAGE TemplateHaskell #-}
module Server.ServerStaticT ( ServerStaticT(..)
                            , module Server.ServerStaticT
                            , module Server.ClientT
                            , module Game.EntityStateT
                            ) where

import Control.Lens (makeLenses)

import Internal
import Game.EntityStateT
import Server.ClientT

makeLenses ''ServerStaticT

newServerStaticT :: ServerStaticT
newServerStaticT = undefined -- TODO
