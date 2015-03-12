{-# LANGUAGE TemplateHaskell #-}
module Server.ServerStaticT ( ServerStaticT(..)
                            , module Server.ServerStaticT
                            ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ServerStaticT

newServerStaticT :: ServerStaticT
newServerStaticT = undefined -- TODO
