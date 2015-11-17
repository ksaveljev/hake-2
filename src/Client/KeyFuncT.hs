{-# LANGUAGE TemplateHaskell #-}
module Client.KeyFuncT ( KeyFuncT(..)
                       , module Client.KeyFuncT
                       ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''KeyFuncT
