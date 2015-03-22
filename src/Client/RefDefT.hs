{-# LANGUAGE TemplateHaskell #-}
module Client.RefDefT ( RefDefT(..)
                      , module Client.RefDefT
                      ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''RefDefT

newRefDefT :: RefDefT
newRefDefT = undefined -- TODO
