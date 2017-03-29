{-# LANGUAGE TemplateHaskell #-}
module Sys.KBD ( KBD(..)
               , module Sys.KBD
               ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''KBD
