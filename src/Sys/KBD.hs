{-# LANGUAGE TemplateHaskell #-}
module Sys.KBD ( KBD(..)
               , module Sys.KBD
               ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''KBD
