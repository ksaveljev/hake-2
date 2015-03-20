{-# LANGUAGE TemplateHaskell #-}
module Game.GameImportT ( GameImportT(..)
                        , module Game.GameImportT
                        ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''GameImportT

newGameImportT :: GameImportT
newGameImportT = undefined -- TODO
