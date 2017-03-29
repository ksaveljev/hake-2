{-# LANGUAGE TemplateHaskell #-}
module QCommon.DPackHeaderT where

import Control.Lens (makeLenses)

import Types

data DPackHeaderT =
  DPackHeaderT { _dphIdent  :: Int -- IDPAKHEADER
               , _dphDirOfs :: Int
               , _dphDirLen :: Int
               }

makeLenses ''DPackHeaderT
