{-# LANGUAGE TemplateHaskell #-}
module QCommon.CmdFunctionT
  (module QCommon.CmdFunctionT)
  where

import Types

import Control.Lens (makeLenses)

makeLenses ''CmdFunctionT