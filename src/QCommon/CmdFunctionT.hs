{-# LANGUAGE TemplateHaskell #-}
module QCommon.CmdFunctionT ( CmdFunctionT(..)
                            , cfFunction
                            , cfName
                            ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''CmdFunctionT
