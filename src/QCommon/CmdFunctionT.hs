{-# LANGUAGE TemplateHaskell #-}
module QCommon.CmdFunctionT ( CmdFunctionT
                            , cfFunction
                            , cfName
                            ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''CmdFunctionT
