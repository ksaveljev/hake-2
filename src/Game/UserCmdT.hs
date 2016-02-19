{-# LANGUAGE TemplateHaskell #-}
module Game.UserCmdT
  ( module Game.UserCmdT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''UserCmdT