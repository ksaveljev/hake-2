{-# LANGUAGE TemplateHaskell #-}
module Client.EntityT
  ( module Client.EntityT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''EntityT

newEntityT :: EntityT
newEntityT =
  EntityT { _eModel      = Nothing
          , _eAngles     = V3 0 0 0
          , _eOrigin     = V3 0 0 0
          , _eFrame      = 0
          , _eOldOrigin  = V3 0 0 0
          , _eOldFrame   = 0
          , _eBackLerp   = 0
          , _eSkinNum    = 0
          , _eLightStyle = 0
          , _eAlpha      = 0
          , _eSkin       = Nothing
          , _enFlags     = 0
          }