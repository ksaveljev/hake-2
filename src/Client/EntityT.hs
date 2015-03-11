{-# LANGUAGE TemplateHaskell #-}
module Client.EntityT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

data EntityT =
  EntityT { _eModel      :: ModelT
          , _eAngles     :: V3 Float
          , _eOrigin     :: V3 Float
          , _eFrame      :: Int
          , _eOldOrigin  :: V3 Float
          , _eOldFrame   :: Int
          , _eBackLerp   :: Float
          , _eSkinNum    :: Int
          , _eLightStyle :: Int
          , _eAlpha      :: Float
          , _eSkin       :: ImageT
          , _eFlags      :: Int
          }

makeLenses ''EntityT
