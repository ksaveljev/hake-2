{-# LANGUAGE TemplateHaskell #-}
module Game.EntityStateT where

import           Control.Lens (makeLenses)
import           Data.Maybe   (isJust)
import           Linear.V3    (V3(..))

import           Types

makeLenses ''EntityStateT

newEntityStateT :: Maybe (Ref EdictT) -> EntityStateT
newEntityStateT edictRef = EntityStateT
    { _esNumber         = number edictRef
    , _esOrigin         = V3 0 0 0
    , _esAngles         = V3 0 0 0
    , _esOldOrigin      = V3 0 0 0
    , _esModelIndex     = 0
    , _esModelIndex2    = 0
    , _esModelIndex3    = 0
    , _esModelIndex4    = 0
    , _esFrame          = 0
    , _esSkinNum        = 0
    , _esEffects        = 0
    , _esRenderFx       = 0
    , _esSolid          = 0
    , _esSound          = 0
    , _esEvent          = 0
    , _esSurroundingEnt = edictRef
    }
  where
    number Nothing = 0
    number (Just (Ref idx)) = idx
