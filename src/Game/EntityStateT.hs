{-# LANGUAGE TemplateHaskell #-}
module Game.EntityStateT
  ( module Game.EntityStateT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''EntityStateT

newEntityStateT :: Maybe (Ref' EdictT) -> EntityStateT
newEntityStateT edictRef =
  EntityStateT { _esNumber         = number edictRef
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
  where number Nothing = 0
        number (Just (Ref _ idx)) = idx