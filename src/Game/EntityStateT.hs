{-# LANGUAGE TemplateHaskell #-}
module Game.EntityStateT ( EntityStateT(..)
                         , module Game.EntityStateT
                         ) where

import Control.Lens (makeLenses)
import Linear.V3 (V3(..))

import Internal

makeLenses ''EntityStateT

newEntityStateT :: EntityStateT
newEntityStateT =
  EntityStateT { _esNumber         = 0
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
               }
