{-# LANGUAGE TemplateHaskell #-}
module Game.EdictActionT ( EdictActionT(..)
                         , module Game.EdictActionT
                         ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''EdictActionT

newEdictActionT :: EdictActionT
newEdictActionT =
  EdictActionT { _eaNextThink = 0
               , _eaPrethink  = Nothing
               , _eaThink     = Nothing
               , _eaBlocked   = Nothing
               , _eaTouch     = Nothing
               , _eaUse       = Nothing
               , _eaPain      = Nothing
               , _eaDie       = Nothing
               }
