{-# LANGUAGE TemplateHaskell #-}
module Game.EdictOtherT ( EdictOtherT(..)
                        , module Game.EdictOtherT
                        ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''EdictOtherT

newEdictOtherT :: EdictOtherT
newEdictOtherT =
  EdictOtherT { _eoChain        = Nothing
              , _eoEnemy        = Nothing
              , _eoOldEnemy     = Nothing
              , _eoActivator    = Nothing
              , _eoGroundEntity = Nothing
              , _eoTeamChain    = Nothing
              , _eoTeamMaster   = Nothing
              , _eoMyNoise      = Nothing
              , _eoMyNoise2     = Nothing
              }
