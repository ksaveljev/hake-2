{-# LANGUAGE TemplateHaskell #-}
module Game.GClientT ( GClientT(..)
                     , module Game.GClientT
                     , module Game.ClientPersistantT
                     , module Game.ClientRespawnT
                     , module Game.PlayerStateT
                     ) where

import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector.Unboxed as UV

import Types
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.PlayerStateT
import Game.PMoveStateT

makeLenses ''GClientT

newGClientT :: Int -> GClientT
newGClientT idx =
  GClientT { _gcPlayerState        = newPlayerStateT
           , _gcPing               = 0
           , _gcPers               = newClientPersistantT
           , _gcResp               = newClientRespawnT
           , _gcOldPMove           = newPMoveStateT
           , _gcShowScores         = False
           , _gcShowInventory      = False
           , _gcShowHelp           = False
           , _gcShowHelpIcon       = False
           , _gcAmmoIndex          = 0
           , _gcButtons            = 0
           , _gcOldButtons         = 0
           , _gcLatchedButtons     = 0
           , _gcWeaponThunk        = False
           , _gcNewWeapon          = Nothing
           , _gcDamageArmor        = 0
           , _gcDamagePArmor       = 0
           , _gcDamageBlood        = 0
           , _gcDamageKnockback    = 0
           , _gcDamageFrom         = V3 0 0 0
           , _gcKillerYaw          = 0
           , _gcWeaponState        = 0
           , _gcKickAngles         = V3 0 0 0
           , _gcKickOrigin         = V3 0 0 0
           , _gcVDmgRoll           = 0
           , _gcVDmgPitch          = 0
           , _gcVDmgTime           = 0
           , _gcFallTime           = 0
           , _gcFallValue          = 0
           , _gcDamageAlpha        = 0
           , _gcBonusAlpha         = 0
           , _gcDamageBlend        = V3 0 0 0
           , _gcVAngle             = V3 0 0 0
           , _gcBobTime            = 0
           , _gcOldViewAngles      = V3 0 0 0
           , _gcOldVelocity        = V3 0 0 0
           , _gcNextDrownTime      = 0
           , _gcOldWaterLevel      = 0
           , _gcBreatherSound      = 0
           , _gcMachinegunShots    = 0
           , _gcAnimEnd            = 0
           , _gcAnimPriority       = 0
           , _gcAnimDuck           = False
           , _gcAnimRun            = False
           , _gcQuadFrameNum       = 0
           , _gcInvincibleFrameNum = 0
           , _gcBreatherFrameNum   = 0
           , _gcEnviroFrameNum     = 0
           , _gcGrenadeBlewUp      = False
           , _gcGrenadeTime        = 0
           , _gcSilencerShots      = 0
           , _gcWeaponSound        = 0
           , _gcPickupMsgTime      = 0
           , _gcFloodLockTill      = 0
           , _gcFloodWhen          = UV.replicate 10 0
           , _gcFloodWhenHead      = 0
           , _gcRespawnTime        = 0
           , _gcChaseTarget        = Nothing
           , _gcUpdateChase        = False
           , _gcIndex              = idx
           }
