module Game.GameTrigger
    ( spTriggerAlways
    , spTriggerCounter
    , spTriggerGravity
    , spTriggerHurt
    , spTriggerKey
    , spTriggerMonsterJump
    , spTriggerMultiple
    , spTriggerOnce
    , spTriggerPush
    , spTriggerRelay
    ) where

import           Control.Lens      (use, (^.), (.=), (%=), (&), (.~), (%~), (-~))
import           Control.Monad     (when, unless)
import           Data.Bits         (complement, (.&.), (.|.))
import qualified Data.ByteString   as B
import           Linear            (_y, _z)

import qualified Constants
import qualified Game.GameBase     as GameBase
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameUtil     as GameUtil
import           Game.LevelLocalsT
import           Game.SpawnTempT
import qualified QCommon.Com       as Com
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary       (encode)
import qualified Util.Lib          as Lib

spTriggerAlways :: Ref EdictT -> Quake ()
spTriggerAlways = error "GameTrigger.spTriggerAlways" -- TODO

spTriggerCounter :: Ref EdictT -> Quake ()
spTriggerCounter = error "GameTrigger.spTriggerCounter" -- TODO

spTriggerGravity :: Ref EdictT -> Quake ()
spTriggerGravity = error "GameTrigger.spTriggerGravity" -- TODO

spTriggerHurt :: Ref EdictT -> Quake ()
spTriggerHurt = error "GameTrigger.spTriggerHurt" -- TODO

spTriggerKey :: Ref EdictT -> Quake ()
spTriggerKey = error "GameTrigger.spTriggerKey" -- TODO

spTriggerMonsterJump :: Ref EdictT -> Quake ()
spTriggerMonsterJump = error "GameTrigger.spTriggerMonsterJump" -- TODO

spTriggerMultiple :: Ref EdictT -> Quake ()
spTriggerMultiple = error "GameTrigger.spTriggerMultiple" -- TODO

spTriggerOnce :: Ref EdictT -> Quake ()
spTriggerOnce = error "GameTrigger.spTriggerOnce" -- TODO

spTriggerPush :: Ref EdictT -> Quake ()
spTriggerPush = error "GameTrigger.spTriggerPush" -- TODO

spTriggerRelay :: Ref EdictT -> Quake ()
spTriggerRelay = error "GameTrigger.spTriggerRelay" -- TODO