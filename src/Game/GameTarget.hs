module Game.GameTarget
    ( spTargetBlaster
    , spTargetChangeLevel
    , spTargetCrossLevelTarget
    , spTargetCrossLevelTrigger
    , spTargetEarthquake
    , spTargetExplosion
    , spTargetGoal
    , spTargetHelp
    , spTargetLaser
    , spTargetLightRamp
    , spTargetSecret
    , spTargetSpawner
    , spTargetSpeaker
    , spTargetSplash
    , spTargetTempEntity
    ) where

import           Control.Applicative   ((<|>))
import           Control.Lens          (use, (^.), (.=), (%=), (+=), (&), (.~), (%~))
import           Control.Monad         (when, void)
import           Data.Bits             ((.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (fromMaybe, isNothing)
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase         as GameBase
import qualified Game.GameCombat       as GameCombat
import {-# SOURCE #-} Game.GameImportT
import           Game.GameLocalsT
import {-# SOURCE #-} qualified Game.GameSpawn        as GameSpawn
import qualified Game.GameUtil         as GameUtil
import qualified Game.GameWeapon       as GameWeapon
import           Game.LevelLocalsT
import           Game.SpawnTempT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib

spTargetBlaster :: Ref EdictT -> Quake ()
spTargetBlaster = error "GameTarget.spTargetBlaster" -- TODO

spTargetChangeLevel :: Ref EdictT -> Quake ()
spTargetChangeLevel = error "GameTarget.spTargetChangeLevel" -- TODO

spTargetCrossLevelTarget :: Ref EdictT -> Quake ()
spTargetCrossLevelTarget = error "GameTarget.spTargetCrossLevelTarget" -- TODO

spTargetCrossLevelTrigger :: Ref EdictT -> Quake ()
spTargetCrossLevelTrigger = error "GameTarget.spTargetCrossLevelTrigger" -- TODO

spTargetEarthquake :: Ref EdictT -> Quake ()
spTargetEarthquake = error "GameTarget.spTargetEarthquake" -- TODO

spTargetExplosion :: Ref EdictT -> Quake ()
spTargetExplosion = error "GameTarget.spTargetExplosion" -- TODO

spTargetGoal :: Ref EdictT -> Quake ()
spTargetGoal = error "GameTarget.spTargetGoal" -- TODO

spTargetHelp :: Ref EdictT -> Quake ()
spTargetHelp = error "GameTarget.spTargetHelp" -- TODO

spTargetLaser :: Ref EdictT -> Quake ()
spTargetLaser = error "GameTarget.spTargetLaser" -- TODO

spTargetLightRamp :: Ref EdictT -> Quake ()
spTargetLightRamp = error "GameTarget.spTargetLightRamp" -- TODO

spTargetSecret :: Ref EdictT -> Quake ()
spTargetSecret = error "GameTarget.spTargetSecret" -- TODO

spTargetSpawner :: Ref EdictT -> Quake ()
spTargetSpawner = error "GameTarget.spTargetSpawner" -- TODO

spTargetSpeaker :: Ref EdictT -> Quake ()
spTargetSpeaker = error "GameTarget.spTargetSpeaker" -- TODO

spTargetSplash :: Ref EdictT -> Quake ()
spTargetSplash = error "GameTarget.spTargetSplash" -- TODO

spTargetTempEntity :: Ref EdictT -> Quake ()
spTargetTempEntity = error "GameTarget.spTargetTempEntity" -- TODO