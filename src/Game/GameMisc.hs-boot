module Game.GameMisc where

import Linear (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState
import Game.Adapters

lightUse :: EntUse

spPathCorner :: EdictReference -> Quake ()

pathCornerTouch :: EntTouch

spPointCombat :: EdictReference -> Quake ()

spViewThing :: EdictReference -> Quake ()

thViewThing :: EntThink

spInfoNull :: EdictReference -> Quake ()

spInfoNotNull :: EdictReference -> Quake ()

spLight :: EdictReference -> Quake ()

spFuncWall :: EdictReference -> Quake ()

spFuncObject :: EdictReference -> Quake ()

spFuncExplosive :: EdictReference -> Quake ()

spMiscExploBox :: EdictReference -> Quake ()

spMiscBlackHole :: EdictReference -> Quake ()

spMiscEasterTank :: EdictReference -> Quake ()

spMiscEasterChick :: EdictReference -> Quake ()

spMiscEasterChick2 :: EdictReference -> Quake ()

spMonsterCommanderBody :: EdictReference -> Quake ()

spMiscBanner :: EdictReference -> Quake ()

spMiscDeadSoldier :: EdictReference -> Quake ()

spMiscViper :: EdictReference -> Quake ()

miscViperUse :: EntUse

spMiscBigViper :: EdictReference -> Quake ()

spMiscViperBomb :: EdictReference -> Quake ()

miscViperBombUse :: EntUse

miscViperBombPrethink :: EntThink

miscViperBombTouch :: EntTouch

spMiscStroggShip :: EdictReference -> Quake ()

spMiscSatelliteDish :: EdictReference -> Quake ()

miscSatelliteDishUse :: EntUse

miscSatelliteDishThink :: EntThink

spLightMine1 :: EdictReference -> Quake ()

spLightMine2 :: EdictReference -> Quake ()

spMiscGibArm :: EdictReference -> Quake ()

spMiscGibLeg :: EdictReference -> Quake ()

spMiscGibHead :: EdictReference -> Quake ()

spTargetCharacter :: EdictReference -> Quake ()

spTargetString :: EdictReference -> Quake ()

targetStringUse :: EntUse

spFuncClock :: EdictReference -> Quake ()

funcClockReset :: EdictReference -> Quake ()

funcClockThink :: EntThink

funcClockFormatCountdown :: EdictReference -> Quake ()

funcClockUse :: EntUse

spMiscTeleporter :: EdictReference -> Quake ()

teleporterTouch :: EntTouch

spFuncAreaPortal :: EntThink

spMiscTeleporterDest :: EntThink

miscDeadSoldierDie :: EntDie

throwGib :: EdictReference -> B.ByteString -> Int -> Int -> Quake ()

gibTouch :: EntTouch

gibThink :: EntThink

clipGibVelocity :: EdictReference -> Quake ()

velocityForDamage :: Int -> Quake (V3 Float)

throwHead :: EdictReference -> B.ByteString -> Int -> Int -> Quake ()

barrelDelay :: EntDie

barrelExplode :: EntThink

barrelTouch :: EntTouch

gibDie :: EntDie

useAreaPortal :: EntUse

funcExplosiveUse :: EntUse

funcExplosiveSpawn :: EntUse

funcExplosiveExplode :: EntDie

pointCombatTouch :: EntTouch

miscStroggShipUse :: EntUse

funcWallUse :: EntUse

miscBannerThink :: EntThink

funcObjectRelease :: EntThink

funcObjectUse :: EntUse

funcObjectTouch :: EntTouch

miscBlackHoleUse :: EntUse

miscBlackHoleThink :: EntThink

miscEasterTankThink :: EntThink

miscEasterChickThink :: EntThink

miscEasterChick2Think :: EntThink

commanderBodyUse :: EntUse

commanderBodyThink :: EntThink

commanderBodyDrop :: EntThink

throwDebris :: EdictReference -> B.ByteString -> Float -> V3 Float -> Quake ()

becomeExplosion1 :: EdictReference -> Quake ()

becomeExplosion2 :: EdictReference -> Quake ()

debrisDie :: EntDie
