module Game.GameMisc where

import Linear (V3)
import qualified Data.ByteString as B

import Types
import QuakeState
import Game.Adapters

lightUse :: EntUse

spPathCorner :: Ref EdictT -> Quake ()

pathCornerTouch :: EntTouch

spPointCombat :: Ref EdictT -> Quake ()

spViewThing :: Ref EdictT -> Quake ()

thViewThing :: EntThink

spInfoNull :: Ref EdictT -> Quake ()

spInfoNotNull :: Ref EdictT -> Quake ()

spLight :: Ref EdictT -> Quake ()

spFuncWall :: Ref EdictT -> Quake ()

spFuncObject :: Ref EdictT -> Quake ()

spFuncExplosive :: Ref EdictT -> Quake ()

spMiscExploBox :: Ref EdictT -> Quake ()

spMiscBlackHole :: Ref EdictT -> Quake ()

spMiscEasterTank :: Ref EdictT -> Quake ()

spMiscEasterChick :: Ref EdictT -> Quake ()

spMiscEasterChick2 :: Ref EdictT -> Quake ()

spMonsterCommanderBody :: Ref EdictT -> Quake ()

spMiscBanner :: Ref EdictT -> Quake ()

spMiscDeadSoldier :: Ref EdictT -> Quake ()

spMiscViper :: Ref EdictT -> Quake ()

miscViperUse :: EntUse

spMiscBigViper :: Ref EdictT -> Quake ()

spMiscViperBomb :: Ref EdictT -> Quake ()

miscViperBombUse :: EntUse

miscViperBombPrethink :: EntThink

miscViperBombTouch :: EntTouch

spMiscStroggShip :: Ref EdictT -> Quake ()

spMiscSatelliteDish :: Ref EdictT -> Quake ()

miscSatelliteDishUse :: EntUse

miscSatelliteDishThink :: EntThink

spLightMine1 :: Ref EdictT -> Quake ()

spLightMine2 :: Ref EdictT -> Quake ()

spMiscGibArm :: Ref EdictT -> Quake ()

spMiscGibLeg :: Ref EdictT -> Quake ()

spMiscGibHead :: Ref EdictT -> Quake ()

spTargetCharacter :: Ref EdictT -> Quake ()

spTargetString :: Ref EdictT -> Quake ()

targetStringUse :: EntUse

spFuncClock :: Ref EdictT -> Quake ()

funcClockReset :: Ref EdictT -> Quake ()

funcClockThink :: EntThink

funcClockFormatCountdown :: Ref EdictT -> Quake ()

funcClockUse :: EntUse

spMiscTeleporter :: Ref EdictT -> Quake ()

teleporterTouch :: EntTouch

spFuncAreaPortal :: EntThink

spMiscTeleporterDest :: EntThink

miscDeadSoldierDie :: EntDie

throwGib :: Ref EdictT -> B.ByteString -> Int -> Int -> Quake ()

gibTouch :: EntTouch

gibThink :: EntThink

clipGibVelocity :: Ref EdictT -> Quake ()

velocityForDamage :: Int -> Quake (V3 Float)

throwHead :: Ref EdictT -> B.ByteString -> Int -> Int -> Quake ()

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

throwDebris :: Ref EdictT -> B.ByteString -> Float -> V3 Float -> Quake ()

becomeExplosion1 :: Ref EdictT -> Quake ()

becomeExplosion2 :: Ref EdictT -> Quake ()

debrisDie :: EntDie
