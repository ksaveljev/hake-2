module Game.GameMisc
    ( spFuncAreaPortal
    , spFuncClock
    , spFuncExplosive
    , spFuncObject
    , spFuncWall
    , spInfoNotNull
    , spInfoNull
    , spLight
    , spLightMine1
    , spLightMine2
    , spMiscBanner
    , spMiscBigViper
    , spMiscBlackHole
    , spMiscDeadSoldier
    , spMiscEasterChick
    , spMiscEasterChick2
    , spMiscEasterTank
    , spMiscExploBox
    , spMiscGibArm
    , spMiscGibHead
    , spMiscGibLeg
    , spMiscSatelliteDish
    , spMiscStroggShip
    , spMiscTeleporter
    , spMiscTeleporterDest
    , spMiscViper
    , spMiscViperBomb
    , spMonsterCommanderBody
    , spPathCorner
    , spPointCombat
    , spTargetCharacter
    , spTargetString
    , spViewThing
    , throwGib
    , throwHead
    ) where

import qualified Data.ByteString as B

import           Types

spFuncAreaPortal :: EntThink
spFuncAreaPortal = error "GameMisc.spFuncAreaPortal" -- TODO

spFuncClock :: Ref EdictT -> Quake ()
spFuncClock = error "GameMisc.spFuncClock" -- TODO

spFuncExplosive :: Ref EdictT -> Quake ()
spFuncExplosive = error "GameMisc.spFuncExplosive" -- TODO

spFuncObject :: Ref EdictT -> Quake ()
spFuncObject = error "GameMisc.spFuncObject" -- TODO

spFuncWall :: Ref EdictT -> Quake ()
spFuncWall = error "GameMisc.spFuncWall" -- TODO

spInfoNotNull :: Ref EdictT -> Quake ()
spInfoNotNull = error "GameMisc.spInfoNotNull" -- TODO

spInfoNull :: Ref EdictT -> Quake ()
spInfoNull = error "GameMisc.spInfoNull" -- TODO

spLight :: Ref EdictT -> Quake ()
spLight = error "GameMisc.spLight" -- TODO

spLightMine1 :: Ref EdictT -> Quake ()
spLightMine1 = error "GameMisc.spLightMine1" -- TODO

spLightMine2 :: Ref EdictT -> Quake ()
spLightMine2 = error "GameMisc.spLightMine2" -- TODO

spMiscBanner :: Ref EdictT -> Quake ()
spMiscBanner = error "GameMisc.spMiscBanner" -- TODO

spMiscBigViper :: Ref EdictT -> Quake ()
spMiscBigViper = error "GameMisc.spMiscBigViper" -- TODO

spMiscBlackHole :: Ref EdictT -> Quake ()
spMiscBlackHole = error "GameMisc.spMiscBlackHole" -- TODO

spMiscDeadSoldier :: Ref EdictT -> Quake ()
spMiscDeadSoldier = error "GameMisc.spMiscDeadSoldier" -- TODO

spMiscEasterChick :: Ref EdictT -> Quake ()
spMiscEasterChick = error "GameMisc.spMiscEasterChick" -- TODO

spMiscEasterChick2 :: Ref EdictT -> Quake ()
spMiscEasterChick2 = error "GameMisc.spMiscEasterChick2" -- TODO

spMiscEasterTank :: Ref EdictT -> Quake ()
spMiscEasterTank = error "GameMisc.spMiscEasterTank" -- TODO

spMiscExploBox :: Ref EdictT -> Quake ()
spMiscExploBox = error "GameMisc.spMiscExploBox" -- TODO

spMiscGibArm :: Ref EdictT -> Quake ()
spMiscGibArm = error "GameMisc.spMiscGibArm" -- TODO

spMiscGibHead :: Ref EdictT -> Quake ()
spMiscGibHead = error "GameMisc.spMiscGibHead" -- TODO

spMiscGibLeg :: Ref EdictT -> Quake ()
spMiscGibLeg = error "GameMisc.spMiscGibLeg" -- TODO

spMiscSatelliteDish :: Ref EdictT -> Quake ()
spMiscSatelliteDish = error "GameMisc.spMiscSatelliteDish" -- TODO

spMiscStroggShip :: Ref EdictT -> Quake ()
spMiscStroggShip = error "GameMisc.spMiscStroggShip" -- TODO

spMiscTeleporter :: Ref EdictT -> Quake ()
spMiscTeleporter = error "GameMisc.spMiscTeleporter" -- TODO

spMiscTeleporterDest :: EntThink
spMiscTeleporterDest = error "GameMisc.spMiscTeleporterDest" -- TODO

spMiscViper :: Ref EdictT -> Quake ()
spMiscViper = error "GameMisc.spMiscViper" -- TODO

spMiscViperBomb :: Ref EdictT -> Quake ()
spMiscViperBomb = error "GameMisc.spMiscViperBomb" -- TODO

spMonsterCommanderBody :: Ref EdictT -> Quake ()
spMonsterCommanderBody = error "GameMisc.spMonsterCommanderBody" -- TODO

spPathCorner :: Ref EdictT -> Quake ()
spPathCorner = error "GameMisc.spPathCorner" -- TODO

spPointCombat :: Ref EdictT -> Quake ()
spPointCombat = error "GameMisc.spPointCombat" -- TODO

spTargetCharacter :: Ref EdictT -> Quake ()
spTargetCharacter = error "GameMisc.spTargetCharacter" -- TODO

spTargetString :: Ref EdictT -> Quake ()
spTargetString = error "GameMisc.spTargetString" -- TODO

spViewThing :: Ref EdictT -> Quake ()
spViewThing = error "GameMisc.spViewThing" -- TODO

throwGib :: Ref EdictT -> B.ByteString -> Int -> Int -> Quake ()
throwGib = error "GameMisc.throwGib" -- TODO

throwHead :: Ref EdictT -> B.ByteString -> Int -> Int -> Quake ()
throwHead = error "GameMisc.throwHead" -- TODO