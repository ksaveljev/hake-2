{-# LANGUAGE OverloadedStrings #-}
module Game.GameMisc where

import Quake
import QuakeState
import Game.EntThink

spPathCorner :: EdictReference -> Quake ()
spPathCorner _ = io (putStrLn "GameMisc.spPathCorner") >> undefined -- TODO

spPointCombat :: EdictReference -> Quake ()
spPointCombat _ = io (putStrLn "GameMisc.spPointCombat") >> undefined -- TODO

spViewThing :: EdictReference -> Quake ()
spViewThing _ = io (putStrLn "GameMisc.spViewThing") >> undefined -- TODO

spInfoNull :: EdictReference -> Quake ()
spInfoNull _ = io (putStrLn "GameMisc.spInfoNull") >> undefined -- TODO

spInfoNotNull :: EdictReference -> Quake ()
spInfoNotNull _ = io (putStrLn "GameMisc.spInfoNotNull") >> undefined -- TODO

spLight :: EdictReference -> Quake ()
spLight _ = io (putStrLn "GameMisc.spLight") >> undefined -- TODO

spFuncWall :: EdictReference -> Quake ()
spFuncWall _ = io (putStrLn "GameMisc.spFuncWall") >> undefined -- TODO

spFuncObject :: EdictReference -> Quake ()
spFuncObject _ = io (putStrLn "GameMisc.spFuncObject") >> undefined -- TODO

spFuncExplosive :: EdictReference -> Quake ()
spFuncExplosive _ = io (putStrLn "GameMisc.spFuncExplosive") >> undefined -- TODO

spMiscExploBox :: EdictReference -> Quake ()
spMiscExploBox _ = io (putStrLn "GameMisc.spMiscExploBox") >> undefined -- TODO

spMiscBlackHole :: EdictReference -> Quake ()
spMiscBlackHole _ = io (putStrLn "GameMisc.spMiscBlackHole") >> undefined -- TODO

spMiscEasterTank :: EdictReference -> Quake ()
spMiscEasterTank _ = io (putStrLn "GameMisc.spMiscEasterTank") >> undefined -- TODO

spMiscEasterChick :: EdictReference -> Quake ()
spMiscEasterChick _ = io (putStrLn "GameMisc.spMiscEasterChick") >> undefined -- TODO

spMiscEasterChick2 :: EdictReference -> Quake ()
spMiscEasterChick2 _ = io (putStrLn "GameMisc.spMiscEasterChick2") >> undefined -- TODO

spMonsterCommanderBody :: EdictReference -> Quake ()
spMonsterCommanderBody _ = io (putStrLn "GameMisc.spMonsterCommanderBody") >> undefined -- TODO

spMiscBanner :: EdictReference -> Quake ()
spMiscBanner _ = io (putStrLn "GameMisc.spMiscBanner") >> undefined -- TODO

spMiscDeadSoldier :: EdictReference -> Quake ()
spMiscDeadSoldier _ = io (putStrLn "GameMisc.spMiscDeadSoldier") >> undefined -- TODO

spMiscViper :: EdictReference -> Quake ()
spMiscViper _ = io (putStrLn "GameMisc.spMiscViper") >> undefined -- TODO

spMiscBigViper :: EdictReference -> Quake ()
spMiscBigViper _ = io (putStrLn "GameMisc.spMiscBigViper") >> undefined -- TODO

spMiscViperBomb :: EdictReference -> Quake ()
spMiscViperBomb _ = io (putStrLn "GameMisc.spMiscViperBomb") >> undefined -- TODO

spMiscStroggShip :: EdictReference -> Quake ()
spMiscStroggShip _ = io (putStrLn "GameMisc.spMiscStroggShip") >> undefined -- TODO

spMiscSatelliteDish :: EdictReference -> Quake ()
spMiscSatelliteDish _ = io (putStrLn "GameMisc.spMiscSatelliteDish") >> undefined -- TODO

spLightMine1 :: EdictReference -> Quake ()
spLightMine1 _ = io (putStrLn "GameMisc.spLightMine1") >> undefined -- TODO

spLightMine2 :: EdictReference -> Quake ()
spLightMine2 _ = io (putStrLn "GameMisc.spLightMine2") >> undefined -- TODO

spMiscGibArm :: EdictReference -> Quake ()
spMiscGibArm _ = io (putStrLn "GameMisc.spMiscGibArm") >> undefined -- TODO

spMiscGibLeg :: EdictReference -> Quake ()
spMiscGibLeg _ = io (putStrLn "GameMisc.spMiscGibLeg") >> undefined -- TODO

spMiscGibHead :: EdictReference -> Quake ()
spMiscGibHead _ = io (putStrLn "GameMisc.spMiscGibHead") >> undefined -- TODO

spTargetCharacter :: EdictReference -> Quake ()
spTargetCharacter _ = io (putStrLn "GameMisc.spTargetCharacter") >> undefined -- TODO

spTargetString :: EdictReference -> Quake ()
spTargetString _ = io (putStrLn "GameMisc.spTargetString") >> undefined -- TODO

spFuncClock :: EdictReference -> Quake ()
spFuncClock _ = io (putStrLn "GameMisc.spFuncClock") >> undefined -- TODO

spMiscTeleporter :: EdictReference -> Quake ()
spMiscTeleporter _ = io (putStrLn "GameMisc.spMiscTeleporter") >> undefined -- TODO

spFuncAreaPortal :: EntThink
spFuncAreaPortal =
  GenericEntThink "sp_func_areaportal" $ \_ -> do
    io (putStrLn "GameMisc.spFuncAreaPortal") >> undefined -- TODO

spMiscTeleporterDest :: EntThink
spMiscTeleporterDest =
  GenericEntThink "SP_misc_teleporter_dest" $ \_ -> do
    io (putStrLn "GameMisc.spMiscTeleporterDest") >> undefined -- TODO
