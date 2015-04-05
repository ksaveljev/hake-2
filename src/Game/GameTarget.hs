{-# LANGUAGE OverloadedStrings #-}
module Game.GameTarget where

import Control.Lens (ix, (.=), zoom)

import Quake
import QuakeState
import Game.Adapters
import qualified Constants

spTargetTempEntity :: EdictReference -> Quake ()
spTargetTempEntity _ = io (putStrLn "GameTarget.spTargetTempEntity") >> undefined -- TODO

spTargetSpeaker :: EdictReference -> Quake ()
spTargetSpeaker _ = io (putStrLn "GameTarget.spTargetSpeaker") >> undefined -- TODO

spTargetHelp :: EdictReference -> Quake ()
spTargetHelp _ = io (putStrLn "GameTarget.spTargetHelp") >> undefined -- TODO

spTargetSecret :: EdictReference -> Quake ()
spTargetSecret _ = io (putStrLn "GameTarget.spTargetSecret") >> undefined -- TODO

spTargetGoal :: EdictReference -> Quake ()
spTargetGoal _ = io (putStrLn "GameTarget.spTargetGoal") >> undefined -- TODO

spTargetExplosion :: EdictReference -> Quake ()
spTargetExplosion (EdictReference edictIdx) = do
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEdictAction.eaUse .= Just useTargetExplosion
      eSvFlags .= Constants.svfNoClient

spTargetChangeLevel :: EdictReference -> Quake ()
spTargetChangeLevel _ = io (putStrLn "GameTarget.spTargetChangeLevel") >> undefined -- TODO

spTargetSplash :: EdictReference -> Quake ()
spTargetSplash _ = io (putStrLn "GameTarget.spTargetSplash") >> undefined -- TODO

spTargetSpawner :: EdictReference -> Quake ()
spTargetSpawner _ = io (putStrLn "GameTarget.spTargetSpawner") >> undefined -- TODO

spTargetBlaster :: EdictReference -> Quake ()
spTargetBlaster _ = io (putStrLn "GameTarget.spTargetBlaster") >> undefined -- TODO

spTargetCrossLevelTrigger :: EdictReference -> Quake ()
spTargetCrossLevelTrigger _ = io (putStrLn "GameTarget.spTargetCrossLevelTrigger") >> undefined -- TODO

spTargetCrossLevelTarget :: EdictReference -> Quake ()
spTargetCrossLevelTarget _ = io (putStrLn "GameTarget.spTargetCrossLevelTarget") >> undefined -- TODO

spTargetLaser :: EdictReference -> Quake ()
spTargetLaser _ = io (putStrLn "GameTarget.spTargetLaser") >> undefined -- TODO

spTargetLightRamp :: EdictReference -> Quake ()
spTargetLightRamp _ = io (putStrLn "GameTarget.spTargetLightRamp") >> undefined -- TODO

spTargetEarthquake :: EdictReference -> Quake ()
spTargetEarthquake _ = io (putStrLn "GameTarget.spTargetEarthquake") >> undefined -- TODO

useTargetExplosion :: EntUse
useTargetExplosion =
  GenericEntUse "use_target_explosion" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetExplosion") >> undefined -- TODO
