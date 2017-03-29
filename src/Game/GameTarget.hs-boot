module Game.GameTarget where

import Types
import QuakeState
import Game.Adapters

spTargetTempEntity :: EdictReference -> Quake ()

spTargetSpeaker :: EdictReference -> Quake ()

spTargetHelp :: EdictReference -> Quake ()

spTargetSecret :: EdictReference -> Quake ()

spTargetGoal :: EdictReference -> Quake ()

spTargetExplosion :: EdictReference -> Quake ()

spTargetChangeLevel :: EdictReference -> Quake ()

spTargetSplash :: EdictReference -> Quake ()

spTargetSpawner :: EdictReference -> Quake ()

spTargetBlaster :: EdictReference -> Quake ()

spTargetCrossLevelTrigger :: EdictReference -> Quake ()

spTargetCrossLevelTarget :: EdictReference -> Quake ()

spTargetLaser :: EdictReference -> Quake ()

spTargetLightRamp :: EdictReference -> Quake ()

spTargetEarthquake :: EdictReference -> Quake ()

useTargetExplosion :: EntUse

useTargetGoal :: EntUse

useTargetSpeaker :: EntUse

useTargetSecret :: EntUse

useTargetHelp :: EntUse

useTargetSplash :: EntUse

useTargetChangeLevel :: EntUse

targetExplosionExplode :: EntThink

useTargetTEnt :: EntUse

useTargetSpawner :: EntUse
