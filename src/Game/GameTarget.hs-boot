module Game.GameTarget where

import Types
import QuakeState
import Game.Adapters

spTargetTempEntity :: Ref EdictT -> Quake ()

spTargetSpeaker :: Ref EdictT -> Quake ()

spTargetHelp :: Ref EdictT -> Quake ()

spTargetSecret :: Ref EdictT -> Quake ()

spTargetGoal :: Ref EdictT -> Quake ()

spTargetExplosion :: Ref EdictT -> Quake ()

spTargetChangeLevel :: Ref EdictT -> Quake ()

spTargetSplash :: Ref EdictT -> Quake ()

spTargetSpawner :: Ref EdictT -> Quake ()

spTargetBlaster :: Ref EdictT -> Quake ()

spTargetCrossLevelTrigger :: Ref EdictT -> Quake ()

spTargetCrossLevelTarget :: Ref EdictT -> Quake ()

spTargetLaser :: Ref EdictT -> Quake ()

spTargetLightRamp :: Ref EdictT -> Quake ()

spTargetEarthquake :: Ref EdictT -> Quake ()

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
