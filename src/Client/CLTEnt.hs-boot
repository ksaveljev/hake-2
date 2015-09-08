module Client.CLTEnt where

import Data.IORef (IORef)
import Linear (V3)

import Quake
import QuakeState

clearTEnts :: Quake ()

registerTEntSounds :: Quake ()

registerTEntModels :: Quake ()

addTEnts :: Quake ()

addBeams :: Quake ()

addPlayerBeams :: Quake ()

addExplosions :: Quake ()

addLasers :: Quake ()

processSustain :: Quake ()

parseTEnt :: Quake ()

allocExplosion :: Quake (IORef ExplosionT)

smokeAndFlash :: V3 Float -> Quake ()

parseLaser :: Int -> Quake ()
