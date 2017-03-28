module Client.CLTEntShared
    ( allocExplosion
    , exFree
    , exExplosion
    , exMisc
    , exFlash
    , exMFlash
    , exPoly
    , exPoly2
    , smokeAndFlash
    ) where

import           Control.Lens        (use, (^.))
import           Data.IORef          (newIORef)
import           Linear              (V3)

import           Client.ClientStateT
import           Client.EntityT
import           Client.ExplosionT
import           Client.FrameT
import qualified Constants
import           QuakeRef
import           QuakeState
import           Types

exFree :: Int
exFree = 0

exExplosion :: Int
exExplosion = 1

exMisc :: Int
exMisc = 2

exFlash :: Int
exFlash = 3

exMFlash :: Int
exMFlash = 4

exPoly :: Int
exPoly = 5

exPoly2 :: Int
exPoly2 = 6

smokeAndFlash :: V3 Float -> Quake ()
smokeAndFlash origin = do
    setFirstExplosion
    setSecondExplosion
  where
    setFirstExplosion = do
        explosionRef <- allocExplosion
        smokeModel <- use (clTEntGlobals.clteModSmoke)
        serverTime <- use (globals.gCl.csFrame.fServerTime)
        entRef <- io $ newIORef $ newEntityT
            { _enFlags = Constants.rfTranslucent
            , _eModel  = smokeModel
            , _eOrigin = origin
            }
        writeRef explosionRef $ newExplosionT
            { _eType       = exMisc
            , _eEnt        = entRef
            , _eFrames     = 4
            , _eStart      = fromIntegral (serverTime  - 100)
            }
    setSecondExplosion = do
        explosionRef <- allocExplosion
        flashModel <- use (clTEntGlobals.clteModFlash)
        serverTime <- use (globals.gCl.csFrame.fServerTime)
        entRef <- io $ newIORef $ newEntityT
            { _enFlags = Constants.rfFullBright
            , _eModel  = flashModel
            , _eOrigin = origin
            }
        writeRef explosionRef $ newExplosionT
            { _eType       = exFlash
            , _eEnt        = entRef
            , _eFrames     = 2
            , _eStart      = fromIntegral (serverTime  - 100)
            }

allocExplosion :: Quake (Ref ExplosionT)
allocExplosion = do
    freeExplosionRef <- findFreeSpot 0 Constants.maxExplosions
    maybe findOldest newExplosion freeExplosionRef
  where
    findFreeSpot idx maxIdx
        | idx >= maxIdx = return Nothing
        | otherwise = do
            explosion <- readRef (Ref idx)
            if (explosion^.eType) == exFree
                then return (Just (Ref idx))
                else findFreeSpot (idx + 1) maxIdx
    newExplosion explosionRef = do
        writeRef explosionRef newExplosionT
        return explosionRef
    findOldest = do
        time <- use (globals.gCl.csTime)
        explosionRef <- findOldestExplosion (fromIntegral time) (Ref 0) 0 Constants.maxExplosions
        writeRef explosionRef newExplosionT
        return explosionRef
    findOldestExplosion time oldestRef idx maxIdx
        | idx >= maxIdx = return oldestRef
        | otherwise = do
            explosion <- readRef (Ref idx)
            if (explosion^.eStart) < time
                then findOldestExplosion (explosion^.eStart) (Ref idx) (idx + 1) maxIdx
                else findOldestExplosion time oldestRef (idx + 1) maxIdx
