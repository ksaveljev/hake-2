module QuakeRef where

import           Control.Lens         (use, ix, (^.), (%=), (.=))
import           Control.Monad        (void)
import           Control.Monad.ST     (runST)
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as MV

import           Client.ClientStateT
import           Client.RefDefT
import qualified Constants
import           Game.GameLocalsT
import           QuakeState
import           Server.ServerStaticT
import           Types

class QuakeRef a where
    readRef :: Ref a -> Quake a
    modifyRef :: Ref a -> (a -> a) -> Quake ()
    writeRef :: Ref a -> a -> Quake ()

instance QuakeRef EdictT where
    readRef (Ref idx) = do
        edicts <- use (gameBaseGlobals.gbGEdicts)
        io (MV.read edicts idx)
    modifyRef (Ref idx) f = do
        edicts <- use (gameBaseGlobals.gbGEdicts)
        io (MV.modify edicts f idx)
    writeRef (Ref idx) item = do
        edicts <- use (gameBaseGlobals.gbGEdicts)
        io (MV.write edicts idx item)

instance QuakeRef CPlaneT where
    readRef (Ref idx) = do
        planes <- use (cmGlobals.cmMapPlanes)
        io (MV.read planes idx)
    modifyRef (Ref idx) f = do
        planes <- use (cmGlobals.cmMapPlanes)
        io (MV.modify planes f idx)
    writeRef (Ref idx) item = do
        planes <- use (cmGlobals.cmMapPlanes)
        io (MV.write planes idx item)

instance QuakeRef CBrushT where
    readRef (Ref idx) = do
        brushes <- use (cmGlobals.cmMapBrushes)
        io (MV.read brushes idx)
    modifyRef (Ref idx) f = do
        brushes <- use (cmGlobals.cmMapBrushes)
        io (MV.modify brushes f idx)
    writeRef (Ref idx) item = do
        brushes <- use (cmGlobals.cmMapBrushes)
        io (MV.write brushes idx item)
