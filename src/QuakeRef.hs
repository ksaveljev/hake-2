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

instance QuakeRef CLeafT where
    readRef (Ref idx) = do
        leafs <- use (cmGlobals.cmMapLeafs)
        return (leafs V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapLeafs.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapLeafs.ix idx .= item

instance QuakeRef CBrushSideT where
    readRef (Ref idx) = do
        brushSides <- use (cmGlobals.cmMapBrushSides)
        return (brushSides V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapBrushSides.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapBrushSides.ix idx .= item

instance QuakeRef CNodeT where
    readRef (Ref idx) = do
        nodes <- use (cmGlobals.cmMapNodes)
        return (nodes V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapNodes.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapNodes.ix idx .= item

instance QuakeRef CAreaT where
    readRef (Ref idx) = do
        areas <- use (cmGlobals.cmMapAreas)
        return (areas V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapAreas.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapAreas.ix idx .= item

instance QuakeRef DAreaPortalT where
    readRef (Ref idx) = do
        areaPortals <- use (cmGlobals.cmMapAreaPortals)
        return (areaPortals V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapAreaPortals.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapAreaPortals.ix idx .= item

