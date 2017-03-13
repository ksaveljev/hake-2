{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           QuakeIOState
import           QuakeState
import           Render.ModelT
import           Server.ServerStaticT
import           Types

class QuakeRef a b where
    readRef :: Ref a b -> Quake b
    modifyRef :: Ref a b -> (b -> b) -> Quake ()
    writeRef :: Ref a b -> b -> Quake ()

instance QuakeRef a EdictT where
    readRef (Ref _ idx) = request $ do
        edicts <- use gbGEdicts
        io (MV.read edicts idx)
    modifyRef (Ref _ idx) f = request $ do
        edicts <- use gbGEdicts
        io (MV.modify edicts f idx)
    writeRef (Ref _ idx) edict = request $ do
        edicts <- use gbGEdicts
        io (MV.write edicts idx edict)

instance QuakeRef a ClientT where
    readRef (Ref _ idx) = do
        clients <- use (svGlobals.svServerStatic.ssClients)
        return (clients V.! idx)
    modifyRef (Ref _ idx) f =
        svGlobals.svServerStatic.ssClients.ix idx %= f
    writeRef (Ref _ idx) client =
        svGlobals.svServerStatic.ssClients.ix idx .= client
  
instance QuakeRef a GClientT where
    readRef (Ref _ idx) = do
        gClients <- use (gameBaseGlobals.gbGame.glClients)
        return (gClients V.! idx)
    modifyRef (Ref _ idx) f =
        gameBaseGlobals.gbGame.glClients.ix idx %= f
    writeRef (Ref _ idx) gClient =
        gameBaseGlobals.gbGame.glClients.ix idx .= gClient
  
instance QuakeRef a CModelT where
    readRef (Ref _ idx) = do
        cModels <- use (cmGlobals.cmMapCModels)
        return (cModels V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapCModels.ix idx %= f
    writeRef (Ref _ idx) cModel =
        cmGlobals.cmMapCModels.ix idx .= cModel
  
instance QuakeRef a LinkT where
    readRef (Ref _ idx) = do
        links <- use (svGlobals.svLinks)
        return (links V.! idx)
    modifyRef (Ref _ idx) f =
        svGlobals.svLinks.ix idx %= f
    writeRef (Ref _ idx) link =
        svGlobals.svLinks.ix idx .= link

instance QuakeRef a GItemT where
    readRef (Ref _ idx) = do
        items <- use (gameBaseGlobals.gbItemList)
        return (items V.! idx)
    modifyRef (Ref _ idx) f =
        gameBaseGlobals.gbItemList.ix idx %= f
    writeRef (Ref _ idx) item =
        gameBaseGlobals.gbItemList.ix idx .= item

instance QuakeRef a MenuFrameworkS where
    readRef (Ref _ idx) = do
        menuItems <- use (menuGlobals.mgMenuFrameworks)
        return (menuItems V.! idx)
    modifyRef (Ref _ idx) f =
        menuGlobals.mgMenuFrameworks.ix idx %= f
    writeRef (Ref _ idx) item =
        menuGlobals.mgMenuFrameworks.ix idx .= item

instance QuakeRef a MenuListS where
    readRef (Ref _ idx) = do
        menuItems <- use (menuGlobals.mgMenuListSItems)
        return (menuItems V.! idx)
    modifyRef (Ref _ idx) f =
        menuGlobals.mgMenuListSItems.ix idx %= f
    writeRef (Ref _ idx) item =
        menuGlobals.mgMenuListSItems.ix idx .= item

instance QuakeRef a MenuSliderS where
    readRef (Ref _ idx) = do
        menuItems <- use (menuGlobals.mgMenuSliderSItems)
        return (menuItems V.! idx)
    modifyRef (Ref _ idx) f =
        menuGlobals.mgMenuSliderSItems.ix idx %= f
    writeRef (Ref _ idx) item =
        menuGlobals.mgMenuSliderSItems.ix idx .= item

instance QuakeRef a MenuActionS where
    readRef (Ref _ idx) = do
        menuItems <- use (menuGlobals.mgMenuActionSItems)
        return (menuItems V.! idx)
    modifyRef (Ref _ idx) f =
        menuGlobals.mgMenuActionSItems.ix idx %= f
    writeRef (Ref _ idx) item =
        menuGlobals.mgMenuActionSItems.ix idx .= item

instance QuakeRef a MenuFieldS where
    readRef (Ref _ idx) = do
        menuItems <- use (menuGlobals.mgMenuFieldSItems)
        return (menuItems V.! idx)
    modifyRef (Ref _ idx) f =
        menuGlobals.mgMenuFieldSItems.ix idx %= f
    writeRef (Ref _ idx) item =
        menuGlobals.mgMenuFieldSItems.ix idx .= item

instance QuakeRef a MenuSeparatorS where
    readRef (Ref _ idx) = do
        menuItems <- use (menuGlobals.mgMenuSeparatorSItems)
        return (menuItems V.! idx)
    modifyRef (Ref _ idx) f =
        menuGlobals.mgMenuSeparatorSItems.ix idx %= f
    writeRef (Ref _ idx) item =
        menuGlobals.mgMenuSeparatorSItems.ix idx .= item

instance QuakeRef a ImageT where
    readRef (Ref _ idx) = do
        images <- use (fastRenderAPIGlobals.frGLTextures)
        return (images V.! idx)
    modifyRef (Ref _ idx) f = do
        images <- use (fastRenderAPIGlobals.frGLTextures)
        seq (runST $ do
            images' <- V.unsafeThaw images
            MV.modify images' f idx
            void (V.unsafeFreeze images')) (return ())
    writeRef (Ref _ idx) item = do
        images <- use (fastRenderAPIGlobals.frGLTextures)
        seq (runST $ do
            images' <- V.unsafeThaw images
            MV.write images' idx item
            void (V.unsafeFreeze images')) (return ())

instance QuakeRef a CAreaT where
    readRef (Ref _ idx) = do
        areas <- use (cmGlobals.cmMapAreas)
        return (areas V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapAreas.ix idx %= f
    writeRef (Ref _ idx) item =
        cmGlobals.cmMapAreas.ix idx .= item

instance QuakeRef a DAreaPortalT where
    readRef (Ref _ idx) = do
        areaPortals <- use (cmGlobals.cmMapAreaPortals)
        return (areaPortals V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapAreaPortals.ix idx %= f
    writeRef (Ref _ idx) item =
        cmGlobals.cmMapAreaPortals.ix idx .= item

instance QuakeRef a AreaNodeT where
    readRef (Ref _ idx) = do
        areaNodes <- use (svGlobals.svAreaNodes)
        return (areaNodes V.! idx)
    modifyRef (Ref _ idx) f =
        svGlobals.svAreaNodes.ix idx %= f
    writeRef (Ref _ idx) item =
        svGlobals.svAreaNodes.ix idx .= item

instance QuakeRef a CLeafT where
    readRef (Ref _ idx) = do
        leafs <- use (cmGlobals.cmMapLeafs)
        return (leafs V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapLeafs.ix idx %= f
    writeRef (Ref _ idx) item =
        cmGlobals.cmMapLeafs.ix idx .= item

instance QuakeRef a UserCmdT where
    readRef (Ref _ idx) = do
        cmds <- use (globals.gCl.csCmds)
        return (cmds V.! idx)
    modifyRef (Ref _ idx) f =
        globals.gCl.csCmds.ix idx %= f
    writeRef (Ref _ idx) item =
        globals.gCl.csCmds.ix idx .= item

instance QuakeRef a CBrushT where
    readRef (Ref _ idx) = do
        brushes <- use (cmGlobals.cmMapBrushes)
        return (brushes V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapBrushes.ix idx %= f
    writeRef (Ref _ idx) item =
        cmGlobals.cmMapBrushes.ix idx .= item

instance QuakeRef a CBrushSideT where
    readRef (Ref _ idx) = do
        brushSides <- use (cmGlobals.cmMapBrushSides)
        return (brushSides V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapBrushSides.ix idx %= f
    writeRef (Ref _ idx) item =
        cmGlobals.cmMapBrushSides.ix idx .= item

instance QuakeRef a CNodeT where
    readRef (Ref _ idx) = do
        nodes <- use (cmGlobals.cmMapNodes)
        return (nodes V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapNodes.ix idx %= f
    writeRef (Ref _ idx) item =
        cmGlobals.cmMapNodes.ix idx .= item

instance QuakeRef a CPlaneT where
    readRef (Ref _ idx) = do
        planes <- use (cmGlobals.cmMapPlanes)
        return (planes V.! idx)
    modifyRef (Ref _ idx) f =
        cmGlobals.cmMapPlanes.ix idx %= f
    writeRef (Ref _ idx) item =
        cmGlobals.cmMapPlanes.ix idx .= item

instance QuakeRef a ModelT where
    readRef (Ref _ idx)
        | idx < Constants.maxModKnown = do
            models <- use (fastRenderAPIGlobals.frModKnown)
            return (models V.! idx)
        | otherwise = do
            models <- use (fastRenderAPIGlobals.frModInline)
            return (models V.! (idx - Constants.maxModKnown))
    modifyRef (Ref _ idx) f
        | idx < Constants.maxModKnown = fastRenderAPIGlobals.frModKnown.ix idx %= f
        | otherwise = fastRenderAPIGlobals.frModInline.ix (idx - Constants.maxModKnown) %= f
    writeRef (Ref _ idx) item
        | idx < Constants.maxModKnown = fastRenderAPIGlobals.frModKnown.ix idx .= item
        | otherwise = fastRenderAPIGlobals.frModInline.ix (idx - Constants.maxModKnown) .= item

instance QuakeRef a MSurfaceT where
    readRef (Ref modelIdx idx) = do
        model <- readRef (Ref Constants.noParent modelIdx)
        return ((model^.mSurfaces) V.! idx)
    modifyRef (Ref modelIdx idx) f = do
        model <- readRef (Ref Constants.noParent modelIdx)
        seq (runST $ do
            surfaces <- V.unsafeThaw (model^.mSurfaces)
            MV.modify surfaces f idx
            void (V.unsafeFreeze surfaces)) (return ())
    writeRef (Ref modelIdx idx) item = do
        model <- readRef (Ref Constants.noParent modelIdx)
        seq (runST $ do
            surfaces <- V.unsafeThaw (model^.mSurfaces)
            MV.write surfaces idx item
            void (V.unsafeFreeze surfaces)) (return ())

instance QuakeRef a MTexInfoT where
    readRef (Ref modelIdx idx) = do
        model <- readRef (Ref Constants.noParent modelIdx)
        return ((model^.mTexInfo) V.! idx)
    modifyRef (Ref modelIdx idx) f = do
        model <- readRef (Ref Constants.noParent modelIdx)
        seq (runST $ do
            texInfo <- V.unsafeThaw (model^.mTexInfo)
            MV.modify texInfo f idx
            void (V.unsafeFreeze texInfo)) (return ())
    writeRef (Ref modelIdx idx) item = do
        model <- readRef (Ref Constants.noParent modelIdx)
        seq (runST $ do
            texInfo <- V.unsafeThaw (model^.mTexInfo)
            MV.write texInfo idx item
            void (V.unsafeFreeze texInfo)) (return ())
       
instance QuakeRef a CDLightT where
  readRef (Ref _ idx) = request $ do
      dLights <- use cgDLights
      io (MV.read dLights idx)
  modifyRef (Ref _ idx) f = request $ do
      dLights <- use cgDLights
      io (MV.modify dLights f idx)
  writeRef (Ref _ idx) dLight = request $ do
      dLights <- use cgDLights
      io (MV.write dLights idx dLight)

instance QuakeRef a GLPolyT where
    readRef (Ref _ idx) = request $ do
        polygonCache <- use frPolygonCache
        io (MV.read polygonCache idx)
    modifyRef (Ref _ idx) f = request $ do
        polygonCache <- use frPolygonCache
        io (MV.modify polygonCache f idx)
    writeRef (Ref _ idx) poly = request $ do
        polygonCache <- use frPolygonCache
        io (MV.write polygonCache idx poly)

instance QuakeRef a FrameT where
    readRef (Ref _ idx) = do
        frames <- use (globals.gCl.csFrames)
        return (frames V.! idx)
    modifyRef (Ref _ idx) f =
        globals.gCl.csFrames.ix idx %= f
    writeRef (Ref _ idx) item =
        globals.gCl.csFrames.ix idx .= item

instance QuakeRef a CParticleT where
    readRef (Ref _ idx) = request $ do
        particles <- use cgParticles
        io (MV.read particles idx)
    modifyRef (Ref _ idx) f = request $ do
        particles <- use cgParticles
        io (MV.modify particles f idx)
    writeRef (Ref _ idx) poly = request $ do
        particles <- use cgParticles
        io (MV.write particles idx poly)

instance QuakeRef a CEntityT where
    readRef (Ref _ idx) = do
        entities <- use (globals.gClEntities)
        return (entities V.! idx)
    modifyRef (Ref _ idx) f =
        globals.gClEntities.ix idx %= f
    writeRef (Ref _ idx) item =
        globals.gClEntities.ix idx .= item

instance QuakeRef a LaserT where
    readRef (Ref _ idx) = do
        lasers <- use (clTEntGlobals.clteLasers)
        return (lasers V.! idx)
    modifyRef (Ref _ idx) f =
        clTEntGlobals.clteLasers.ix idx %= f
    writeRef (Ref _ idx) item =
        clTEntGlobals.clteLasers.ix idx .= item

instance QuakeRef a CLSustainT where
    readRef (Ref _ idx) = do
        sustains <- use (clTEntGlobals.clteSustains)
        return (sustains V.! idx)
    modifyRef (Ref _ idx) f =
        clTEntGlobals.clteSustains.ix idx %= f
    writeRef (Ref _ idx) item =
        clTEntGlobals.clteSustains.ix idx .= item

instance QuakeRef a ExplosionT where
    readRef (Ref _ idx) = do
        explosions <- use (clTEntGlobals.clteExplosions)
        return (explosions V.! idx)
    modifyRef (Ref _ idx) f =
        clTEntGlobals.clteExplosions.ix idx %= f
    writeRef (Ref _ idx) item =
        clTEntGlobals.clteExplosions.ix idx .= item

instance QuakeRef a BeamT where
    readRef (Ref _ idx) = do
        beams <- use (clTEntGlobals.clteBeams)
        return (beams V.! idx)
    modifyRef (Ref _ idx) f =
        clTEntGlobals.clteBeams.ix idx %= f
    writeRef (Ref _ idx) item =
        clTEntGlobals.clteBeams.ix idx .= item

instance QuakeRef a CLightStyleT where
    readRef (Ref _ idx) = do
        lightStyles <- use (clientGlobals.cgLightStyle)
        return (lightStyles V.! idx)
    modifyRef (Ref _ idx) f =
        clientGlobals.cgLightStyle.ix idx %= f
    writeRef (Ref _ idx) item =
        clientGlobals.cgLightStyle.ix idx .= item

instance QuakeRef VGlobals EntityT where
    readRef (Ref _ idx) = do
        entities <- use (vGlobals.vgEntities)
        return (entities V.! idx)
    modifyRef (Ref _ idx) f =
        vGlobals.vgEntities.ix idx %= f
    writeRef (Ref _ idx) item =
        vGlobals.vgEntities.ix idx .= item

instance QuakeRef RefDefT EntityT where
    readRef (Ref _ idx) = do
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        return ((newRefDef^.rdEntities) V.! idx)
    modifyRef (Ref _ idx) f =
        fastRenderAPIGlobals.frNewRefDef.rdEntities.ix idx %= f
    writeRef (Ref _ idx) item =
        fastRenderAPIGlobals.frNewRefDef.rdEntities.ix idx .= item

instance QuakeRef VGlobals LightStyleT where
    readRef (Ref _ idx) = do
        lightStyles <- use (vGlobals.vgLightStyles)
        return (lightStyles V.! idx)
    modifyRef (Ref _ idx) f =
        vGlobals.vgLightStyles.ix idx %= f
    writeRef (Ref _ idx) item =
        vGlobals.vgLightStyles.ix idx .= item

-- IMPROVE: where else could it be put if not here?
readCurrentEntity :: CurrentEntity -> Quake (EntityT)
readCurrentEntity (VEntityRef entityRef) = readRef entityRef
readCurrentEntity (RDEntityRef entityRef) = readRef entityRef
readCurrentEntity (NewEntity entity) = return entity

modifyCurrentEntity :: CurrentEntity -> (EntityT -> EntityT) -> Quake ()
modifyCurrentEntity (VEntityRef entityRef) f = modifyRef entityRef f
modifyCurrentEntity (RDEntityRef entityRef) f = modifyRef entityRef f
modifyCurrentEntity (NewEntity entity) f = fastRenderAPIGlobals.frCurrentEntity .= Just (NewEntity (f entity))

writeCurrentEntity :: CurrentEntity -> EntityT -> Quake ()
writeCurrentEntity (VEntityRef entityRef) item = writeRef entityRef item
writeCurrentEntity (RDEntityRef entityRef) item = writeRef entityRef item
writeCurrentEntity (NewEntity _) item = fastRenderAPIGlobals.frCurrentEntity .= Just (NewEntity item)