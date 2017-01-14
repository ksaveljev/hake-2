{-# LANGUAGE FlexibleInstances #-}
module QuakeRef where

import           Control.Lens         (use, ix, (^.), (%=), (.=), (&), (.~), (%~))
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
import           Render.MSurfaceT
import           Server.ServerStaticT
import           Types

class QuakeRef b where
    readRef :: Ref a b -> Quake b
    modifyRef :: Ref a b -> (b -> b) -> Quake ()
    writeRef :: Ref a b -> b -> Quake ()

instance QuakeRef EdictT where
    readRef (Ref idx) = request $ do
        edicts <- use gbGEdicts
        io (MV.read edicts idx)
    modifyRef (Ref idx) f = request $ do
        edicts <- use gbGEdicts
        io (MV.modify edicts f idx)
    writeRef (Ref idx) edict = request $ do
        edicts <- use gbGEdicts
        io (MV.write edicts idx edict)

instance QuakeRef ClientT where
    readRef (Ref idx) = do
        clients <- use (svGlobals.svServerStatic.ssClients)
        return (clients V.! idx)
    modifyRef (Ref idx) f =
        svGlobals.svServerStatic.ssClients.ix idx %= f
    writeRef (Ref idx) client =
        svGlobals.svServerStatic.ssClients.ix idx .= client
  
instance QuakeRef GClientT where
    readRef (Ref idx) = do
        gClients <- use (gameBaseGlobals.gbGame.glClients)
        return (gClients V.! idx)
    modifyRef (Ref idx) f =
        gameBaseGlobals.gbGame.glClients.ix idx %= f
    writeRef (Ref idx) gClient =
        gameBaseGlobals.gbGame.glClients.ix idx .= gClient
  
instance QuakeRef CModelT where
    readRef (Ref idx) = do
        cModels <- use (cmGlobals.cmMapCModels)
        return (cModels V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapCModels.ix idx %= f
    writeRef (Ref idx) cModel =
        cmGlobals.cmMapCModels.ix idx .= cModel
  
instance QuakeRef LinkT where
    readRef (Ref idx) = do
        links <- use (svGlobals.svLinks)
        return (links V.! idx)
    modifyRef (Ref idx) f =
        svGlobals.svLinks.ix idx %= f
    writeRef (Ref idx) link =
        svGlobals.svLinks.ix idx .= link

instance QuakeRef GItemT where
    readRef (Ref idx) = do
        items <- use (gameBaseGlobals.gbItemList)
        return (items V.! idx)
    modifyRef (Ref idx) f =
        gameBaseGlobals.gbItemList.ix idx %= f
    writeRef (Ref idx) item =
        gameBaseGlobals.gbItemList.ix idx .= item

instance QuakeRef MenuFrameworkS where
    readRef (Ref idx) = do
        menuItems <- use (menuGlobals.mgMenuFrameworks)
        return (menuItems V.! idx)
    modifyRef (Ref idx) f =
        menuGlobals.mgMenuFrameworks.ix idx %= f
    writeRef (Ref idx) item =
        menuGlobals.mgMenuFrameworks.ix idx .= item

instance QuakeRef MenuListS where
    readRef (Ref idx) = do
        menuItems <- use (menuGlobals.mgMenuListSItems)
        return (menuItems V.! idx)
    modifyRef (Ref idx) f =
        menuGlobals.mgMenuListSItems.ix idx %= f
    writeRef (Ref idx) item =
        menuGlobals.mgMenuListSItems.ix idx .= item

instance QuakeRef MenuSliderS where
    readRef (Ref idx) = do
        menuItems <- use (menuGlobals.mgMenuSliderSItems)
        return (menuItems V.! idx)
    modifyRef (Ref idx) f =
        menuGlobals.mgMenuSliderSItems.ix idx %= f
    writeRef (Ref idx) item =
        menuGlobals.mgMenuSliderSItems.ix idx .= item

instance QuakeRef MenuActionS where
    readRef (Ref idx) = do
        menuItems <- use (menuGlobals.mgMenuActionSItems)
        return (menuItems V.! idx)
    modifyRef (Ref idx) f =
        menuGlobals.mgMenuActionSItems.ix idx %= f
    writeRef (Ref idx) item =
        menuGlobals.mgMenuActionSItems.ix idx .= item

instance QuakeRef MenuFieldS where
    readRef (Ref idx) = do
        menuItems <- use (menuGlobals.mgMenuFieldSItems)
        return (menuItems V.! idx)
    modifyRef (Ref idx) f =
        menuGlobals.mgMenuFieldSItems.ix idx %= f
    writeRef (Ref idx) item =
        menuGlobals.mgMenuFieldSItems.ix idx .= item

instance QuakeRef MenuSeparatorS where
    readRef (Ref idx) = do
        menuItems <- use (menuGlobals.mgMenuSeparatorSItems)
        return (menuItems V.! idx)
    modifyRef (Ref idx) f =
        menuGlobals.mgMenuSeparatorSItems.ix idx %= f
    writeRef (Ref idx) item =
        menuGlobals.mgMenuSeparatorSItems.ix idx .= item

instance QuakeRef ImageT where
    readRef (Ref idx) = do
        images <- use (fastRenderAPIGlobals.frGLTextures)
        return (images V.! idx)
    modifyRef (Ref idx) f = do
        images <- use (fastRenderAPIGlobals.frGLTextures)
        seq (runST $ do
            images' <- V.unsafeThaw images
            MV.modify images' f idx
            void (V.unsafeFreeze images')) (return ())
    writeRef (Ref idx) item = do
        images <- use (fastRenderAPIGlobals.frGLTextures)
        seq (runST $ do
            images' <- V.unsafeThaw images
            MV.write images' idx item
            void (V.unsafeFreeze images')) (return ())

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

instance QuakeRef AreaNodeT where
    readRef (Ref idx) = do
        areaNodes <- use (svGlobals.svAreaNodes)
        return (areaNodes V.! idx)
    modifyRef (Ref idx) f =
        svGlobals.svAreaNodes.ix idx %= f
    writeRef (Ref idx) item =
        svGlobals.svAreaNodes.ix idx .= item

instance QuakeRef CLeafT where
    readRef (Ref idx) = do
        leafs <- use (cmGlobals.cmMapLeafs)
        return (leafs V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapLeafs.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapLeafs.ix idx .= item

instance QuakeRef UserCmdT where
    readRef (Ref idx) = do
        cmds <- use (globals.gCl.csCmds)
        return (cmds V.! idx)
    modifyRef (Ref idx) f =
        globals.gCl.csCmds.ix idx %= f
    writeRef (Ref idx) item =
        globals.gCl.csCmds.ix idx .= item

instance QuakeRef CBrushT where
    readRef (Ref idx) = do
        brushes <- use (cmGlobals.cmMapBrushes)
        return (brushes V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapBrushes.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapBrushes.ix idx .= item

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

instance QuakeRef CPlaneT where
    readRef (Ref idx) = do
        planes <- use (cmGlobals.cmMapPlanes)
        return (planes V.! idx)
    modifyRef (Ref idx) f =
        cmGlobals.cmMapPlanes.ix idx %= f
    writeRef (Ref idx) item =
        cmGlobals.cmMapPlanes.ix idx .= item

instance QuakeRef ModelT where
    readRef (Ref idx)
        | idx < Constants.maxModKnown = do
            models <- use (fastRenderAPIGlobals.frModKnown)
            return (models V.! idx)
        | otherwise = do
            models <- use (fastRenderAPIGlobals.frModInline)
            return (models V.! (idx - Constants.maxModKnown))
    modifyRef (Ref idx) f
        | idx < Constants.maxModKnown = fastRenderAPIGlobals.frModKnown.ix idx %= f
        | otherwise = fastRenderAPIGlobals.frModInline.ix (idx - Constants.maxModKnown) %= f
    writeRef (Ref idx) item
        | idx < Constants.maxModKnown = fastRenderAPIGlobals.frModKnown.ix idx .= item
        | otherwise = fastRenderAPIGlobals.frModInline.ix (idx - Constants.maxModKnown) .= item

instance QuakeRef MSurfaceT where
    readRef (Ref idx) = do
        loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
        model <- readRef loadModelRef
        return ((model^.mSurfaces) V.! idx)
    modifyRef (Ref idx) f = do
        loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
        model <- readRef loadModelRef
        seq (runST $ do
            surfaces <- V.unsafeThaw (model^.mSurfaces)
            MV.modify surfaces f idx
            void (V.unsafeFreeze surfaces)) (return ())
    writeRef (Ref idx) item = do
        loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
        model <- readRef loadModelRef
        seq (runST $ do
            surfaces <- V.unsafeThaw (model^.mSurfaces)
            MV.write surfaces idx item
            void (V.unsafeFreeze surfaces)) (return ())

instance QuakeRef MTexInfoT where
    readRef (Ref idx) = do
        loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
        model <- readRef loadModelRef
        return ((model^.mTexInfo) V.! idx)
    modifyRef (Ref idx) f = do
        loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
        model <- readRef loadModelRef
        seq (runST $ do
            texInfo <- V.unsafeThaw (model^.mTexInfo)
            MV.modify texInfo f idx
            void (V.unsafeFreeze texInfo)) (return ())
    writeRef (Ref idx) item = do
        loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
        model <- readRef loadModelRef
        seq (runST $ do
            texInfo <- V.unsafeThaw (model^.mTexInfo)
            MV.write texInfo idx item
            void (V.unsafeFreeze texInfo)) (return ())
       
instance QuakeRef CDLightT where
  readRef (Ref idx) = request $ do
      dLights <- use cgDLights
      io (MV.read dLights idx)
  modifyRef (Ref idx) f = request $ do
      dLights <- use cgDLights
      io (MV.modify dLights f idx)
  writeRef (Ref idx) dLight = request $ do
      dLights <- use cgDLights
      io (MV.write dLights idx dLight)

instance QuakeRef GLPolyT where
    readRef (Ref idx) = request $ do
        polygonCache <- use frPolygonCache
        io (MV.read polygonCache idx)
    modifyRef (Ref idx) f = request $ do
        polygonCache <- use frPolygonCache
        io (MV.modify polygonCache f idx)
    writeRef (Ref idx) poly = request $ do
        polygonCache <- use frPolygonCache
        io (MV.write polygonCache idx poly)

instance QuakeRef FrameT where
    readRef (Ref idx) = do
        frames <- use (globals.gCl.csFrames)
        return (frames V.! idx)
    modifyRef (Ref idx) f =
        globals.gCl.csFrames.ix idx %= f
    writeRef (Ref idx) item =
        globals.gCl.csFrames.ix idx .= item

instance QuakeRef CParticleT where
    readRef (Ref idx) = request $ do
        particles <- use cgParticles
        io (MV.read particles idx)
    modifyRef (Ref idx) f = request $ do
        particles <- use cgParticles
        io (MV.modify particles f idx)
    writeRef (Ref idx) poly = request $ do
        particles <- use cgParticles
        io (MV.write particles idx poly)

instance QuakeRef CEntityT where
    readRef (Ref idx) = do
        entities <- use (globals.gClEntities)
        return (entities V.! idx)
    modifyRef (Ref idx) f =
        globals.gClEntities.ix idx %= f
    writeRef (Ref idx) item =
        globals.gClEntities.ix idx .= item

instance QuakeRef LaserT where
    readRef (Ref idx) = do
        lasers <- use (clTEntGlobals.clteLasers)
        return (lasers V.! idx)
    modifyRef (Ref idx) f =
        clTEntGlobals.clteLasers.ix idx %= f
    writeRef (Ref idx) item =
        clTEntGlobals.clteLasers.ix idx .= item

instance QuakeRef CLSustainT where
    readRef (Ref idx) = do
        sustains <- use (clTEntGlobals.clteSustains)
        return (sustains V.! idx)
    modifyRef (Ref idx) f =
        clTEntGlobals.clteSustains.ix idx %= f
    writeRef (Ref idx) item =
        clTEntGlobals.clteSustains.ix idx .= item

instance QuakeRef ExplosionT where
    readRef (Ref idx) = do
        explosions <- use (clTEntGlobals.clteExplosions)
        return (explosions V.! idx)
    modifyRef (Ref idx) f =
        clTEntGlobals.clteExplosions.ix idx %= f
    writeRef (Ref idx) item =
        clTEntGlobals.clteExplosions.ix idx .= item

instance QuakeRef BeamT where
    readRef (Ref idx) = do
        beams <- use (clTEntGlobals.clteBeams)
        return (beams V.! idx)
    modifyRef (Ref idx) f =
        clTEntGlobals.clteBeams.ix idx %= f
    writeRef (Ref idx) item =
        clTEntGlobals.clteBeams.ix idx .= item

instance QuakeRef CLightStyleT where
    readRef (Ref idx) = do
        lightStyles <- use (clientGlobals.cgLightStyle)
        return (lightStyles V.! idx)
    modifyRef (Ref idx) f =
        clientGlobals.cgLightStyle.ix idx %= f
    writeRef (Ref idx) item =
        clientGlobals.cgLightStyle.ix idx .= item

-- TODO: make sure EntityT is not used with vgEntities
instance QuakeRef EntityT where
    readRef (Ref idx) = do
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        return ((newRefDef^.rdEntities) V.! idx)
    modifyRef (Ref idx) f =
        fastRenderAPIGlobals.frNewRefDef.rdEntities.ix idx %= f
    writeRef (Ref idx) item =
        fastRenderAPIGlobals.frNewRefDef.rdEntities.ix idx .= item