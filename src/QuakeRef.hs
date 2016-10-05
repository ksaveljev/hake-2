{-# LANGUAGE FlexibleInstances #-}
module QuakeRef where

import           Control.Lens         (use, ix, (^.), (%=), (.=), (&), (.~), (%~))
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as MV

import           Client.ClientStateT
import qualified Constants
import           Game.GameLocalsT
import           QuakeIOState
import           QuakeState
import           Render.ModelT
import           Server.ServerStaticT
import           Types

class QuakeRef a where
    readRef :: Ref a -> Quake a
    modifyRef :: Ref a -> (a -> a) -> Quake ()
    writeRef :: Ref a -> a -> Quake ()

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
    modifyRef (Ref idx) f =
        fastRenderAPIGlobals.frGLTextures.ix idx %= f
    writeRef (Ref idx) item =
        fastRenderAPIGlobals.frGLTextures.ix idx .= item

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
        modifyRef loadModelRef (\v -> v & mSurfaces.ix idx %~ f)
    writeRef (Ref idx) item = do
        loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
        modifyRef loadModelRef (\v -> v & mSurfaces.ix idx .~ item)

instance QuakeRef MTexInfoT where
  readRef (Ref idx) = do
      loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
      model <- readRef loadModelRef
      return ((model^.mTexInfo) V.! idx)
  modifyRef (Ref idx) f = do
      loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
      modifyRef loadModelRef (\v -> v & mTexInfo.ix idx %~ f)
  writeRef (Ref idx) item = do
      loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
      modifyRef loadModelRef (\v -> v & mTexInfo.ix idx .~ item)
       
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