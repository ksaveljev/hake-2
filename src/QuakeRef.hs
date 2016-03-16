{-# LANGUAGE FlexibleInstances #-}
module QuakeRef where

import           Game.GameLocalsT
import           QuakeIOState
import           QuakeState
import           Server.ServerStaticT
import           Types

import           Control.Lens (use, ix, (%=), (.=))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

class QuakeRef a where
  readRef :: Ref a -> Quake a
  modifyRef :: Ref a -> (a -> a) -> Quake ()
  writeRef :: Ref a -> a -> Quake ()

instance QuakeRef EdictT where
  readRef (Ref idx) =
    request (do edicts <- use gbGEdicts
                io (MV.read edicts idx))
  modifyRef (Ref idx) f =
    request (do edicts <- use gbGEdicts
                io (MV.modify edicts f idx))
  writeRef (Ref idx) edict =
    request (do edicts <- use gbGEdicts
                io (MV.write edicts idx edict))

instance QuakeRef ClientT where
  readRef (Ref idx) =
    do clients <- use (svGlobals.svServerStatic.ssClients)
       return (clients V.! idx)
  modifyRef (Ref idx) f =
    svGlobals.svServerStatic.ssClients.ix idx %= f
  writeRef (Ref idx) client =
    svGlobals.svServerStatic.ssClients.ix idx .= client
  
instance QuakeRef GClientT where
  readRef (Ref idx) =
    do gClients <- use (gameBaseGlobals.gbGame.glClients)
       return (gClients V.! idx)
  modifyRef (Ref idx) f =
    gameBaseGlobals.gbGame.glClients.ix idx %= f
  writeRef (Ref idx) gClient =
    gameBaseGlobals.gbGame.glClients.ix idx .= gClient
  
instance QuakeRef CModelT where
  readRef (Ref idx) =
    do cModels <- use (cmGlobals.cmMapCModels)
       return (cModels V.! idx)
  modifyRef (Ref idx) f =
    cmGlobals.cmMapCModels.ix idx %= f
  writeRef (Ref idx) cModel =
    cmGlobals.cmMapCModels.ix idx .= cModel
  
instance QuakeRef LinkT where
  readRef (Ref idx) =
    do links <- use (svGlobals.svLinks)
       return (links V.! idx)
  modifyRef (Ref idx) f =
    svGlobals.svLinks.ix idx %= f
  writeRef (Ref idx) link =
    svGlobals.svLinks.ix idx .= link

instance QuakeRef GItemT where
  readRef (Ref idx) =
    do items <- use (gameBaseGlobals.gbItemList)
       return (items V.! idx)
  modifyRef (Ref idx) f =
    gameBaseGlobals.gbItemList.ix idx %= f
  writeRef (Ref idx) item =
    gameBaseGlobals.gbItemList.ix idx .= item

instance QuakeRef MenuFrameworkS where
  readRef (Ref idx) =
    do menuItems <- use (menuGlobals.mgMenuFrameworks)
       return (menuItems V.! idx)
  modifyRef (Ref idx) f =
    menuGlobals.mgMenuFrameworks.ix idx %= f
  writeRef (Ref idx) item =
    menuGlobals.mgMenuFrameworks.ix idx .= item

instance QuakeRef MenuListS where
  readRef (Ref idx) =
    do menuItems <- use (menuGlobals.mgMenuListSItems)
       return (menuItems V.! idx)
  modifyRef (Ref idx) f =
    menuGlobals.mgMenuListSItems.ix idx %= f
  writeRef (Ref idx) item =
    menuGlobals.mgMenuListSItems.ix idx .= item

instance QuakeRef MenuSliderS where
  readRef (Ref idx) =
    do menuItems <- use (menuGlobals.mgMenuSliderSItems)
       return (menuItems V.! idx)
  modifyRef (Ref idx) f =
    menuGlobals.mgMenuSliderSItems.ix idx %= f
  writeRef (Ref idx) item =
    menuGlobals.mgMenuSliderSItems.ix idx .= item

instance QuakeRef MenuActionS where
  readRef (Ref idx) =
    do menuItems <- use (menuGlobals.mgMenuActionSItems)
       return (menuItems V.! idx)
  modifyRef (Ref idx) f =
    menuGlobals.mgMenuActionSItems.ix idx %= f
  writeRef (Ref idx) item =
    menuGlobals.mgMenuActionSItems.ix idx .= item

instance QuakeRef MenuFieldS where
  readRef (Ref idx) =
    do menuItems <- use (menuGlobals.mgMenuFieldSItems)
       return (menuItems V.! idx)
  modifyRef (Ref idx) f =
    menuGlobals.mgMenuFieldSItems.ix idx %= f
  writeRef (Ref idx) item =
    menuGlobals.mgMenuFieldSItems.ix idx .= item

instance QuakeRef MenuSeparatorS where
  readRef (Ref idx) =
    do menuItems <- use (menuGlobals.mgMenuSeparatorSItems)
       return (menuItems V.! idx)
  modifyRef (Ref idx) f =
    menuGlobals.mgMenuSeparatorSItems.ix idx %= f
  writeRef (Ref idx) item =
    menuGlobals.mgMenuSeparatorSItems.ix idx .= item

instance QuakeRef ImageT where
  readRef (Ref idx) =
    request (do images <- use frGLTextures
                io (MV.read images idx))
  modifyRef (Ref idx) f =
    request (do images <- use frGLTextures
                io (MV.modify images f idx))
  writeRef (Ref idx) item =
    request (do images <- use frGLTextures
                io (MV.write images idx item))

instance QuakeRef CAreaT where
  readRef (Ref idx) =
    do areas <- use (cmGlobals.cmMapAreas)
       return (areas V.! idx)
  modifyRef (Ref idx) f =
    cmGlobals.cmMapAreas.ix idx %= f
  writeRef (Ref idx) item =
    cmGlobals.cmMapAreas.ix idx .= item

instance QuakeRef DAreaPortalT where
  readRef (Ref idx) =
    do areaPortals <- use (cmGlobals.cmMapAreaPortals)
       return (areaPortals V.! idx)
  modifyRef (Ref idx) f =
    cmGlobals.cmMapAreaPortals.ix idx %= f
  writeRef (Ref idx) item =
    cmGlobals.cmMapAreaPortals.ix idx .= item

instance QuakeRef AreaNodeT where
  readRef (Ref idx) =
    do areaNodes <- use (svGlobals.svAreaNodes)
       return (areaNodes V.! idx)
  modifyRef (Ref idx) f =
    svGlobals.svAreaNodes.ix idx %= f
  writeRef (Ref idx) item =
    svGlobals.svAreaNodes.ix idx .= item