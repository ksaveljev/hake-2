{-# LANGUAGE MultiParamTypeClasses #-}
module QuakeRef where

import           QuakeIOState
import           QuakeState
import           Types

import           Control.Lens (use, ix, (%=), (.=))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

class QuakeRef a b | a -> b where
  readRef :: a -> Quake b
  modifyRef :: a -> (b -> b) -> Quake ()
  writeRef :: a -> b -> Quake ()

instance QuakeRef EdictRef EdictT where
  readRef (EdictRef idx) =
    request (do edicts <- use ioGEdicts
                io (MV.read edicts idx))
  modifyRef (EdictRef idx) f =
    request (do edicts <- use ioGEdicts
                io (MV.modify edicts f idx))
  writeRef (EdictRef idx) edict =
    request (do edicts <- use ioGEdicts
                io (MV.write edicts idx edict))

instance QuakeRef ClientRef ClientT where
  readRef (ClientRef idx) =
    do clients <- use (svGlobals.svServerStatic.ssClients)
       return (clients V.! idx)
  modifyRef (ClientRef idx) f =
    svGlobals.svServerStatic.ssClients.ix idx %= f
  writeRef (ClientRef idx) client =
    svGlobals.svServerStatic.ssClients.ix idx .= client
  
instance QuakeRef GClientRef GClientT where
  readRef (GClientRef idx) =
    do gClients <- use (gameBaseGlobals.gbGame.glClients)
       return (gClients V.! idx)
  modifyRef (GClientRef idx) f =
    gameBaseGlobals.gbGame.glClients.ix idx %= f
  writeRef (GClientRef idx) gClient =
    gameBaseGlobals.gbGame.glClients.ix idx .= gClient
  
instance QuakeRef CModelRef CModelT where
  readRef (CModelRef idx) =
    do cModels <- use (cmGlobals.cmMapCModels)
       return (cModels V.! idx)
  modifyRef (CModelRef idx) f =
    cmGlobals.cmMapCModels.ix idx %= f
  writeRef (CModelRef idx) cModel =
    cmGlobals.cmMapCModels.ix idx .= cModel
  
instance QuakeRef LinkRef LinkT where
  readRef (LinkRef idx) =
    do links <- use (svGlobals.svLinks)
       return (links V.! idx)
  modifyRef (LinkRef idx) f =
    svGlobals.svLinks.ix idx %= f
  writeRef (LinkRef idx) link =
    svGlobals.svLinks.ix idx .= link

instance QuakeRef GItemRef GItemT where
  readRef (GItemRef idx) =
    do items <- use (gameBaseGlobals.gbItemList)
       return (items V.! idx)
  modifyRef (GItemRef idx) f =
    gameBaseGlobals.gbItemList.ix idx %= f
  writeRef (GItemRef idx) item =
    gameBaseGlobals.gbItemList.ix idx .= item