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