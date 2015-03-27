{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.CM where

import Control.Lens (use, (%=), (.=), (^.), ix, preuse)
import Control.Monad (void, when, unless, liftM)
import Data.Functor ((<$>))
import Data.Maybe (isNothing)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import Game.CModelT
import Game.CSurfaceT
import Game.MapSurfaceT
import QCommon.CLeafT
import QCommon.LumpT
import QCommon.QFiles.BSP.DHeaderT
import QCommon.QFiles.BSP.DLeafT
import QCommon.TexInfoT
import Util.QuakeFile (QuakeFile)
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified QCommon.MD4 as MD4
-- import qualified Util.QuakeFile as QuakeFile

-- Loads in the map and all submodels.
loadMap :: B.ByteString -> Bool -> [Int] -> Quake (CModelT, [Int])
loadMap name clientLoad checksum = do
    Com.dprintf $ "CM_LoadMap(" `B.append` name `B.append` ")...\n"

    void $ CVar.get "map_noareas" "0" 0

    mapName <- use $ cmGlobals.cmMapName
    flushMapValue <- CVar.variableValue "flushmap"

    if | mapName == name && (clientLoad || flushMapValue == 0) -> do
           lastChecksum <- use $ cmGlobals.cmLastChecksum
           let updatedChecksum = lastChecksum : tail checksum

           unless clientLoad $ do
             cmGlobals.cmPortalOpen %= UV.map (const False)
             floodAreaConnections

           -- still have the right version
           cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (cModel, updatedChecksum)

       | B.length name == 0 -> do
           resetSomeGlobals
           cmGlobals.cmNumNodes .= 0
           cmGlobals.cmNumLeafs .= 1
           cmGlobals.cmNumClusters .= 1
           cmGlobals.cmNumAreas .= 1

           -- cinematic servers won't have anything at all
           let updatedChecksum = 0 : tail checksum
           cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (cModel, updatedChecksum)

       | otherwise -> do
           resetSomeGlobals
           cmGlobals.cmNumNodes .= 0
           cmGlobals.cmNumLeafs .= 0

           --
           -- load the file
           --
           loadedFile <- FS.loadFile name

           when (isNothing loadedFile) $
             Com.comError Constants.errDrop ("Couldn't load " `B.append` name)

           let Just buf = BL.fromStrict <$> loadedFile
               len = BL.length buf
               bufChecksum = MD4.blockChecksum buf len
               updatedChecksum = bufChecksum : tail checksum

           cmGlobals.cmLastChecksum .= bufChecksum

           let header = newDHeaderT buf

           when (header^.dhVersion /= Constants.bspVersion) $
             Com.comError Constants.errDrop
                          ("CMod_LoadBrushModel: " `B.append`
                           name `B.append`
                           " has wrong version number (" `B.append`
                           BC.pack (show $ header^.dhVersion) `B.append` -- IMPROVE: convert Int to ByteString using binary package?
                           " should be " `B.append`
                           BC.pack (show Constants.bspVersion) `B.append` -- IMPROVE: convert Int to ByteString using binary package?
                           ")")

           cmGlobals.cmCModBase .= Just buf

           let lumps = header^.dhLumps

           loadSurfaces (lumps V.! Constants.lumpTexInfo)
           loadLeafs (lumps V.! Constants.lumpLeafs)
           loadLeafBrushes (lumps V.! Constants.lumpLeafBrushes)
           loadPlanes (lumps V.! Constants.lumpPlanes)
           loadBrushes (lumps V.! Constants.lumpBrushes)
           loadBrushSides (lumps V.! Constants.lumpBrushSides)
           loadSubmodels (lumps V.! Constants.lumpModels)

           loadNodes (lumps V.! Constants.lumpNodes)
           loadAreas (lumps V.! Constants.lumpAreas)
           loadAreaPortals (lumps V.! Constants.lumpAreaPortals)
           loadVisibility (lumps V.! Constants.lumpVisibility)
           loadEntityString (lumps V.! Constants.lumpEntities)

           initBoxHull

           cmGlobals.cmPortalOpen %= UV.map (const False)

           floodAreaConnections

           cmGlobals.cmMapName .= name

           cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (cModel, updatedChecksum)

  where resetSomeGlobals :: Quake ()
        resetSomeGlobals = do
           cmGlobals.cmNumCModels .= 0
           cmGlobals.cmNumVisibility .= 0
           cmGlobals.cmNumEntityChars .= 0
           cmGlobals.cmMapEntityString .= ""
           cmGlobals.cmMapName .= ""

loadSubmodels :: LumpT -> Quake ()
loadSubmodels _ = io (putStrLn "CM.loadSubmodels") >> undefined -- TODO

loadSurfaces :: LumpT -> Quake ()
loadSurfaces lump = do
    Com.dprintf "CMod_LoadSurfaces()\n"

    when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` texInfoTSize

    when (count < 1) $
      Com.comError Constants.errDrop "Map with no surfaces"

    when (count > Constants.maxMapTexInfo) $
      Com.comError Constants.errDrop "Map has too many surfaces"

    cmGlobals.cmNumTexInfo .= count
    Com.dprintf $ " numtexinfo=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE: convert Int to ByteString using binary package?

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "surfaces:\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapSurfaces <- mapM (readMapSurface buf) [0..count-1]
    cmGlobals.cmMapSurfaces %= (V.// updatedMapSurfaces)

  where readMapSurface :: BL.ByteString -> Int -> Quake (Int, MapSurfaceT)
        readMapSurface buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * texInfoTSize
              tex = newTexInfoT (BL.drop offset buf)
              csurface = CSurfaceT { _csName  = tex^.tiTexture
                                   , _csFlags = tex^.tiFlags
                                   , _csValue = tex^.tiValue
                                   }

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            Com.dprintf $ "| " `B.append` (tex^.tiTexture) `B.append`
                          "| " `B.append` (tex^.tiTexture) `B.append`
                          "| " `B.append` BC.pack (show (tex^.tiValue)) `B.append` -- IMPROVE: convert Int to ByteString using binary package?
                          "| " `B.append` BC.pack (show (tex^.tiFlags)) `B.append` -- IMPROVE: convert Bool to ByteString using binary package?
                          "|\n"

          return (idx, MapSurfaceT { _msCSurface = csurface, _msRName = Just (tex^.tiTexture) })

loadNodes :: LumpT -> Quake ()
loadNodes _ = io (putStrLn "CM.loadNodes") >> undefined -- TODO

loadBrushes :: LumpT -> Quake ()
loadBrushes _ = io (putStrLn "CM.loadBrushes") >> undefined -- TODO

loadLeafs :: LumpT -> Quake ()
loadLeafs lump = do
    Com.dprintf "CMod_LoadLeafs()\n"

    when ((lump^.lFileLen) `mod` dLeafTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dLeafTSize

    when (count < 1) $
      Com.comError Constants.errDrop "Map with no leafs"

    -- need to save space for box planes
    when (count > Constants.maxMapPlanes) $
      Com.comError Constants.errDrop "Map has too many planes"

    Com.dprintf $ " numleafes=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE: convert Int to ByteString using binary package?

    cmGlobals.cmNumLeafs .= count
    cmGlobals.cmNumClusters .= 0

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "cleaf-list:(contents, cluster, area, firstleafbrush, numleafbrushes)\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapLeafs <- mapM (readMapLeaf buf) [0..count-1]
    cmGlobals.cmMapLeafs %= (V.// updatedMapLeafs)

    numClusters <- use $ cmGlobals.cmNumClusters
    Com.dprintf $ " numclusters=" `B.append` BC.pack (show numClusters) `B.append` "\n" -- IMPROVE ?

    Just leafContents <- preuse $ cmGlobals.cmMapLeafs.ix 0.clContents
    when (leafContents /= Constants.contentsSolid) $
      Com.comError Constants.errDrop "Map leaf 0 is not CONTENTS_SOLID"

    cmGlobals.cmSolidLeaf .= 0
    cmGlobals.cmEmptyLeaf .= (-1)

    mapLeafs <- use $ cmGlobals.cmMapLeafs
    let emptyLeaf = V.findIndex (\leaf -> leaf^.clContents == 0) mapLeafs

    case emptyLeaf of
      Nothing -> Com.comError Constants.errDrop "Map does not have an empty leaf"
      Just idx -> cmGlobals.cmEmptyLeaf .= idx

  where readMapLeaf :: BL.ByteString -> Int -> Quake (Int, CLeafT)
        readMapLeaf buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dLeafTSize
              leaf = newDLeafT (BL.drop offset buf)
              cleaf = CLeafT { _clContents       = leaf^.dlContents
                             , _clCluster        = fromIntegral $ leaf^.dlCluster
                             , _clArea           = fromIntegral $ leaf^.dlArea
                             , _clFirstLeafBrush = leaf^.dlFirstLeafBrush
                             , _clNumLeafBrushes = leaf^.dlNumLeafBrushes
                             }

          numClusters <- use $ cmGlobals.cmNumClusters

          when (fromIntegral (leaf^.dlCluster) >= numClusters) $
            cmGlobals.cmNumClusters .= fromIntegral (leaf^.dlCluster) + 1

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            Com.dprintf $ "| " `B.append` BC.pack (show (leaf^.dlContents)) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show (leaf^.dlCluster)) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show (leaf^.dlFirstLeafBrush)) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show (leaf^.dlNumLeafBrushes)) `B.append` -- IMPROVE ?
                          "|\n"

          return (idx, cleaf)


loadPlanes :: LumpT -> Quake ()
loadPlanes _ = io (putStrLn "CM.loadPlanes") >> undefined -- TODO

loadLeafBrushes :: LumpT -> Quake ()
loadLeafBrushes _ = io (putStrLn "CM.loadLeafBrushes") >> undefined -- TODO

loadBrushSides :: LumpT -> Quake ()
loadBrushSides _ = io (putStrLn "CM.loadBrushSides") >> undefined -- TODO

loadAreas :: LumpT -> Quake ()
loadAreas _ = io (putStrLn "CM.loadAreas") >> undefined -- TODO

loadAreaPortals :: LumpT -> Quake ()
loadAreaPortals _ = io (putStrLn "CM.loadAreaPortals") >> undefined -- TODO

loadVisibility :: LumpT -> Quake ()
loadVisibility _ = io (putStrLn "CM.loadVisibility") >> undefined -- TODO

loadEntityString :: LumpT -> Quake ()
loadEntityString _ = io (putStrLn "CM.loadEntityString") >> undefined -- TODO

initBoxHull :: Quake ()
initBoxHull = io (putStrLn "CM.initBoxHull") >> undefined -- TODO

inlineModel :: B.ByteString -> Quake CModelT
inlineModel _ = io (putStrLn "CM.inlineModel") >> undefined -- TODO

numInlineModels :: Quake Int
numInlineModels = io (putStrLn "CM.numInlineModels") >> undefined -- TODO

entityString :: Quake B.ByteString
entityString = io (putStrLn "CM.entityString") >> undefined -- TODO

-- CM_WritePortalState writes the portal state to a savegame file.
writePortalState :: QuakeFile -> Quake ()
writePortalState _ = io (putStrLn "CM.writePortalState") >> undefined -- TODO

-- CM_ReadPortalState reads the portal state from a savegame file and recalculates the area connections.
readPortalState :: QuakeFile -> Quake ()
readPortalState _ = io (putStrLn "CM.readPortalState") >> undefined -- TODO

setAreaPortalState :: Int -> Bool -> Quake ()
setAreaPortalState _ _ = io (putStrLn "CM.setAreaPortalState") >> undefined -- TODO

areasConnected :: Int -> Int -> Quake Bool
areasConnected _ _ = io (putStrLn "CM.areasConnected") >> undefined -- TODO

floodAreaConnections :: Quake ()
floodAreaConnections = io (putStrLn "CM.floodAreaConnections") >> undefined -- TODO
