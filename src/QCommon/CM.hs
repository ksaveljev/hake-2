{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
module QCommon.CM where

import Control.Lens (use, (%=), (.=), (^.), (+=), ix, preuse, Lens', zoom, _1, _2)
import Control.Monad (void, when, unless)
import Data.Bits ((.|.), (.&.), shiftR)
import Data.Functor ((<$>))
import Data.Int (Int8)
import Data.Maybe (isNothing, fromJust)
import Data.Word (Word16)
import Linear (V3(..), _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import Game.CSurfaceT
import Game.MapSurfaceT
import QCommon.CAreaT
import QCommon.CBrushT
import QCommon.CBrushSideT
import QCommon.CLeafT
import QCommon.CNodeT
import QCommon.LumpT
import QCommon.QFiles.BSP.DAreaPortalT
import QCommon.QFiles.BSP.DAreaT
import QCommon.QFiles.BSP.DBrushSideT
import QCommon.QFiles.BSP.DBrushT
import QCommon.QFiles.BSP.DHeaderT
import QCommon.QFiles.BSP.DLeafT
import QCommon.QFiles.BSP.DModelT
import QCommon.QFiles.BSP.DNodeT
import QCommon.QFiles.BSP.DPlaneT
import QCommon.QFiles.BSP.DVisT
import QCommon.TexInfoT
import Util.Binary
import Util.QuakeFile (QuakeFile)
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified QCommon.MD4 as MD4
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

-- Loads in the map and all submodels.
loadMap :: B.ByteString -> Bool -> [Int] -> Quake (Int, [Int]) -- return model index (cmGlobals.cmMapCModels) and checksum
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
           --cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (0, updatedChecksum)

       | B.length name == 0 -> do
           resetSomeGlobals
           cmGlobals.cmNumNodes .= 0
           cmGlobals.cmNumLeafs .= 1
           cmGlobals.cmNumClusters .= 1
           cmGlobals.cmNumAreas .= 1

           -- cinematic servers won't have anything at all
           let updatedChecksum = 0 : tail checksum
           --cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (0, updatedChecksum)

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

           --cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (0, updatedChecksum)

  where resetSomeGlobals :: Quake ()
        resetSomeGlobals = do
           cmGlobals.cmNumCModels .= 0
           cmGlobals.cmNumVisibility .= 0
           cmGlobals.cmNumEntityChars .= 0
           cmGlobals.cmMapEntityString .= ""
           cmGlobals.cmMapName .= ""

loadSubmodels :: LumpT -> Quake ()
loadSubmodels lump = do
    Com.dprintf "CMod_LoadSubmodels()\n"

    when ((lump^.lFileLen) `mod` dModelTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dModelTSize

    when (count < 1) $
      Com.comError Constants.errDrop "Map with no models"

    when (count > Constants.maxMapModels) $
      Com.comError Constants.errDrop "Map has too many models"

    Com.dprintf $ " numcmodels=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE ?

    cmGlobals.cmNumCModels .= count

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "submodles(headnode, <origin>, <mins>, <maxs>)\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapCModels <- mapM (readMapCModel buf) [0..count-1]
    cmGlobals.cmMapCModels %= (V.// updatedMapCModels)

  where readMapCModel :: BL.ByteString -> Int -> Quake (Int, CModelT)
        readMapCModel buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dModelTSize
              model = newDModelT (BL.drop offset buf)
              cmodel = CModelT { _cmMins     = fmap (\a -> a - 1) (model^.dmMins)
                               , _cmMaxs     = fmap (+1) (model^.dmMaxs)
                               , _cmOrigin   = model^.dmOrigin
                               , _cmHeadNode = model^.dmHeadNode
                               }

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            io (putStrLn "CM.loadSubmodels#readMapCModel") >> undefined -- TODO

          return (idx, cmodel)

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
loadNodes lump = do
    Com.dprintf "CMod_LoadNodes()\n"

    when ((lump^.lFileLen) `mod` dNodeTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dNodeTSize

    when (count < 1) $
      Com.comError Constants.errDrop "Map with no nodes"

    when (count > Constants.maxMapNodes) $
      Com.comError Constants.errDrop "Map has too many nodes"

    cmGlobals.cmNumNodes .= count

    Com.dprintf $ " numnodes=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE ?

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "nodes(planenum, child[0], child[1])\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapNodes <- mapM (readMapNode buf) [0..count-1]
    cmGlobals.cmMapNodes %= (V.// updatedMapNodes)

  where readMapNode :: BL.ByteString -> Int -> Quake (Int, CNodeT)
        readMapNode buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dNodeTSize
              node = newDNodeT (BL.drop offset buf)
              cnode = CNodeT { _cnPlane    = Just (node^.dnPlaneNum)
                             , _cnChildren = node^.dnChildren
                             }

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            io (putStrLn "CM.loadNodes#readMapNode") >> undefined -- TODO

          return (idx, cnode)

loadBrushes :: LumpT -> Quake ()
loadBrushes lump = do
    Com.dprintf "CMod_LoadBrushes()\n"

    when ((lump^.lFileLen) `mod` dBrushTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dBrushTSize

    when (count > Constants.maxMapBrushes) $
      Com.comError Constants.errDrop "Map has too many brushes"

    cmGlobals.cmNumBrushes .= count

    Com.dprintf $ " numbrushes=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "brushes:(firstbrushside, numsides, contents)\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapBrushes <- mapM (readMapBrush buf) [0..count-1]
    cmGlobals.cmMapBrushes %= (V.// updatedMapBrushes)

  where readMapBrush :: BL.ByteString -> Int -> Quake (Int, CBrushT)
        readMapBrush buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dBrushTSize
              brush = newDBrushT (BL.drop offset buf)
              cbrush = CBrushT { _cbContents       = brush^.dbContents
                               , _cbNumSides       = brush^.dbNumSides
                               , _cbFirstBrushSide = brush^.dbFirstSide
                               , _cbCheckCount     = 0
                               }

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            Com.dprintf $ "| " `B.append` BC.pack (show $ brush^.dbFirstSide) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show $ brush^.dbNumSides) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show $ brush^.dbContents) `B.append` -- IMPROVE ?
                          "|\n"

          return (idx, cbrush)

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

    Just lContents <- preuse $ cmGlobals.cmMapLeafs.ix 0.clContents
    when (lContents /= Constants.contentsSolid) $
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
loadPlanes lump = do
    Com.dprintf "CMod_LoadPlanes()\n"

    when ((lump^.lFileLen) `mod` dPlaneTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dPlaneTSize

    when (count < 1) $
      Com.comError Constants.errDrop "Map with no planes"

    -- need to save space for box planes
    when (count > Constants.maxMapPlanes) $
      Com.comError Constants.errDrop "Map has too many planes"

    Com.dprintf $ " numplanes=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE ?

    cmGlobals.cmNumPlanes .= count

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "cplanes(normal[0],normal[1],normal[2], dist, type, signbits)\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapPlanes <- mapM (readMapPlane buf) [0..count-1]
    cmGlobals.cmMapPlanes %= (V.// updatedMapPlanes)

  where readMapPlane :: BL.ByteString -> Int -> Quake (Int, CPlaneT)
        readMapPlane buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dPlaneTSize
              plane = newDPlaneT (BL.drop offset buf)
              cplane = CPlaneT { _cpNormal   = plane^.dpNormal
                               , _cpDist     = plane^.dpDist
                               , _cpType     = fromIntegral (plane^.dpType)
                               , _cpSignBits = getBits (plane^.dpNormal)
                               , _cpPad      = (0, 0)
                               }

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            Com.dprintf $ "| " `B.append` BC.pack (show $ cplane^.cpNormal._x) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show $ cplane^.cpNormal._y) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show $ cplane^.cpNormal._z) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show $ cplane^.cpDist) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show $ cplane^.cpType) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show $ cplane^.cpSignBits) `B.append` -- IMPROVE ?
                          "|\n"

          return (idx, cplane)

        getBits :: V3 Float -> Int8
        getBits (V3 a b c) =
          let a' = if a < 0 then 1 else 0
              b' = if b < 0 then 2 else 0
              c' = if c < 0 then 4 else 0
          in a' .|. b' .|. c'

loadLeafBrushes :: LumpT -> Quake ()
loadLeafBrushes lump = do
    Com.dprintf "CMod_LoadLeafBrushes()\n"

    when ((lump^.lFileLen) `mod` 2 /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` 2

    Com.dprintf $ " numbrushes=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE: convert Int to ByteString using binary package?

    when (count < 1) $
      Com.comError Constants.errDrop "Map with no planes"

    -- need to save space for box planes
    when (count > Constants.maxMapLeafBrushes) $
      Com.comError Constants.errDrop "Map has too many leafbrushes"

    cmGlobals.cmNumLeafBrushes .= count

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "map_brushes:\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapLeafBrushes <- mapM (readMapLeafBrush buf) [0..count-1]
    cmGlobals.cmMapLeafBrushes %= (UV.// updatedMapLeafBrushes)

  where readMapLeafBrush :: BL.ByteString -> Int -> Quake (Int, Word16)
        readMapLeafBrush buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * 2
              val = runGet getWord16le (BL.drop offset buf)

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            Com.dprintf $ "| " `B.append` BC.pack (show idx) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show val) `B.append` -- IMPROVE ?
                          "|\n"

          return (idx, val)

loadBrushSides :: LumpT -> Quake ()
loadBrushSides lump = do
    Com.dprintf "CMod_LoadBrushSides()\n"

    when ((lump^.lFileLen) `mod` dBrushSideTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dBrushSideTSize

    when (count > Constants.maxMapBrushSides) $
      Com.comError Constants.errDrop "Map has too many planes"

    cmGlobals.cmNumBrushSides .= count

    Com.dprintf $ " numbrushsides=" `B.append` BC.pack (show count) `B.append` "\n" -- IMPROVE ?

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "brushside(planenum, surfacenum):\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapBrushSides <- mapM (readMapBrushSide buf) [0..count-1]
    cmGlobals.cmMapBrushSides %= (V.// updatedMapBrushSides)

  where readMapBrushSide :: BL.ByteString -> Int -> Quake (Int, CBrushSideT)
        readMapBrushSide buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dBrushSideTSize
              brushSide = newDBrushSideT (BL.drop offset buf)
              num = fromIntegral $ brushSide^.dbsPlaneNum
              j = fromIntegral $ brushSide^.dbsTexInfo

          numTexInfo <- use $ cmGlobals.cmNumTexInfo

          when (j >= numTexInfo) $
            Com.comError Constants.errDrop "Bad brushside texinfo"

          let cbrushside = CBrushSideT { cbsPlane   = Just num
                                       , cbsSurface = Just j
                                       }

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            Com.dprintf $ "| " `B.append` BC.pack (show num) `B.append` -- IMPROVE ?
                          "| " `B.append` BC.pack (show j) `B.append` -- IMPROVE ?
                          "|\n"

          return (idx, cbrushside)

loadAreas :: LumpT -> Quake ()
loadAreas lump = do
    Com.dprintf "CMod_LoadAreas()\n"

    when ((lump^.lFileLen) `mod` dAreaTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dAreaTSize

    when (count > Constants.maxMapAreas) $
      Com.comError Constants.errDrop "Map has too many areas"

    Com.dprintf $ " numareas=" `B.append` BC.pack (show count) `B.append` "\n"

    cmGlobals.cmNumAreas .= count

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "areas(numportals, firstportal)\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapAreas <- mapM (readMapArea buf) [0..count-1]
    cmGlobals.cmMapAreas %= (V.// updatedMapAreas)

  where readMapArea :: BL.ByteString -> Int -> Quake (Int, CAreaT)
        readMapArea buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dAreaTSize
              area = newDAreaT (BL.drop offset buf)
              carea = CAreaT { _caNumAreaPortals  = area^.daNumAreaPortals
                             , _caFirstAreaPortal = area^.daFirstAreaPortal
                             , _caFloodNum        = 0
                             , _caFloodValid      = 0
                             }

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            io (putStrLn "CM.loadAreas#readMapArea") >> undefined -- TODO

          return (idx, carea)

loadAreaPortals :: LumpT -> Quake ()
loadAreaPortals lump = do
    Com.dprintf "CMod_LoadAreaPortals()\n"

    when ((lump^.lFileLen) `mod` dAreaPortalTSize /= 0) $
      Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"

    let count = (lump^.lFileLen) `div` dAreaPortalTSize

    when (count > Constants.maxMapAreas) $
      Com.comError Constants.errDrop "Map has too many areas"

    cmGlobals.cmNumAreaPortals .= count

    Com.dprintf $ " numareaportals=" `B.append` BC.pack (show count) `B.append` "\n"

    whenQ (use $ cmGlobals.cmDebugLoadMap) $
      Com.dprintf "areaportals(portalnum, otherarea)\n"

    Just buf <- use $ cmGlobals.cmCModBase

    updatedMapAreaPortals <- mapM (readMapAreaPortal buf) [0..count-1]
    cmGlobals.cmMapAreaPortals %= (V.// updatedMapAreaPortals)

  where readMapAreaPortal :: BL.ByteString -> Int -> Quake (Int, DAreaPortalT)
        readMapAreaPortal buf idx = do
          let offset = fromIntegral $ (lump^.lFileOfs) + idx * dAreaPortalTSize
              areaPortal = newDAreaPortalT (BL.drop offset buf)

          whenQ (use $ cmGlobals.cmDebugLoadMap) $
            io (putStrLn "CM.loadAreaPortals#readMapAreaPortal") >> undefined -- TODO

          return (idx, areaPortal)


loadVisibility :: LumpT -> Quake ()
loadVisibility lump = do
    Com.dprintf "CMod_LoadVisibility()\n"

    cmGlobals.cmNumVisibility .= lump^.lFileLen

    Com.dprintf $ " numvisibility=" `B.append` BC.pack (show (lump^.lFileLen)) `B.append` "\n"

    when ((lump^.lFileLen) > Constants.maxMapVisibility) $
      Com.comError Constants.errDrop "Map has too large visibility lump"

    Just buf <- use $ cmGlobals.cmCModBase

    let visData = BL.take (fromIntegral (lump^.lFileLen)) $ BL.drop (fromIntegral (lump^.lFileOfs)) buf

    cmGlobals.cmMapVisibility .= visData
    cmGlobals.cmMapVis .= newDVisT visData

loadEntityString :: LumpT -> Quake ()
loadEntityString lump = do
    Com.dprintf "CMod_LoadEntityString()\n"

    cmGlobals.cmNumEntityChars .= (lump^.lFileLen)

    when ((lump^.lFileLen) > Constants.maxMapEntString) $
      Com.comError Constants.errDrop "Map has too large entity lump"

    Just buf <- use $ cmGlobals.cmCModBase

    let entitystring = B.takeWhile (/= 0) $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) (BL.toStrict buf))

    cmGlobals.cmMapEntityString .= entitystring

    Com.dprintf $ "entitystring=" `B.append` BC.pack (show $ B.length entitystring) `B.append`
                  " bytes, [" `B.append` B.take 15 entitystring `B.append` "\n" -- TODO

{- Set up the planes and nodes so that the six floats of a bounding box can
- just be stored out and get a proper clipping hull structure.
-}
initBoxHull :: Quake ()
initBoxHull = do
    numNodes <- use $ cmGlobals.cmNumNodes
    numBrushes <- use $ cmGlobals.cmNumBrushes
    numLeafBrushes <- use $ cmGlobals.cmNumLeafBrushes
    numBrushSides <- use $ cmGlobals.cmNumBrushSides
    numPlanes <- use $ cmGlobals.cmNumPlanes

    when ( numNodes + 6 > Constants.maxMapNodes
        || numBrushes + 1 > Constants.maxMapBrushes
        || numLeafBrushes + 1 > Constants.maxMapLeafBrushes
        || numBrushSides + 6 > Constants.maxMapBrushSides
        || numPlanes + 12 > Constants.maxMapPlanes
        ) $
      Com.comError Constants.errDrop "Not enough room for box tree"

    cmGlobals.cmBoxHeadNode .= numNodes

    let boxBrush = CBrushT { _cbContents       = Constants.contentsMonster
                           , _cbNumSides       = 6
                           , _cbFirstBrushSide = numBrushSides
                           , _cbCheckCount     = 0
                           }

    cmGlobals.cmMapBrushes %= (V.// [(numBrushes, boxBrush)])

    let boxLeaf = CLeafT { _clContents       = Constants.contentsMonster
                         , _clCluster        = 0
                         , _clArea           = 0
                         , _clFirstLeafBrush = fromIntegral numLeafBrushes
                         , _clNumLeafBrushes = 1
                         }

    numLeafs <- use $ cmGlobals.cmNumLeafs
    cmGlobals.cmMapLeafs %= (V.// [(numLeafs, boxLeaf)])

    cmGlobals.cmMapLeafBrushes %= (UV.// [(numLeafBrushes, fromIntegral numBrushes)])

    mapM_ (setBrushSidesNodesAndPlanes numBrushSides numPlanes numLeafs) [0..5]

  where setBrushSidesNodesAndPlanes :: Int -> Int -> Int -> Int -> Quake ()
        setBrushSidesNodesAndPlanes numBrushSides numPlanes numLeafs idx = do
          let side = idx .&. 1

          -- brush sides
          let s = CBrushSideT { cbsPlane   = Just (numPlanes + idx * 2 + side)
                              , cbsSurface = Nothing
                              }

          cmGlobals.cmMapBrushSides %= (V.// [(numBrushSides + idx, s)])

          -- nodes
          emptyLeaf <- use $ cmGlobals.cmEmptyLeaf
          boxHeadNode <- use $ cmGlobals.cmBoxHeadNode

          let c = CNodeT { _cnPlane    = Just (numPlanes + idx * 2)
                         , _cnChildren = calcChildren idx side numLeafs emptyLeaf boxHeadNode
                         }

          cmGlobals.cmMapNodes %= (V.// [(boxHeadNode + idx, c)])

          -- planes
          let p1 = CPlaneT { _cpNormal   = getNormal idx 1
                           , _cpDist     = 0
                           , _cpType     = fromIntegral (idx `shiftR` 1)
                           , _cpSignBits = 0
                           , _cpPad      = (0, 0)
                           }

          let p2 = CPlaneT { _cpNormal   = getNormal idx (-1)
                           , _cpDist     = 0
                           , _cpType     = fromIntegral (3 + (idx `shiftR` 1))
                           , _cpSignBits = 0
                           , _cpPad      = (0, 0)
                           }

          cmGlobals.cmMapPlanes %= (V.// [(numPlanes + idx * 2, p1), (numPlanes + idx * 2 + 1, p2)])

        calcChildren :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
        calcChildren idx side numLeafs emptyLeaf boxHeadNode =
          let a = (-1) - emptyLeaf
              b = if idx == 5 then (-1) - numLeafs else boxHeadNode + idx + 1
          in if side == 0 then (a, b) else (b, a)

        getNormal :: Int -> Float -> V3 Float
        getNormal idx v =
          let x = idx `shiftR` 1
          in if | x == 0 -> V3 v 0 0
                | x == 1 -> V3 0 v 0
                | otherwise -> V3 0 0 v

inlineModel :: B.ByteString -> Quake Int -- return index of cmGlobals.cmMapCModels
inlineModel name = do
    when (name == "" || BC.head name /= '*') $
      Com.comError Constants.errDrop "CM_InlineModel: bad name"

    let num = Lib.atoi (B.drop 1 name)
    numCModels <- use $ cmGlobals.cmNumCModels

    when (num < 1 || num >= numCModels) $
      Com.comError Constants.errDrop "CM_InlineModel: bad number"

    return num

numInlineModels :: Quake Int
numInlineModels = use $ cmGlobals.cmNumCModels

entityString :: Quake B.ByteString
entityString = use $ cmGlobals.cmMapEntityString

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

floodAreaR :: Int -> Int -> Quake ()
floodAreaR areaIdx floodNum = do
    floodValid <- use $ cmGlobals.cmFloodValid
    Just area <- preuse $ cmGlobals.cmMapAreas.ix areaIdx

    if (area^.caFloodValid) == floodValid
      then when ((area^.caFloodNum) == floodNum) $
             Com.comError Constants.errDrop "FloodArea_r: reflooded"
      else do
        let updatedArea = area { _caFloodNum = floodNum, _caFloodValid = floodValid }
        cmGlobals.cmMapAreas %= (V.// [(areaIdx, updatedArea)])

        continueFlood area 0 ((area^.caNumAreaPortals)-1)

  where continueFlood :: CAreaT -> Int -> Int -> Quake ()
        continueFlood area idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              portalOpen <- use $ cmGlobals.cmPortalOpen
              Just p <- preuse $ cmGlobals.cmMapAreaPortals.ix ((area^.caFirstAreaPortal) + idx)

              when (portalOpen UV.! (p^.dapPortalNum)) $
                floodAreaR (p^.dapOtherArea) floodNum

              continueFlood area (idx + 1) maxIdx

floodAreaConnections :: Quake ()
floodAreaConnections = do
    Com.dprintf "FloodAreaConnections...\n"

    -- all current floods are not invalid
    cmGlobals.cmFloodValid += 1

    floodValid <- use $ cmGlobals.cmFloodValid
    numAreas <- use $ cmGlobals.cmNumAreas

    -- area 0 is not used
    flood floodValid 1 (numAreas - 1) 0

  where flood :: Int -> Int -> Int -> Int -> Quake ()
        flood floodValid idx maxIdx floodNum
          | idx >= maxIdx = return ()
          | otherwise = do
              Just area <- preuse $ cmGlobals.cmMapAreas.ix idx
              if (area^.caFloodValid) == floodValid
                then flood floodValid (idx + 1) maxIdx floodNum
                else do
                  floodAreaR idx (floodNum + 1)
                  flood floodValid (idx + 1) maxIdx (floodNum + 1)

-- fills in a list of all the leafs touched
boxLeafNums :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> [Int] -> Quake (Int, [Int])
boxLeafNums mins maxs list listSize topnode = do
    Just headnode <- preuse $ cmGlobals.cmMapCModels.ix 0.cmHeadNode
    boxLeafNumsHeadnode mins maxs list listSize headnode topnode

-- fills in a list of all the leafs touched and starts with the head node
boxLeafNumsHeadnode :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> Int -> [Int] -> Quake (Int, [Int])
boxLeafNumsHeadnode mins maxs list listSize headnode topnode = do
    zoom cmGlobals $ do
      cmLeafCount .= 0
      cmLeafTopNode .= (-1)

    boxLeafNumsR mins maxs list listSize headnode

    leafCount <- use $ cmGlobals.cmLeafCount
    leafTopNode <- use $ cmGlobals.cmLeafTopNode

    return (leafCount, leafTopNode : tail topnode)

-- recursively fills in a list of all the leafs touched
boxLeafNumsR :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> Int -> Quake ()
boxLeafNumsR mins maxs leafList leafMaxCount nodenum
  | nodenum < 0 = do
      leafCount <- use $ cmGlobals.cmLeafCount

      if leafCount >= leafMaxCount
        then Com.dprintf "CM_BoxLeafnums_r: overflow\n"
        else do
          leafList.ix leafCount .= (-1) - nodenum
          cmGlobals.cmLeafCount += 1

  | otherwise = do
      Just node <- preuse $ cmGlobals.cmMapNodes.ix nodenum
      let planeIdx = fromJust (node^.cnPlane)

      s <- Math3D.boxOnPlaneSize mins maxs planeIdx

      if | s == 1 -> boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._1)
         | s == 2 -> boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._2)
         | otherwise -> do
             leafTopNode <- use $ cmGlobals.cmLeafTopNode

             when (leafTopNode == -1) $ do
               cmGlobals.cmLeafTopNode .= nodenum

             boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._1)
             boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._2)

leafContents :: Int -> Quake Int
leafContents _ = io (putStrLn "CM.leafContents") >> undefined -- TODO

leafCluster :: Int -> Quake Int
leafCluster _ = io (putStrLn "CM.leafCluster") >> undefined -- TODO

leafArea :: Int -> Quake Int
leafArea _ = io (putStrLn "CM.leafArea") >> undefined -- TODO
