{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module QCommon.CM where

import Control.Lens (use, (%=), (.=), (^.), (+=), ix, preuse, Lens', zoom, _1, _2, Traversal')
import Control.Monad (void, when, unless, liftM)
import Data.Bits ((.|.), (.&.), shiftR, shiftL)
import Data.Functor ((<$>))
import Data.Int (Int8, Int64)
import Data.Maybe (isNothing, fromJust)
import Data.Word (Word8, Word16)
import Linear (V3(..), _x, _y, _z, dot)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import Game.CSurfaceT
import QCommon.LumpT
import QCommon.QFiles.BSP.DAreaT
import QCommon.QFiles.BSP.DBrushSideT
import QCommon.QFiles.BSP.DBrushT
import QCommon.QFiles.BSP.DHeaderT
import QCommon.QFiles.BSP.DLeafT
import QCommon.QFiles.BSP.DModelT
import QCommon.QFiles.BSP.DNodeT
import QCommon.QFiles.BSP.DPlaneT
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

-- 1/32 epsilon to keep floating point happy
distEpsilon :: Float
distEpsilon = 0.03125

nullSurface :: MapSurfaceT
nullSurface = newMapSurfaceT

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

          -- j == -1 case should be handled someway
          -- we simply point to a hacky empty MapSurfaceT
          let cbrushside = CBrushSideT { _cbsPlane   = Just num
                                       , _cbsSurface = Just (if j == -1 then Constants.maxMapTexInfo else j)
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
          let s = CBrushSideT { _cbsPlane   = Just (numPlanes + idx * 2 + side)
                              , _cbsSurface = Nothing
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

inlineModel :: B.ByteString -> Quake CModelReference
inlineModel name = do
    when (name == "" || BC.head name /= '*') $
      Com.comError Constants.errDrop "CM_InlineModel: bad name"

    let num = Lib.atoi (B.drop 1 name)
    numCModels <- use $ cmGlobals.cmNumCModels

    when (num < 1 || num >= numCModels) $
      Com.comError Constants.errDrop "CM_InlineModel: bad number"

    return (CModelReference num)

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
setAreaPortalState portalNum open = do
    numAreaPortals <- use $ cmGlobals.cmNumAreaPortals

    when (portalNum > numAreaPortals) $
      Com.comError Constants.errDrop "areaportal > numareaportals"

    cmGlobals.cmPortalOpen.ix portalNum .= open
    floodAreaConnections

-- CM_AreasConnected returns true, if two areas are connected.
areasConnected :: Int -> Int -> Quake Bool
areasConnected area1 area2 = do
    noAreasValue <- liftM (^.cvValue) mapNoAreasCVar

    if noAreasValue /= 0
      then return True
      else do
        numAreas <- use $ cmGlobals.cmNumAreas

        when (area1 > numAreas || area2 > numAreas) $
          Com.comError Constants.errDrop "area > numareas"

        mapAreas <- use $ cmGlobals.cmMapAreas

        if ((mapAreas V.! area1)^.caFloodNum) == ((mapAreas V.! area2)^.caFloodNum)
          then return True
          else return False

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
boxLeafNums :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> Maybe [Int] -> Quake (Int, Maybe [Int])
boxLeafNums mins maxs list listSize topnode = do
    Just headnode <- preuse $ cmGlobals.cmMapCModels.ix 0.cmHeadNode
    boxLeafNumsHeadnode mins maxs list listSize headnode topnode

-- fills in a list of all the leafs touched and starts with the head node
boxLeafNumsHeadnode :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> Int -> Maybe [Int] -> Quake (Int, Maybe [Int])
boxLeafNumsHeadnode mins maxs list listSize headnode topnode = do
    zoom cmGlobals $ do
      cmLeafCount .= 0
      cmLeafTopNode .= (-1)

    boxLeafNumsR mins maxs list listSize headnode

    leafCount <- use $ cmGlobals.cmLeafCount
    leafTopNode <- use $ cmGlobals.cmLeafTopNode

    return $ case topnode of
               Nothing -> (leafCount, Nothing)
               Just x -> (leafCount, Just (leafTopNode : tail x))

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
      Just p <- preuse $ cmGlobals.cmMapPlanes.ix planeIdx
      let s = Math3D.boxOnPlaneSide mins maxs p

      if | s == 1 -> boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._1)
         | s == 2 -> boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._2)
         | otherwise -> do
             leafTopNode <- use $ cmGlobals.cmLeafTopNode

             when (leafTopNode == -1) $ do
               cmGlobals.cmLeafTopNode .= nodenum

             boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._1)
             boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._2)

leafContents :: Int -> Quake Int
leafContents leafNum = do
    numLeafs <- use $ cmGlobals.cmNumLeafs

    when (leafNum < 0 || leafNum >= numLeafs) $
      Com.comError Constants.errDrop "CM_LeafContents: bad number"

    Just contents <- preuse $ cmGlobals.cmMapLeafs.ix leafNum.clContents
    return contents

leafCluster :: Int -> Quake Int
leafCluster leafNum = do
    numLeafs <- use $ cmGlobals.cmNumLeafs

    when (leafNum < 0 || leafNum >= numLeafs) $
      Com.comError Constants.errDrop "CM_LeafCluster: bad number"

    Just cluster <- preuse $ cmGlobals.cmMapLeafs.ix leafNum.clCluster
    return cluster

leafArea :: Int -> Quake Int
leafArea leafNum = do
    numLeafs <- use $ cmGlobals.cmNumLeafs

    when (leafNum < 0 || leafNum >= numLeafs) $
      Com.comError Constants.errDrop "CM_LeafArea: bad number"

    Just area <- preuse $ cmGlobals.cmMapLeafs.ix leafNum.clArea
    return area

boxTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Quake TraceT
boxTrace start end mins maxs headNode brushMask = do
    -- for multi-check avoidance
    cmGlobals.cmCheckCount += 1

    -- for statistics, may be zeroed
    globals.cTraces += 1

    -- fill in a default trace
    -- was: memset(& trace_trace, 0, sizeof(trace_trace));
    cmGlobals.cmTraceTrace .= newTraceT { _tFraction = 1
                                        , _tSurface  = Just (nullSurface^.msCSurface)
                                        }

    numNodes <- use $ cmGlobals.cmNumNodes

    if numNodes == 0
      -- map not loaded
      then
        use $ cmGlobals.cmTraceTrace
      else do
        zoom (cmGlobals) $ do
          cmTraceContents .= brushMask
          cmTraceStart .= start
          cmTraceEnd .= end
          cmTraceMins .= mins
          cmTraceMaxs .= maxs

        if start == end
          then do
            -- check for position test special case
            let c1 = fmap (subtract 1) (start + mins)
                c2 = fmap (+ 1) (start + maxs)

            (numLeafs, _) <- boxLeafNumsHeadnode c1 c2 (cmGlobals.cmLeafs) 1024 headNode (Just [0])

            checkLeafs 0 numLeafs

            cmGlobals.cmTraceTrace.tEndPos .= start

            use $ cmGlobals.cmTraceTrace
          else do
            -- check for point special case
            let nullV3 :: V3 Float = V3 0 0 0

            if (mins == nullV3) && (maxs == nullV3)
              then do
                cmGlobals.cmTraceIsPoint .= True
                cmGlobals.cmTraceExtents .= nullV3
              else do
                cmGlobals.cmTraceIsPoint .= False
                let a = if (- (mins^._x)) > (maxs^._x) then (- (mins^._x)) else maxs^._x
                    b = if (- (mins^._y)) > (maxs^._y) then (- (mins^._y)) else maxs^._y
                    c = if (- (mins^._z)) > (maxs^._z) then (- (mins^._z)) else maxs^._z
                cmGlobals.cmTraceExtents .= V3 a b c

            -- general sweeping through world
            recursiveHullCheck headNode 0 1 start end

            traceFraction <- use $ cmGlobals.cmTraceTrace.tFraction

            if traceFraction == 1
              then cmGlobals.cmTraceTrace.tEndPos .= end
              else cmGlobals.cmTraceTrace.tEndPos .= start + fmap (* traceFraction) (end - start)

            traceTrace <- use $ cmGlobals.cmTraceTrace
            return traceTrace

  where checkLeafs :: Int -> Int -> Quake ()
        checkLeafs idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just leafIdx <- preuse $ cmGlobals.cmLeafs.ix idx
              testInLeaf leafIdx

              allSolid <- use $ cmGlobals.cmTraceTrace.tAllSolid

              unless allSolid $
                checkLeafs (idx + 1) maxIdx

testInLeaf :: Int -> Quake ()
testInLeaf leafNum = do
    Just leaf <- preuse $ cmGlobals.cmMapLeafs.ix leafNum
    traceContents <- use $ cmGlobals.cmTraceContents

    when ((leaf^.clContents) .&. traceContents /= 0) $
      -- trace line against all brushes in the leaf
      traceLine (fromIntegral $ leaf^.clFirstLeafBrush) 0 (fromIntegral $ leaf^.clNumLeafBrushes)

  where traceLine :: Int -> Int -> Int -> Quake ()
        traceLine firstLeafBrush idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just brushNum <- liftM (fmap fromIntegral) (preuse $ cmGlobals.cmMapLeafBrushes.ix (firstLeafBrush + idx))
              Just brush <- preuse $ cmGlobals.cmMapBrushes.ix brushNum

              checkCount <- use $ cmGlobals.cmCheckCount
              if (brush^.cbCheckCount) == checkCount
                     -- already checked this brush in another leaf
                then
                  traceLine firstLeafBrush (idx + 1) maxIdx
                else do
                  cmGlobals.cmMapBrushes.ix brushNum.cbCheckCount .= checkCount

                  traceContents <- use $ cmGlobals.cmTraceContents
                  if (brush^.cbContents) .&. traceContents == 0
                    then
                      traceLine firstLeafBrush (idx + 1) maxIdx
                    else do
                      traceMins <- use $ cmGlobals.cmTraceMins
                      traceMaxs <- use $ cmGlobals.cmTraceMaxs
                      traceStart <- use $ cmGlobals.cmTraceStart

                      testBoxInBrush traceMins traceMaxs traceStart (cmGlobals.cmTraceTrace) brush

                      fraction <- use $ cmGlobals.cmTraceTrace.tFraction

                      unless (fraction == 0) $
                        traceLine firstLeafBrush (idx + 1) maxIdx

recursiveHullCheck :: Int -> Float -> Float -> V3 Float -> V3 Float -> Quake ()
recursiveHullCheck num p1f p2f p1 p2 = do
    traceFraction <- use $ cmGlobals.cmTraceTrace.tFraction

    -- do nothing if we already hit something nearer
    unless (traceFraction <= p1f) $ do

      -- if < 0, we are in a leaf node
      if num < 0
        then
          traceToLeaf ((-1) - num)
        else do
          -- find the point distances to the separating plane
          -- and the offset for the size of the box
          Just node <- preuse $ cmGlobals.cmMapNodes.ix num
          let Just planeIdx = node^.cnPlane
          Just plane <- preuse $ cmGlobals.cmMapPlanes.ix planeIdx

          (t1, t2, offset) <- findDistancesAndOffset plane

          -- see which sides we need to consider
          if | t1 >= offset && t2 >= offset -> recursiveHullCheck (node^.cnChildren._1) p1f p2f p1 p2
             | t1 < (-offset) && t2 < (-offset) -> recursiveHullCheck (node^.cnChildren._2) p1f p2f p1 p2
             | otherwise -> do
                 -- put the crosspoint DIST_EPSILON pixels on the near side
                 let (side, tmpFrac, tmpFrac2) = if | t1 < t2 -> let idist' = 1 / (t1 - t2)
                                                                     side' :: Int = 1
                                                                     frac2' = (t1 + offset + distEpsilon) * idist'
                                                                     frac' = (t1 - offset + distEpsilon) * idist'
                                                                 in (side', frac', frac2')
                                                    | t1 > t2 -> let idist' = 1 / (t1 - t2)
                                                                     side' :: Int = 0
                                                                     frac2' = (t1 - offset - distEpsilon) * idist'
                                                                     frac' = (t1 + offset + distEpsilon) * idist'
                                                                 in (side', frac', frac2')
                                                    | otherwise -> (0, 1, 0)

                     -- move up to the node
                     frac = if | tmpFrac < 0 -> 0
                               | tmpFrac > 1 -> 1
                               | otherwise -> tmpFrac

                 moveUpTheNode side frac node

                 -- go past the node
                 let frac2 = if | tmpFrac2 < 0 -> 0
                                | tmpFrac2 > 1 -> 1
                                | otherwise -> tmpFrac2

                 goPastTheNode side frac2 node

  where findDistancesAndOffset :: CPlaneT -> Quake (Float, Float, Float)
        findDistancesAndOffset plane = do
          let pType :: Int = fromIntegral $ plane^.cpType
          traceIsPoint <- use $ cmGlobals.cmTraceIsPoint
          traceExtents <- use $ cmGlobals.cmTraceExtents

          if pType < 3
            then do
              let t1 = p1^.(Math3D.v3Access pType) - (plane^.cpDist)
                  t2 = p2^.(Math3D.v3Access pType) - (plane^.cpDist)
                  offset = traceExtents^.(Math3D.v3Access pType)
              return (t1, t2, offset)
            else do
              let t1 = dot (plane^.cpNormal) p1 - (plane^.cpDist)
                  t2 = dot (plane^.cpNormal) p2 - (plane^.cpDist)
                  offset = if traceIsPoint
                             then 0
                             else dot (fmap abs traceExtents) (fmap abs (plane^.cpNormal))
              return (t1, t2, offset)

        moveUpTheNode :: Int -> Float -> CNodeT -> Quake ()
        moveUpTheNode side frac node = do
          let midf = p1f + (p2f - p1f) * frac
              mid = p1 + fmap (* frac) (p2 - p1)

          recursiveHullCheck (node^.cnChildren.(if side == 0 then _1 else _2)) p1f midf p1 mid

        goPastTheNode :: Int -> Float -> CNodeT -> Quake ()
        goPastTheNode side frac2 node = do
          let midf = p1f + (p2f - p1f) * frac2
              mid = p1 + fmap (* frac2) (p2 - p1)

          recursiveHullCheck (node^.cnChildren.(if side == 0 then _2 else _1)) midf p2f mid p2

traceToLeaf :: Int -> Quake ()
traceToLeaf leafNum = do
    Just leaf <- preuse $ cmGlobals.cmMapLeafs.ix leafNum
    traceContents <- use $ cmGlobals.cmTraceContents

    unless ((leaf^.clContents) .&. traceContents == 0) $ do
      -- trace line against all brushes in the leaf
      traceLineAgainstAllBrushes (fromIntegral $ leaf^.clFirstLeafBrush) 0 (fromIntegral $ leaf^.clNumLeafBrushes)

  where traceLineAgainstAllBrushes :: Int -> Int -> Int -> Quake ()
        traceLineAgainstAllBrushes firstLeafBrush idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just brushNum <- liftM (fmap fromIntegral) (preuse $ cmGlobals.cmMapLeafBrushes.ix (firstLeafBrush + idx))
              Just b <- preuse $ cmGlobals.cmMapBrushes.ix brushNum
              checkCount <- use $ cmGlobals.cmCheckCount

              if (b^.cbCheckCount) == checkCount
                then -- already checked this brush in another leaf
                  traceLineAgainstAllBrushes firstLeafBrush (idx + 1) maxIdx
                else do
                  cmGlobals.cmMapBrushes.ix brushNum.cbCheckCount .= checkCount
                  traceContents <- use $ cmGlobals.cmTraceContents

                  if (b^.cbContents) .&. traceContents == 0
                    then traceLineAgainstAllBrushes firstLeafBrush (idx + 1) maxIdx
                    else do
                      traceMins <- use $ cmGlobals.cmTraceMins
                      traceMaxs <- use $ cmGlobals.cmTraceMaxs
                      traceStart <- use $ cmGlobals.cmTraceStart
                      traceEnd <- use $ cmGlobals.cmTraceEnd

                      clipBoxToBrush traceMins traceMaxs traceStart traceEnd (cmGlobals.cmTraceTrace) b
                      traceTraceFraction <- use $ cmGlobals.cmTraceTrace.tFraction

                      unless (traceTraceFraction == 0) $
                        traceLineAgainstAllBrushes firstLeafBrush (idx + 1) maxIdx

clipBoxToBrush :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Lens' QuakeState TraceT -> CBrushT -> Quake ()
clipBoxToBrush mins maxs p1 p2 traceLens brush = do
    unless ((brush^.cbNumSides) == 0) $ do
      globals.cBrushTraces += 1

      (done, enterFrac, leaveFrac, clipPlane, getOut, startOut, leadSide) <- findIntersections (-1) 1 Nothing False False Nothing 0 (brush^.cbNumSides)

      unless done $ do
        if not startOut -- origin point was inside brush
          then do
            traceLens.tStartSolid .= True
            unless getOut $ traceLens.tAllSolid .= True
          else
            when (enterFrac < leaveFrac) $ do
              traceT <- use $ traceLens

              when (enterFrac > (-1) && enterFrac < (traceT^.tFraction)) $ do
                Just plane <- preuse $ cmGlobals.cmMapPlanes.ix (fromJust $ clipPlane)
                Just brushSide <- preuse $ cmGlobals.cmMapBrushSides.ix (fromJust $ leadSide)
                let Just surfaceIdx = brushSide^.cbsSurface
                surface <- case brushSide^.cbsSurface of
                             Nothing -> return nullSurface
                             Just surfaceIdx -> do
                               Just surface <- preuse $ cmGlobals.cmMapSurfaces.ix surfaceIdx
                               return surface

                traceLens.tFraction .= if enterFrac < 0 then 0 else enterFrac
                traceLens.tPlane .= plane
                traceLens.tSurface .= Just (surface^.msCSurface) -- TODO: this might be an issue! maybe hold the reference to mapSurfaceT ?
                traceLens.tContents .= (brush^.cbContents)
        
        -- 3rd argument is index of cmGlobals.cmMapPlanes
        -- 6th argument is index of cmGlobals.cmMapBrushSides
  where findIntersections :: Float -> Float -> Maybe Int -> Bool -> Bool -> Maybe Int -> Int -> Int -> Quake (Bool, Float, Float, Maybe Int, Bool, Bool, Maybe Int)
        findIntersections enterFrac leaveFrac clipPlane getOut startOut leadSide idx maxIdx
          | idx >= maxIdx = return (False, enterFrac, leaveFrac, clipPlane, getOut, startOut, leadSide)
          | otherwise = do
              Just side <- preuse $ cmGlobals.cmMapBrushSides.ix ((brush^.cbFirstBrushSide) + idx)
              Just plane <- preuse $ cmGlobals.cmMapPlanes.ix (fromJust $ side^.cbsPlane)

              -- FIXME: special case for axial
              traceIsPoint <- use $ cmGlobals.cmTraceIsPoint

              let dist = if traceIsPoint
                           -- special point case
                           then plane^.cpDist
                           -- general box case
                           else
                             -- push the plane out apropriately for mins/maxs
                             -- FIXME: use signbits into 8 way lookup for each mins/maxs
                             let a = if (plane^.cpNormal._x) < 0 then maxs^._x else mins^._x
                                 b = if (plane^.cpNormal._y) < 0 then maxs^._y else mins^._y
                                 c = if (plane^.cpNormal._z) < 0 then maxs^._z else mins^._z
                                 ofs = V3 a b c
                                 distance = dot ofs (plane^.cpNormal)
                             in plane^.cpDist - distance

                  d1 = (dot p1 (plane^.cpNormal)) - dist
                  d2 = (dot p2 (plane^.cpNormal)) - dist
                  getOut' = if d2 > 0
                              then True -- endpoint is not in solid
                              else getOut
                  startOut' = if d1 > 0
                                then True
                                else startOut

              if | d1 > 0 && d2 >= d1 -> return (True, enterFrac, leaveFrac, clipPlane, getOut', startOut', leadSide) -- completely in front of face, no intersection
                 | d1 <= 0 && d2 <= 0 -> findIntersections enterFrac leaveFrac clipPlane getOut' startOut' leadSide (idx + 1) maxIdx
                 | d1 > d2 -> do -- crosses face
                     let f = (d1 - distEpsilon) / (d1 - d2)
                         enterFrac' = if f > enterFrac then f else enterFrac
                         clipPlane' = if f > enterFrac then side^.cbsPlane else clipPlane
                         leadSide' = if f > enterFrac then Just ((brush^.cbFirstBrushSide) + idx) else leadSide
                     findIntersections enterFrac' leaveFrac clipPlane' getOut' startOut' leadSide' (idx + 1) maxIdx
                 | otherwise -> do
                     let f = (d1 + distEpsilon) / (d1 - d2)
                         leaveFrac' = if f < leaveFrac then f else leaveFrac
                     findIntersections enterFrac leaveFrac' clipPlane getOut' startOut' leadSide (idx + 1) maxIdx

-- To keep everything totally uniform, bounding boxes are turned into small
-- BSP trees instead of being compared directly
headnodeForBox :: V3 Float -> V3 Float -> Quake Int
headnodeForBox mins maxs = do
    numPlanes <- use $ cmGlobals.cmNumPlanes
    boxHeadNode <- use $ cmGlobals.cmBoxHeadNode

    mapPlanes <- use $ cmGlobals.cmMapPlanes
    -- this version is much better from the point of view of memory
    -- consumption than two versions below
    let !a = mapPlanes V.! (numPlanes +  0)
        !b = mapPlanes V.! (numPlanes +  1)
        !c = mapPlanes V.! (numPlanes +  2)
        !d = mapPlanes V.! (numPlanes +  3)
        !e = mapPlanes V.! (numPlanes +  4)
        !f = mapPlanes V.! (numPlanes +  5)
        !g = mapPlanes V.! (numPlanes +  6)
        !h = mapPlanes V.! (numPlanes +  7)
        !i = mapPlanes V.! (numPlanes +  8)
        !j = mapPlanes V.! (numPlanes +  9)
        !k = mapPlanes V.! (numPlanes + 10)
        !l = mapPlanes V.! (numPlanes + 11)

    cmGlobals.cmMapPlanes %= (V.// [ (numPlanes +  0, a { _cpDist = maxs^._x })
                                   , (numPlanes +  1, b { _cpDist = - (maxs^._x) })
                                   , (numPlanes +  2, c { _cpDist = mins^._x })
                                   , (numPlanes +  3, d { _cpDist = - (mins^._x) })
                                   , (numPlanes +  4, e { _cpDist = maxs^._y })
                                   , (numPlanes +  5, f { _cpDist = - (maxs^._y) })
                                   , (numPlanes +  6, g { _cpDist = mins^._y })
                                   , (numPlanes +  7, h { _cpDist = - (mins^._y) })
                                   , (numPlanes +  8, i { _cpDist = maxs^._z })
                                   , (numPlanes +  9, j { _cpDist = - (maxs^._z) })
                                   , (numPlanes + 10, k { _cpDist = mins^._z })
                                   , (numPlanes + 11, l { _cpDist = - (mins^._z) })
                                   ])
    {-
    - this version builds a lot of thunks which reside in memory
    -
    let newDistances = V.fromList [ maxs^._x
                                  , - (maxs^._x)
                                  , mins^._x
                                  , - (mins^._x)
                                  , maxs^._y
                                  , - (maxs^._y)
                                  , mins^._y
                                  , - (mins^._y)
                                  , maxs^._z
                                  , - (maxs^._z)
                                  , mins^._z
                                  , - (mins^._z)
                                  ]
        updates = map (updateDist mapPlanes newDistances numPlanes) [0..11]

    cmGlobals.cmMapPlanes %= (V.// updates)
    -}

{- 
-   this version is simply crazy from the allocation rate point of view
-
    zoom (cmGlobals.cmMapPlanes) $ do
      ix (numPlanes +  0).cpDist .= (maxs^._x)
      ix (numPlanes +  1).cpDist .= (- (maxs^._x))
      ix (numPlanes +  2).cpDist .= (mins^._x)
      ix (numPlanes +  3).cpDist .= (- (mins^._x))
      ix (numPlanes +  4).cpDist .= (maxs^._y)
      ix (numPlanes +  5).cpDist .= (- (maxs^._y))
      ix (numPlanes +  6).cpDist .= (mins^._y)
      ix (numPlanes +  7).cpDist .= (- (mins^._y))
      ix (numPlanes +  8).cpDist .= (maxs^._z)
      ix (numPlanes +  9).cpDist .= (- (maxs^._z))
      ix (numPlanes + 10).cpDist .= (mins^._z)
      ix (numPlanes + 11).cpDist .= (- (mins^._z))
      -}

    return boxHeadNode

{-
  where updateDist :: V.Vector CPlaneT -> V.Vector Float -> Int -> Int -> (Int, CPlaneT)
        updateDist planes distances boxHeadNode idx = (boxHeadNode + idx, (planes V.! (boxHeadNode + idx)) { _cpDist = distances V.! idx })
        -}

-- Searches the leaf number that contains the 3d point
pointLeafNum :: V3 Float -> Quake Int
pointLeafNum p = do
    -- sound may call this without map loaded
    numPlanes <- use $ cmGlobals.cmNumPlanes

    if numPlanes == 0
      then return 0
      else pointLeafNumR p 0

--  Recursively searches the leaf number that contains the 3d point
pointLeafNumR :: V3 Float -> Int -> Quake Int
pointLeafNumR p num = do
    num' <- findNum num
    globals.cPointContents += 1 -- optimize counter

    return $ (-1) - num'

  where findNum :: Int -> Quake Int
        findNum n
          | n >= 0 = do
              Just node <- preuse $ cmGlobals.cmMapNodes.ix n
              let Just planeIdx = node^.cnPlane
              Just plane <- preuse $ cmGlobals.cmMapPlanes.ix planeIdx

              let d = if plane^.cpType < 3
                        then p^.(Math3D.v3Access (fromIntegral $ plane^.cpType)) - (plane^.cpDist)
                        else dot (plane^.cpNormal) p - (plane^.cpDist)

              let n' = if d < 0
                         then node^.cnChildren._2
                         else node^.cnChildren._1

              findNum n'
          | otherwise = return n

clusterPVS :: Int -> Quake B.ByteString
clusterPVS cluster = do
    if cluster == -1
      then do
        numClusters <- use $ cmGlobals.cmNumClusters
        let num = (numClusters + 7) `shiftR` 3
            empty = B.replicate num 0
        return $ empty `B.append` B.replicate (Constants.maxMapLeafs `div` 8 - num) 0
      else do
        let access = if Constants.dvisPvs == 0 then _1 else _2
        Just offset <- preuse $ cmGlobals.cmMapVis.dvBitOfs.ix cluster.(access)
        mapVisibility <- use $ cmGlobals.cmMapVisibility
        res <- decompressVis mapVisibility offset
        return $ res `B.append` B.replicate (Constants.maxMapLeafs `div` 8 - (B.length res)) 0

-- IMPROVE: 99% the same with clusterPVS, refactor
clusterPHS :: Int -> Quake B.ByteString
clusterPHS cluster = do
    if cluster == -1
      then do
        numClusters <- use $ cmGlobals.cmNumClusters
        let num = (numClusters + 7) `shiftR` 3
            empty = B.replicate num 0
        return $ empty `B.append` B.replicate (Constants.maxMapLeafs `div` 8 - num) 0
      else do
        let access = if Constants.dvisPhs == 0 then _1 else _2
        Just offset <- preuse $ cmGlobals.cmMapVis.dvBitOfs.ix cluster.(access)
        mapVisibility <- use $ cmGlobals.cmMapVisibility
        res <- decompressVis mapVisibility offset
        return $ res `B.append` B.replicate (Constants.maxMapLeafs `div` 8 - (B.length res)) 0

decompressVis :: BL.ByteString -> Int -> Quake B.ByteString
decompressVis mapVisibility offset = do
    numClusters <- use $ cmGlobals.cmNumClusters
    numVisibility <- use $ cmGlobals.cmNumVisibility

    let row = (numClusters + 7) `shiftR` 3

    -- no vis info, so make all visible
    if mapVisibility == "" || numVisibility == 0
      then return $ B.replicate row 0xFF
      else decompress row (fromIntegral offset) 0 ""

        -- IMPROVE: performance? is it needed? are we decompressing often?
  where decompress :: Int -> Int64 -> Int -> B.ByteString -> Quake B.ByteString
        decompress row inp outp acc
          | outp >= row = return acc
          | otherwise = do
              let b = BL.index mapVisibility inp
              if b /= 0
                then decompress row (inp + 1) (outp + 1) (acc `B.snoc` b)
                else do
                  let c = fromIntegral $ BL.index mapVisibility (inp + 1)
                  c' <- if outp + c > row
                          then do
                            Com.dprintf "warning: Vis decompression overrun\n"
                            return $ row - outp
                          else return c

                  decompress row (inp + 2) (outp + c') (acc `B.append` (B.replicate c' 0))

testBoxInBrush :: V3 Float -> V3 Float -> V3 Float -> Lens' QuakeState TraceT -> CBrushT -> Quake ()
testBoxInBrush mins maxs p1 traceLens brush = do
    unless ((brush^.cbNumSides) == 0) $ do
      done <- checkIntersection (brush^.cbFirstBrushSide) 0 (brush^.cbNumSides)

      unless done $ do
        zoom traceLens $ do
          tStartSolid .= True
          tAllSolid .= True
          tFraction .= 0
          tContents .= brush^.cbContents

  where checkIntersection :: Int -> Int -> Int -> Quake Bool
        checkIntersection firstBrushSide idx maxIdx
          | idx >= maxIdx = return False
          | otherwise = do
              Just side <- preuse $ cmGlobals.cmMapBrushSides.ix (firstBrushSide + idx)
              let Just planeIdx = side^.cbsPlane
              Just plane <- preuse $ cmGlobals.cmMapPlanes.ix planeIdx

              -- FIXME: special case for axial
              -- general box case
              -- push the plane out apropriately for mins/maxs
              -- FIXME: use signbits into 8 way lookup for each mins/maxs
              let a = if plane^.cpNormal._x < 0 then maxs^._x else mins^._x
                  b = if plane^.cpNormal._y < 0 then maxs^._y else mins^._y
                  c = if plane^.cpNormal._z < 0 then maxs^._z else mins^._z
                  ofs = V3 a b c
                  dist = (plane^.cpDist) - dot ofs (plane^.cpNormal)
                  d1 = dot p1 (plane^.cpNormal) - dist

              -- if completely in front of face, no intersection
              if d1 > 0
                then return True
                else checkIntersection firstBrushSide (idx + 1) maxIdx

-- Returns a tag that describes the content of the point
pointContents :: V3 Float -> Int -> Quake Int
pointContents p headNode = do
    numNodes <- use $ cmGlobals.cmNumNodes

    if numNodes == 0 -- map not loaded
      then
        return 0
      else do
        idx <- pointLeafNumR p headNode
        Just contents <- preuse $ cmGlobals.cmMapLeafs.ix idx.clContents
        return contents

{-
- ================== CM_TransformedPointContents
- 
- Handles offseting and rotation of the end points for moving and rotating
- entities ==================
-}
transformedPointContents :: V3 Float -> Int -> V3 Float -> V3 Float -> Quake Int
transformedPointContents p headNode origin angles = do
    -- subtract origin offset
    let pL = p - origin

    boxHeadNode <- use $ cmGlobals.cmBoxHeadNode

    -- rotate start and end into the models frame of reference
    let pL' = if headNode /= boxHeadNode && ((angles^._x) /= 0 || (angles^._y) /= 0 || (angles^._z) /= 0)
                then let (Just forward, Just right, Just up) = Math3D.angleVectors angles True True True
                     in V3 (pL `dot` forward) (-(pL `dot` right)) (pL `dot` up)
                else pL

    idx <- pointLeafNumR pL' headNode
    Just contents <- preuse $ cmGlobals.cmMapLeafs.ix idx.clContents
    return contents

{-
- CM_TransformedBoxTrace handles offseting and rotation of the end points
- for moving and rotating entities.
-}
transformedBoxTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> V3 Float -> V3 Float -> Quake TraceT
transformedBoxTrace start end mins maxs headNode brushMask origin angles = do
    -- subtract origin offset
    let startL = start - origin
        endL = end - origin

    -- rotate start and end into the models frame of reference
    boxHeadNode <- use $ cmGlobals.cmBoxHeadNode
    let rotated = if headNode /= boxHeadNode && ((angles^._x) /= 0 || (angles^._y) /= 0 || (angles^._z) /= 0)
                    then True
                    else False

        (startL', endL') = if rotated
                             then let (Just forward, Just right, Just up) = Math3D.angleVectors angles True True True
                                      s = V3 (startL `dot` forward) (- (startL `dot` right)) (startL `dot` up)
                                      e = V3 (endL `dot` forward) (- (endL `dot` right)) (endL `dot` up)
                                  in (s, e)
                             else (startL, endL)

    -- sweep the box through the model
    traceT <- boxTrace startL' endL' mins maxs headNode brushMask

    let traceT' = if rotated && (traceT^.tFraction) /= 1
                    then let (Just forward, Just right, Just up) = Math3D.angleVectors angles True True True
                             temp = traceT^.tPlane.cpNormal
                         in traceT { _tPlane = (traceT^.tPlane) { _cpNormal = V3 (temp `dot` forward) (- (temp `dot` right)) (temp `dot` up) } }
                    else traceT
        endPos = start + fmap (* (traceT^.tFraction)) (end - start)

    return $ traceT' { _tEndPos = endPos }

{-
- CM_WriteAreaBits writes a length byte followed by a bit vector of all the areas that area
- in the same flood as the area parameter
- 
- This is used by the client refreshes to cull visibility.
-}
writeAreaBits :: Traversal' QuakeState (VS.Vector Word8) -> Int -> Quake Int
writeAreaBits bufferLens area = do
    numAreas <- use $ cmGlobals.cmNumAreas

    let bytes = (numAreas + 7) `shiftR` 3

    noAreasValue <- liftM (^.cvValue) mapNoAreasCVar
    Just buffer <- preuse bufferLens

    if noAreasValue /= 0
      then
        -- for debugging, send everything
        bufferLens .= (VS.replicate bytes 255) VS.++ (VS.drop bytes buffer)
      else do
        let buffer' = (VS.replicate bytes 0) VS.++ (VS.drop bytes buffer)
        mapAreas <- use $ cmGlobals.cmMapAreas
        let floodNum = (mapAreas V.! area)^.caFloodNum
        bufferLens .= constructAreaBits floodNum mapAreas buffer' 0 numAreas

    return bytes

  where constructAreaBits :: Int -> V.Vector CAreaT -> VS.Vector Word8 -> Int -> Int -> VS.Vector Word8
        constructAreaBits floodNum mapAreas buffer idx maxIdx
          | idx >= maxIdx = buffer
          | otherwise =
              if ((mapAreas V.! idx)^.caFloodNum) == floodNum || area == 0
                then constructAreaBits floodNum mapAreas (buffer VS.// [(idx `shiftR` 3, (buffer VS.! (idx `shiftR` 3)) .|. (1 `shiftL` (fromIntegral idx .&. 7)))]) (idx + 1) maxIdx
                else constructAreaBits floodNum mapAreas buffer (idx + 1) maxIdx

{-
- CM_HeadnodeVisible returns true if any leaf under headnode has a cluster that is potentially
- visible.
-}
headNodeVisible :: Int -> UV.Vector Word8 -> Quake Bool
headNodeVisible nodeNum visbits = do
    if nodeNum < 0
      then do
        mapLeafs <- use $ cmGlobals.cmMapLeafs

        let leafNum = -1 - nodeNum
            cluster = (mapLeafs V.! leafNum)^.clCluster

        if cluster == -1
          then
            return False
          else
            if (visbits UV.! (cluster `shiftR` 3)) .&. (1 `shiftL` (cluster .&. 7)) /= 0
              then return True
              else return False
      else do
        mapNodes <- use $ cmGlobals.cmMapNodes
        let node = mapNodes V.! nodeNum

        v <- headNodeVisible (node^.cnChildren._1) visbits
        if v
          then return True
          else headNodeVisible (node^.cnChildren._2) visbits
