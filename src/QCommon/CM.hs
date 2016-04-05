module QCommon.CM
  ( areasConnected
  , clusterPHS
  , clusterPVS
  , entityString
  , inlineModel
  , leafArea
  , leafCluster
  , loadMap
  , numInlineModels
  , pointLeafNum
  , setAreaPortalState
  ) where

import           QCommon.CAreaT
import           QCommon.CLeafT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FSShared as FS
import           QCommon.LumpT
import           QCommon.TexInfoT
import qualified QCommon.MD4 as MD4
import           QCommon.QFiles.BSP.DAreaPortalT
import           QCommon.QFiles.BSP.DAreaT
import           QCommon.QFiles.BSP.DBrushSideT
import           QCommon.QFiles.BSP.DBrushT
import           QCommon.QFiles.BSP.DHeaderT
import           QCommon.QFiles.BSP.DLeafT
import           QCommon.QFiles.BSP.DModelT
import           QCommon.QFiles.BSP.DNodeT
import           QCommon.QFiles.BSP.DPlaneT
import           QCommon.QFiles.BSP.DVisT
import qualified Constants
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary (encode, getMany)
import qualified Util.Lib as Lib

import           Control.Lens (use, (.=), (%=), (+=), (^.), (&), (.~))
import           Control.Monad (void, unless, when)
import           Data.Binary.Get (runGet, getWord16le)
import           Data.Bits (shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..))
import           System.IO (Handle)

setAreaPortalState :: Int -> Bool -> Quake ()
setAreaPortalState = error "CM.setAreaPortalState" -- TODO

areasConnected :: Int -> Int -> Quake Bool
areasConnected = error "CM.areasConnected" -- TODO

loadMap :: B.ByteString -> Bool -> [Int] -> Quake (Ref CModelT, [Int]) -- return model ref (cmGlobals.cmMapCModels) and checksum
loadMap name clientLoad checksum =
  do Com.dprintf (B.concat ["CM_LoadMap(", name, ")...\n"])
     void (CVar.get "map_noareas" "0" 0)
     mapName <- use (cmGlobals.cmMapName)
     flushMap <- CVar.variableValue "flushmap"
     proceedLoadMap name clientLoad checksum mapName flushMap

proceedLoadMap :: B.ByteString -> Bool -> [Int] -> B.ByteString -> Float -> Quake (Ref CModelT, [Int])
proceedLoadMap name clientLoad checksum mapName flushMap
  | mapName == name && (clientLoad || flushMap == 0) =
      do lastChecksum <- use (cmGlobals.cmLastChecksum)
         unless clientLoad $
           do cmGlobals.cmPortalOpen %= UV.map (const False)
              floodAreaConnections
         return (Ref 0, lastChecksum : tail checksum)
  | B.null name =
      do resetCommonCMGlobals
         cmGlobals.cmNumNodes .= 0
         cmGlobals.cmNumLeafs .= 1
         cmGlobals.cmNumClusters .= 1
         cmGlobals.cmNumAreas .= 1
         return (Ref 0, 0 : tail checksum)
  | otherwise =
      do resetCommonCMGlobals
         cmGlobals.cmNumNodes .= 0
         cmGlobals.cmNumLeafs .= 0
         fileHandle <- FS.fOpenFileWithLength name
         maybe loadMapError (doLoadMap name checksum) fileHandle
  where resetCommonCMGlobals =
          do cmGlobals.cmNumCModels .= 0
             cmGlobals.cmNumVisibility .= 0
             cmGlobals.cmNumEntityChars .= 0
             cmGlobals.cmMapEntityString .= B.empty
             cmGlobals.cmMapName .= B.empty
        loadMapError =
          do Com.comError Constants.errDrop ("Couldn't load " `B.append` name)
             return (Ref 0, 0 : tail checksum)

doLoadMap :: B.ByteString -> [Int] -> (Handle, Int) -> Quake (Ref CModelT, [Int])
doLoadMap name checksum (fileHandle, len) =
  do buf <- request (io (BL.hGet fileHandle len))
     loadBSP name checksum buf len (runGet getDHeaderT buf)

loadBSP :: B.ByteString -> [Int] -> BL.ByteString -> Int -> DHeaderT -> Quake (Ref CModelT, [Int])
loadBSP name checksum buf len header =
  do checkHeader
     cmGlobals.cmLastChecksum .= bufChecksum
     loadSurfaces     buf (lumps V.! Constants.lumpTexInfo)
     loadLeafs        buf (lumps V.! Constants.lumpLeafs)
     loadLeafBrushes  buf (lumps V.! Constants.lumpLeafBrushes)
     loadPlanes       buf (lumps V.! Constants.lumpPlanes)
     loadBrushes      buf (lumps V.! Constants.lumpBrushes)
     loadBrushSides   buf (lumps V.! Constants.lumpBrushSides)
     loadSubmodels    buf (lumps V.! Constants.lumpModels)
     loadNodes        buf (lumps V.! Constants.lumpNodes)
     loadAreas        buf (lumps V.! Constants.lumpAreas)
     loadAreaPortals  buf (lumps V.! Constants.lumpAreaPortals)
     loadVisibility   buf (lumps V.! Constants.lumpVisibility)
     loadEntityString buf (lumps V.! Constants.lumpEntities)
     initBoxHull
     cmGlobals.cmPortalOpen %= UV.map (const False)
     floodAreaConnections
     cmGlobals.cmMapName .= name
     return (Ref 0, updatedChecksum)
  where checkHeader =
          when (header^.dhVersion /= Constants.bspVersion) $
            Com.comError Constants.errDrop (B.concat
              [ "CMod_LoadBrushModel: ", name, " has wrong version number ("
              , encode (header^.dhVersion), " should be ", encode Constants.bspVersion, ")"])
        bufChecksum = MD4.blockChecksum buf (fromIntegral len)
        updatedChecksum = bufChecksum : tail checksum
        lumps = header^.dhLumps

inlineModel :: B.ByteString -> Quake (Ref CModelT)
inlineModel name =
  do checkName
     checkNumCModels =<< numInlineModels
     return (Ref num)
  where checkName
          | B.null name || BC.head name /= '*' =
              Com.comError Constants.errDrop "CM_InlineModel: bad name"
          | otherwise = return ()
        checkNumCModels numCModels
          | num < 1 || num >= numCModels =
              Com.comError Constants.errDrop "CM_InlineModel: bad number"
          | otherwise = return ()
        num = Lib.atoi (B.drop 1 name)

numInlineModels :: Quake Int
numInlineModels = use (cmGlobals.cmNumCModels)

entityString :: Quake B.ByteString
entityString = use (cmGlobals.cmMapEntityString)

floodAreaConnections :: Quake ()
floodAreaConnections =
  do Com.dprintf "FloodAreaConnections...\n"
     cmGlobals.cmFloodValid += 1
     floodValid <- use (cmGlobals.cmFloodValid)
     numAreas <- use (cmGlobals.cmNumAreas)
     areas <- use (cmGlobals.cmMapAreas)
     V.ifoldM_ (flood floodValid) 0 (V.take numAreas areas)

flood :: Int -> Int -> Int -> CAreaT -> Quake Int
flood floodValid floodNum idx area
  | idx == 0 = return floodNum -- area 0 is not used
  | (area^.caFloodValid) == floodValid = return floodNum
  | otherwise =
      do floodAreaR (Ref idx) floodValid (floodNum + 1)
         return (floodNum + 1)

floodAreaR :: Ref CAreaT -> Int -> Int -> Quake ()
floodAreaR areaRef floodValid floodNum = recFlood =<< readRef areaRef
  where recFlood area
          | (area^.caFloodValid) == floodValid =
              when ((area^.caFloodNum) == floodNum) $
                Com.comError Constants.errDrop "FloodArea_r: reflooded"
          | otherwise =
              do modifyRef areaRef (\v -> v & caFloodNum .~ floodNum
                                            & caFloodValid .~ floodValid)
                 portalOpen <- use (cmGlobals.cmPortalOpen)
                 floodPortals area portalOpen 0 (area^.caNumAreaPortals)
        floodPortals area portalOpen idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise =
              do areaPortal <- readRef (Ref ((area^.caFirstAreaPortal) + idx))
                 when (portalOpen UV.! (areaPortal^.dapPortalNum)) $
                   floodAreaR (Ref (areaPortal^.dapOtherArea)) floodValid floodNum
                 floodPortals area portalOpen (idx + 1) maxIdx

pointLeafNum :: V3 Float -> Quake Int
pointLeafNum = error "CM.pointLeafNum" -- TODO

leafArea :: Int -> Quake Int
leafArea leafNum =
  do verifyLeafNum leafNum "CM_LeafArea: bad number"
     fmap (^.clArea) (readRef (Ref leafNum))

verifyLeafNum :: Int -> B.ByteString -> Quake ()
verifyLeafNum leafNum errMsg =
  do numLeafs <- use (cmGlobals.cmNumLeafs)
     when (leafNum < 0 || leafNum >= numLeafs) $
       Com.comError Constants.errDrop errMsg

leafCluster :: Int -> Quake Int
leafCluster leafNum =
  do verifyLeafNum leafNum "CM_LeafCluster: bad number"
     fmap (^.clCluster) (readRef (Ref leafNum))

clusterPHS :: Int -> Quake B.ByteString
clusterPHS = error "CM.clusterPHS" -- TODO

clusterPVS :: Int -> Quake B.ByteString
clusterPVS = error "CM.clusterPVS" -- TODO

loadSurfaces :: BL.ByteString -> LumpT -> Quake ()
loadSurfaces buf lump =
  do Com.dprintf "CMod_LoadSurfaces()\n"
     checkLump
     Com.dprintf (B.concat [" numtexinfo=", encode count, "\n"])
     cmGlobals.cmNumTexInfo .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapSurfaces %= (\v -> V.update v (V.imap (\i t -> (i, toMapSurface t)) readTexInfo))
  where count = (lump^.lFileLen) `div` texInfoTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count < 1) $
               Com.comError Constants.errDrop "Map with no surfaces"
             when (count > Constants.maxMapTexInfo) $
               Com.comError Constants.errDrop "Map has too many surfaces"
        readTexInfo = runGet (V.replicateM count getTexInfoT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toMapSurface :: TexInfoT -> MapSurfaceT
toMapSurface texInfo = MapSurfaceT { _msCSurface = cSurface, _msRName = Just (texInfo^.tiTexture) }
  where cSurface = CSurfaceT { _csName = texInfo^.tiTexture
                             , _csFlags = texInfo^.tiFlags
                             , _csValue = texInfo^.tiValue
                             }

loadLeafs :: BL.ByteString -> LumpT -> Quake ()
loadLeafs buf lump =
  do Com.dprintf "CMod_LoadLeafs()\n"
     checkLump
     Com.dprintf (B.concat [" numleafes=", encode count, "\n"])
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     let dLeafs = readDLeafs
         numClusters = V.foldl' countNumClusters 0 dLeafs
     cmGlobals.cmMapLeafs %= (\v -> V.update v (V.imap (\i d -> (i, toCLeaf d)) dLeafs))
     cmGlobals.cmNumLeafs .= count
     cmGlobals.cmNumClusters .= numClusters
     checkFirstLeaf =<< use (cmGlobals.cmMapLeafs)
     findEmptyLeaf =<< use (cmGlobals.cmMapLeafs)
  where count = (lump^.lFileLen) `div` dLeafTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dLeafTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count < 1) $
               Com.comError Constants.errDrop "Map with no leafs"
             when (count > Constants.maxMapPlanes) $
               Com.comError Constants.errDrop "Map has too many planes"
        readDLeafs = runGet (V.replicateM count getDLeafT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)
        countNumClusters numClusters leaf
          | fromIntegral (leaf^.dlCluster) >= numClusters = fromIntegral (leaf^.dlCluster) + 1
          | otherwise = numClusters
        checkFirstLeaf mapLeafs =
          do when (((V.head mapLeafs)^.clContents) /= Constants.contentsSolid) $ -- TODO: head is kinda safe here but would probably be a better idea to use a more safe approach
               Com.comError Constants.errDrop "Map leaf 0 is not CONTENTS_SOLID"
             cmGlobals.cmSolidLeaf .= 0
        findEmptyLeaf mapLeafs =
          case V.findIndex (\leaf -> leaf^.clContents == 0) mapLeafs of
            Nothing -> Com.comError Constants.errDrop "Map does not have an empty leaf"
            Just idx -> cmGlobals.cmEmptyLeaf .= idx

toCLeaf :: DLeafT -> CLeafT
toCLeaf dLeaf = CLeafT { _clContents       = dLeaf^.dlContents
                       , _clCluster        = fromIntegral (dLeaf^.dlCluster)
                       , _clArea           = fromIntegral (dLeaf^.dlArea)
                       , _clFirstLeafBrush = dLeaf^.dlFirstLeafBrush
                       , _clNumLeafBrushes = dLeaf^.dlNumLeafBrushes
                       }

loadLeafBrushes :: BL.ByteString -> LumpT -> Quake ()
loadLeafBrushes buf lump =
  do Com.dprintf "CMod_LoadLeafBrushes()\n"
     checkLump
     Com.dprintf (B.concat [" numbrushes=", encode count, "\n"])
     cmGlobals.cmNumLeafBrushes .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapLeafBrushes %= (\v -> UV.update v (UV.imap (\i b -> (i, b)) readMapLeafBrushes))
  where count = (lump^.lFileLen) `div` 2
        checkLump =
          do when ((lump^.lFileLen) `mod` 2 /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count < 1) $
               Com.comError Constants.errDrop "Map with no planes"
             when (count > Constants.maxMapLeafBrushes) $
               Com.comError Constants.errDrop "Map has too many leafbrushes"
        readMapLeafBrushes = runGet (UV.replicateM count getWord16le) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

loadPlanes :: BL.ByteString -> LumpT -> Quake ()
loadPlanes buf lump =
  do Com.dprintf "CMod_LoadPlanes()\n"
     checkLump
     Com.dprintf (B.concat [" numplanes=", encode count, "\n"])
     cmGlobals.cmNumPlanes .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapPlanes %= (\v -> V.update v (V.imap (\i p -> (i, toCPlane p)) readDPlanes))
  where count = (lump^.lFileLen) `div` dPlaneTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dPlaneTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count < 1) $
               Com.comError Constants.errDrop "Map with no planes"
             when (count > Constants.maxMapPlanes) $
               Com.comError Constants.errDrop "Map has too many planes"
        readDPlanes = runGet (V.replicateM count getDPlaneT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCPlane :: DPlaneT -> CPlaneT
toCPlane dPlane = CPlaneT { _cpNormal   = dPlane^.dpNormal
                          , _cpDist     = dPlane^.dpDist
                          , _cpType     = fromIntegral (dPlane^.dpType)
                          , _cpSignBits = getBits (dPlane^.dpNormal)
                          , _cpPad      = (0, 0)
                          }
  where getBits (V3 a b c) =
          let a' = if a < 0 then 1 else 0
              b' = if b < 0 then 2 else 0
              c' = if c < 0 then 4 else 0
          in a' .|. b' .|. c'

loadBrushes :: BL.ByteString -> LumpT -> Quake ()
loadBrushes buf lump =
  do Com.dprintf "CMod_LoadBrushes()\n"
     checkLump
     Com.dprintf (B.concat [" numbrushes=", encode count, "\n"])
     cmGlobals.cmNumBrushes .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapBrushes %= (\v -> V.update v (V.imap (\i b -> (i, toCBrush b)) readDBrushes))
  where count = (lump^.lFileLen) `div` dBrushTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dBrushTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count > Constants.maxMapBrushes) $
               Com.comError Constants.errDrop "Map has too many brushes"
        readDBrushes = runGet (V.replicateM count getDBrushT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCBrush :: DBrushT -> CBrushT
toCBrush dBrush = CBrushT { _cbContents       = dBrush^.dbContents
                          , _cbNumSides       = dBrush^.dbNumSides
                          , _cbFirstBrushSide = dBrush^.dbFirstSide
                          , _cbCheckCount     = 0
                          }

loadBrushSides :: BL.ByteString -> LumpT -> Quake ()
loadBrushSides buf lump =
  do Com.dprintf "CMod_LoadBrushSides()\n"
     checkLump
     cmGlobals.cmNumBrushSides .= count
     Com.dprintf (B.concat [" numbrushsides=", encode count, "\n"])
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     let brushSides = readDBrushSides
     validateBrushSides brushSides =<< use (cmGlobals.cmNumTexInfo)
     cmGlobals.cmMapBrushSides %= (\v -> V.update v (V.imap (\i b -> (i, toCBrushSide b)) brushSides))
  where count = (lump^.lFileLen) `div` dBrushSideTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dBrushSideTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count > Constants.maxMapBrushSides) $
               Com.comError Constants.errDrop "Map has too many planes"
        readDBrushSides = runGet (V.replicateM count getDBrushSideT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)
        validateBrushSides brushSides numTexInfo =
          mapM_ (checkBrushSide numTexInfo) brushSides
        checkBrushSide numTexInfo brushSide
          | fromIntegral (brushSide^.dbsTexInfo) >= numTexInfo =
              Com.comError Constants.errDrop "Bad brushside texinfo"
          | otherwise = return ()

toCBrushSide :: DBrushSideT -> CBrushSideT
toCBrushSide dBrushSide =
  CBrushSideT { _cbsPlane   = Just planeRef
              , _cbsSurface = Just surfaceRef
              }
  where j = fromIntegral (dBrushSide^.dbsTexInfo)
        planeRef = Ref (fromIntegral (dBrushSide^.dbsPlaneNum))
        surfaceRef | j == -1 = Ref Constants.maxMapTexInfo
                   | otherwise = Ref j

loadSubmodels :: BL.ByteString -> LumpT -> Quake ()
loadSubmodels buf lump =
  do Com.dprintf "CMod_LoadSubmodels()\n"
     checkLump
     Com.dprintf (B.concat [" numcmodels=", encode count, "\n"])
     cmGlobals.cmNumCModels .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapCModels %= (\v -> V.update v (V.imap (\i m -> (i, toCModel m)) readDModels))
  where count = (lump^.lFileLen) `div` dModelTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dModelTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count < 1) $
               Com.comError Constants.errDrop "Map with no models"
             when (count > Constants.maxMapModels) $
               Com.comError Constants.errDrop "Map has too many models"
        readDModels = runGet (V.replicateM count getDModelT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCModel :: DModelT -> CModelT
toCModel dModel = CModelT { _cmMins     = fmap (\a -> a - 1) (dModel^.dmMins)
                          , _cmMaxs     = fmap (+1) (dModel^.dmMaxs)
                          , _cmOrigin   = dModel^.dmOrigin
                          , _cmHeadNode = dModel^.dmHeadNode
                          }

loadNodes :: BL.ByteString -> LumpT -> Quake ()
loadNodes buf lump =
  do Com.dprintf "CMod_LoadNodes()\n"
     checkLump
     Com.dprintf (B.concat [" numnodes=", encode count, "\n"])
     cmGlobals.cmNumNodes .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapNodes %= (\v -> V.update v (V.imap (\i n -> (i, toCNode n)) readDNodes))
  where count = (lump^.lFileLen) `div` dNodeTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dNodeTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count < 1) $
               Com.comError Constants.errDrop "Map with no nodes"
             when (count > Constants.maxMapNodes) $
               Com.comError Constants.errDrop "Map has too many nodes"
        readDNodes = runGet (V.replicateM count getDNodeT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCNode :: DNodeT -> CNodeT
toCNode dNode = CNodeT { _cnPlane    = Just (Ref (dNode^.dnPlaneNum))
                       , _cnChildren = dNode^.dnChildren
                       }

loadAreas :: BL.ByteString -> LumpT -> Quake ()
loadAreas buf lump =
  do Com.dprintf "CMod_LoadAreas()\n"
     checkLump
     Com.dprintf (B.concat [" numareas=", encode count, "\n"])
     cmGlobals.cmNumAreas .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapAreas %= (\v -> V.update v (V.imap (\i a -> (i, toCArea a)) readDAreas))
  where count = (lump^.lFileLen) `div` dAreaTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dAreaTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count > Constants.maxMapAreas) $
               Com.comError Constants.errDrop "Map has too many areas"
        readDAreas = runGet (V.replicateM count getDAreaT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCArea :: DAreaT -> CAreaT
toCArea dArea = CAreaT { _caNumAreaPortals  = dArea^.daNumAreaPortals
                       , _caFirstAreaPortal = dArea^.daFirstAreaPortal
                       , _caFloodNum        = 0
                       , _caFloodValid      = 0
                       }

loadAreaPortals :: BL.ByteString -> LumpT -> Quake ()
loadAreaPortals buf lump =
  do Com.dprintf "CMod_LoadAreaPortals()\n"
     checkLump
     Com.dprintf (B.concat [" numareaportals=", encode count, "\n"])
     cmGlobals.cmNumAreaPortals .= count
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     cmGlobals.cmMapAreaPortals %= (\v -> V.update v (V.imap (\i ap -> (i, ap)) readDAreaPortals))
  where count = (lump^.lFileLen) `div` dAreaPortalTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` dAreaPortalTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count > Constants.maxMapAreas) $
               Com.comError Constants.errDrop "Map has too many areas"
        readDAreaPortals = runGet (V.replicateM count getDAreaPortalT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

loadVisibility :: BL.ByteString -> LumpT -> Quake ()
loadVisibility buf lump =
  do Com.dprintf "CMod_LoadVisibility()\n"
     checkLump
     cmGlobals.cmNumVisibility .= lump^.lFileLen
     Com.dprintf (B.concat [" numvisibility=", encode (lump^.lFileLen), "\n"])
     cmGlobals.cmMapVisibility .= BL.toStrict visData
     cmGlobals.cmMapVis .= runGet getDVisT visData
  where checkLump =
          when ((lump^.lFileLen) > Constants.maxMapVisibility) $
            Com.comError Constants.errDrop "Map has too large visibility lump"
        visData = BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

loadEntityString :: BL.ByteString -> LumpT -> Quake ()
loadEntityString buf lump =
  do Com.dprintf "CMod_LoadEntityString()\n"
     checkLump
     cmGlobals.cmNumEntityChars .= (lump^.lFileLen)
     cmGlobals.cmMapEntityString .= BL.toStrict str
     Com.dprintf (B.concat [ "entitystring=", encode (BL.length str)
                           , " bytes, [", BL.toStrict (BL.take 100 str), "\n"])
  where checkLump =
          when ((lump^.lFileLen) > Constants.maxMapEntString) $
            Com.comError Constants.errDrop "Map has too large entity lump"
        str = BL.takeWhile (/= 0) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

initBoxHull :: Quake ()
initBoxHull =
  do numNodes <- use (cmGlobals.cmNumNodes)
     numBrushes <- use (cmGlobals.cmNumBrushes)
     numLeafBrushes <- use (cmGlobals.cmNumLeafBrushes)
     numBrushSides <- use (cmGlobals.cmNumBrushSides)
     numPlanes <- use (cmGlobals.cmNumPlanes)
     numLeafs <- use (cmGlobals.cmNumLeafs)
     verifyModel numNodes numBrushes numLeafBrushes numBrushSides numPlanes
     cmGlobals.cmBoxHeadNode .= numNodes
     writeRef (Ref numBrushes) (boxBrush numBrushSides)
     writeRef (Ref numLeafs) (boxLeaf numLeafBrushes)
     cmGlobals.cmMapLeafBrushes %= (UV.// [(numLeafBrushes, fromIntegral numBrushes)])
     mapM_ (setBrushSidesNodesAndPlanes numBrushSides numPlanes numLeafs) [0..5]
  where boxBrush numBrushSides =
          CBrushT { _cbContents       = Constants.contentsMonster
                  , _cbNumSides       = 6
                  , _cbFirstBrushSide = numBrushSides
                  , _cbCheckCount     = 0
                  }
        boxLeaf numLeafBrushes =
          CLeafT { _clContents       = Constants.contentsMonster
                 , _clCluster        = 0
                 , _clArea           = 0
                 , _clFirstLeafBrush = fromIntegral numLeafBrushes
                 , _clNumLeafBrushes = 1
                 }

verifyModel :: Int -> Int -> Int -> Int -> Int -> Quake ()
verifyModel numNodes numBrushes numLeafBrushes numBrushSides numPlanes =
  when ( numNodes + 6 > Constants.maxMapNodes
      || numBrushes + 1 > Constants.maxMapBrushes
      || numLeafBrushes + 1 > Constants.maxMapLeafBrushes
      || numBrushSides + 6 > Constants.maxMapBrushSides
      || numPlanes + 12 > Constants.maxMapPlanes
      ) $
    Com.comError Constants.errDrop "Not enough room for box tree"

setBrushSidesNodesAndPlanes :: Int -> Int -> Int -> Int -> Quake ()
setBrushSidesNodesAndPlanes numBrushSides numPlanes numLeafs idx =
  do writeRef (Ref (numBrushSides + idx)) s
     emptyLeaf <- use (cmGlobals.cmEmptyLeaf)
     boxHeadNode <- use (cmGlobals.cmBoxHeadNode)
     writeRef (Ref (boxHeadNode + idx)) (buildNode emptyLeaf boxHeadNode)
     writeRef (Ref (numPlanes + idx * 2)) p1
     writeRef (Ref (numPlanes + idx * 2 + 1)) p2
  where side = idx .&. 1
        s = CBrushSideT { _cbsPlane   = Just (Ref (numPlanes + idx * 2 + side))
                        , _cbsSurface = Nothing
                        }
        buildNode emptyLeaf boxHeadNode =
          CNodeT { _cnPlane    = Just (Ref (numPlanes + idx * 2))
                 , _cnChildren = calcChildren idx side numLeafs emptyLeaf boxHeadNode
                 }
        calcChildren idx side numLeafs emptyLeaf boxHeadNode =
          let a = (-1) - emptyLeaf
              b = if idx == 5 then (-1) - numLeafs else boxHeadNode + idx + 1
          in if side == 0 then (a, b) else (b, a)
        p1 = CPlaneT { _cpNormal   = getNormal idx 1
                     , _cpDist     = 0
                     , _cpType     = fromIntegral (idx `shiftR` 1)
                     , _cpSignBits = 0
                     , _cpPad      = (0, 0)
                     }
        p2 = CPlaneT { _cpNormal   = getNormal idx (-1)
                     , _cpDist     = 0
                     , _cpType     = fromIntegral (3 + (idx `shiftR` 1))
                     , _cpSignBits = 0
                     , _cpPad      = (0, 0)
                     }
        getNormal idx v
          | x == 0 = V3 v 0 0
          | x == 1 = V3 0 v 0
          | otherwise = V3 0 0 v
          where x = idx `shiftR` 1