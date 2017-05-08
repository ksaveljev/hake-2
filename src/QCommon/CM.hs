{-# LANGUAGE Rank2Types #-}
module QCommon.CM
    ( areasConnected
    , boxLeafNums
    , boxTrace
    , clusterPHS
    , clusterPVS
    , entityString
    , headnodeForBox
    , headNodeVisible
    , inlineModel
    , leafArea
    , leafCluster
    , loadMap
    , numInlineModels
    , pointContents
    , pointLeafNum
    , setAreaPortalState
    , transformedBoxTrace
    , transformedPointContents
    , writeAreaBits
    , writePortalState
    ) where

import           Control.Lens                    (Lens', Traversal', use, preuse, ix, (.=), (%=), (+=), (^.), (&), (.~), _1, _2)
import           Control.Monad                   (void, unless, when)
import           Data.Binary.Get                 (runGet, getWord16le)
import           Data.Bits                       (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.Lazy            as BL
import           Data.Maybe                      (fromJust)
import qualified Data.Vector                     as V
import qualified Data.Vector.Storable            as SV
import qualified Data.Vector.Unboxed             as UV
import           Data.Word                       (Word8)
import           Linear                          (V3(..), dot, _x, _y, _z)
import           System.IO                       (Handle)

import qualified Constants
import           Game.CModelT
import           Game.CPlaneT
import           Game.CVarT
import           Game.MapSurfaceT
import           Game.TraceT
import           QCommon.CAreaT
import           QCommon.CBrushSideT
import           QCommon.CBrushT
import           QCommon.CLeafT
import           QCommon.CNodeT
import qualified QCommon.Com                     as Com
import qualified QCommon.CVar                    as CVar
import           QCommon.CVarVariables
import qualified QCommon.FSShared                as FS
import           QCommon.LumpT
import           QCommon.TexInfoT
import qualified QCommon.MD4                     as MD4
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
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary                     (encode)
import qualified Util.Lib                        as Lib
import qualified Util.Math3D                     as Math3D
import qualified Util.QuakeFile                  as QuakeFile

-- 1/32 epsilon to keep floating point happy
distEpsilon :: Float
distEpsilon = 0.03125

nullV3 :: V3 Float
nullV3 = V3 0 0 0

nullSurface :: MapSurfaceT
nullSurface = newMapSurfaceT

setAreaPortalState :: Int -> Bool -> Quake ()
setAreaPortalState portalNum open = do
    numAreaPortals <- use (cmGlobals.cmNumAreaPortals)
    when (portalNum > numAreaPortals) $
        Com.comError Constants.errDrop "areaportal > numareaportals"
    cmGlobals.cmPortalOpen.ix portalNum .= open
    floodAreaConnections

areasConnected :: Int -> Int -> Quake Bool
areasConnected area1 area2 = do
    noAreas <- fmap (^.cvValue) mapNoAreasCVar
    checkAreasConnected noAreas
  where
    checkAreasConnected noAreas
        | noAreas /= 0 = return True
        | otherwise = do
            numAreas <- use (cmGlobals.cmNumAreas)
            when (area1 > numAreas || area2 > numAreas) $
                Com.comError Constants.errDrop "area > numareas"
            mapAreas <- use (cmGlobals.cmMapAreas)
            return (((mapAreas V.! area1)^.caFloodNum) == ((mapAreas V.! area2)^.caFloodNum))

loadMap :: B.ByteString -> Bool -> [Int] -> Quake (Ref CModelT, [Int]) -- return model ref (cmGlobals.cmMapCModels) and checksum
loadMap name clientLoad checksum = do
    Com.dprintf (B.concat ["CM_LoadMap(", name, ")...\n"])
    void (CVar.get "map_noareas" "0" 0)
    mapName <- use (cmGlobals.cmMapName)
    flushMap <- CVar.variableValue "flushmap"
    proceedLoadMap name clientLoad checksum mapName flushMap

proceedLoadMap :: B.ByteString -> Bool -> [Int] -> B.ByteString -> Float -> Quake (Ref CModelT, [Int])
proceedLoadMap name clientLoad checksum mapName flushMap
    | mapName == name && (clientLoad || flushMap == 0) = do
        lastChecksum <- use (cmGlobals.cmLastChecksum)
        unless clientLoad $ do
            cmGlobals.cmPortalOpen %= UV.map (const False)
            floodAreaConnections
        return (Ref 0, lastChecksum : tail checksum)
    | B.null name = do
        resetCommonCMGlobals
        cmGlobals.cmNumNodes .= 0
        cmGlobals.cmNumLeafs .= 1
        cmGlobals.cmNumClusters .= 1
        cmGlobals.cmNumAreas .= 1
        return (Ref 0, 0 : tail checksum)
    | otherwise = do
        resetCommonCMGlobals
        cmGlobals.cmNumNodes .= 0
        cmGlobals.cmNumLeafs .= 0
        fileHandle <- FS.fOpenFileWithLength name
        maybe loadMapError (doLoadMap name checksum) fileHandle
  where
    resetCommonCMGlobals = do
        cmGlobals.cmNumCModels .= 0
        cmGlobals.cmNumVisibility .= 0
        cmGlobals.cmNumEntityChars .= 0
        cmGlobals.cmMapEntityString .= B.empty
        cmGlobals.cmMapName .= B.empty
    loadMapError = do
        Com.comError Constants.errDrop ("Couldn't load " `B.append` name)
        return (Ref 0, 0 : tail checksum)

doLoadMap :: B.ByteString -> [Int] -> (Handle, Int) -> Quake (Ref CModelT, [Int])
doLoadMap name checksum (fileHandle, len) = do
    buf <- io (BL.hGet fileHandle len)
    loadBSP name checksum buf len (runGet getDHeaderT buf)

loadBSP :: B.ByteString -> [Int] -> BL.ByteString -> Int -> DHeaderT -> Quake (Ref CModelT, [Int])
loadBSP name checksum buf len header = do
    checkHeader
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
  where
    checkHeader =
        when (header^.dhVersion /= Constants.bspVersion) $
            Com.comError Constants.errDrop (B.concat
                [ "CMod_LoadBrushModel: ", name, " has wrong version number ("
                , encode (header^.dhVersion), " should be ", encode Constants.bspVersion, ")"])
    bufChecksum = MD4.blockChecksum buf (fromIntegral len)
    updatedChecksum = bufChecksum : tail checksum
    lumps = header^.dhLumps

inlineModel :: B.ByteString -> Quake (Ref CModelT)
inlineModel name = do
    checkName
    checkNumCModels =<< numInlineModels
    return (Ref num)
  where
    checkName
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
floodAreaConnections = do
    Com.dprintf "FloodAreaConnections...\n"
    cmGlobals.cmFloodValid += 1
    floodValid <- use (cmGlobals.cmFloodValid)
    numAreas <- use (cmGlobals.cmNumAreas)
    flood floodValid 1 (numAreas - 1) 0
  where
    flood floodValid idx maxIdx floodNum
        | idx >= maxIdx = return ()
        | otherwise = do
            area <- readRef (Ref idx)
            if (area^.caFloodValid) == floodValid
                then
                    flood floodValid (idx + 1) maxIdx floodNum
                else do
                    floodAreaR (Ref idx) floodValid (floodNum + 1)
                    flood floodValid (idx + 1) maxIdx (floodNum + 1)

floodAreaR :: Ref CAreaT -> Int -> Int -> Quake ()
floodAreaR areaRef floodValid floodNum = recFlood =<< readRef areaRef
  where
    recFlood area
        | (area^.caFloodValid) == floodValid =
            unless ((area^.caFloodNum) == floodNum) $
                Com.comError Constants.errDrop "FloodArea_r: reflooded"
        | otherwise = do
            modifyRef areaRef (\v -> v & caFloodNum .~ floodNum
                                       & caFloodValid .~ floodValid)
            portalOpen <- use (cmGlobals.cmPortalOpen)
            floodPortals area portalOpen 0 (area^.caNumAreaPortals)
    floodPortals area portalOpen idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            areaPortal <- readRef (Ref ((area^.caFirstAreaPortal) + idx))
            when (portalOpen UV.! (areaPortal^.dapPortalNum)) $
                floodAreaR (Ref (areaPortal^.dapOtherArea)) floodValid floodNum
            floodPortals area portalOpen (idx + 1) maxIdx

leafArea :: Int -> Quake Int
leafArea leafNum = do
    verifyLeafNum leafNum "CM_LeafArea: bad number"
    fmap (^.clArea) (readRef (Ref leafNum))

verifyLeafNum :: Int -> B.ByteString -> Quake ()
verifyLeafNum leafNum errMsg = do
    numLeafs <- use (cmGlobals.cmNumLeafs)
    when (leafNum < 0 || leafNum >= numLeafs) $
        Com.comError Constants.errDrop errMsg

leafCluster :: Int -> Quake Int
leafCluster leafNum = do
    verifyLeafNum leafNum "CM_LeafCluster: bad number"
    fmap (^.clCluster) (readRef (Ref leafNum))

clusterPHS :: Int -> Quake B.ByteString
clusterPHS cluster
    | cluster == -1 = do
        numClusters <- use (cmGlobals.cmNumClusters)
        let num = (numClusters + 7) `shiftR` 3
            empty = B.replicate num 0
        return (empty `B.append` B.replicate (Constants.maxMapLeafs `div` 8 - num) 0)
    | otherwise = do
        bitOfs <- use (cmGlobals.cmMapVis.dvBitOfs)
        let access = if Constants.dvisPhs == 0 then _1 else _2
            offset = (bitOfs V.! cluster)^.access
        mapVisibility <- use (cmGlobals.cmMapVisibility)
        res <- decompressVis mapVisibility offset
        return (res `B.append` B.replicate (Constants.maxMapLeafs `div` 8 - (B.length res)) 0)

clusterPVS :: Int -> Quake B.ByteString
clusterPVS = clusterPHS -- TODO: verify this

decompressVis :: B.ByteString -> Int -> Quake B.ByteString
decompressVis mapVisibility offset = do
    numClusters <- use (cmGlobals.cmNumClusters)
    numVisibility <- use (cmGlobals.cmNumVisibility)
    let row = (numClusters + 7) `shiftR` 3
    -- no vis info, so make all visible
    if mapVisibility == "" || numVisibility == 0
        then return (B.replicate row 0xFF)
        else decompress row (fromIntegral offset) 0 ""
  where 
    -- IMPROVE: performance? is it needed? are we decompressing often?
    decompress row inp outp acc -- IMPROVE: old implementation, needs refactoring
        | outp >= row = return acc
        | otherwise = do
            let b = B.index mapVisibility inp
            if b /= 0
              then decompress row (inp + 1) (outp + 1) (acc `B.snoc` b)
              else do
                let c = fromIntegral $ B.index mapVisibility (inp + 1)
                c' <- if outp + c > row
                        then do
                          Com.dprintf "warning: Vis decompression overrun\n"
                          return $ row - outp
                        else return c
                decompress row (inp + 2) (outp + c') (acc `B.append` (B.replicate c' 0))

loadSurfaces :: BL.ByteString -> LumpT -> Quake ()
loadSurfaces buf lump = do
    Com.dprintf "CMod_LoadSurfaces()\n"
    checkLump
    Com.dprintf (B.concat [" numtexinfo=", encode count, "\n"])
    cmGlobals.cmNumTexInfo .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    cmGlobals.cmMapSurfaces %= (\v -> V.update v (V.imap (\i t -> (i, toMapSurface t)) readTexInfo))
  where
    count = (lump^.lFileLen) `div` texInfoTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count < 1) $
            Com.comError Constants.errDrop "Map with no surfaces"
        when (count > Constants.maxMapTexInfo) $
            Com.comError Constants.errDrop "Map has too many surfaces"
    readTexInfo = runGet (V.replicateM count getTexInfoT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toMapSurface :: TexInfoT -> MapSurfaceT
toMapSurface texInfo = MapSurfaceT { _msCSurface = cSurface, _msRName = Just (texInfo^.tiTexture) }
  where
    cSurface = CSurfaceT
        { _csName = texInfo^.tiTexture
        , _csFlags = texInfo^.tiFlags
        , _csValue = texInfo^.tiValue
        }

loadLeafs :: BL.ByteString -> LumpT -> Quake ()
loadLeafs buf lump = do
    Com.dprintf "CMod_LoadLeafs()\n"
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
  where
    count = (lump^.lFileLen) `div` dLeafTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dLeafTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count < 1) $
            Com.comError Constants.errDrop "Map with no leafs"
        when (count > Constants.maxMapPlanes) $
            Com.comError Constants.errDrop "Map has too many planes"
    readDLeafs = runGet (V.replicateM count getDLeafT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)
    countNumClusters numClusters leaf
        | fromIntegral (leaf^.dlCluster) >= numClusters = fromIntegral (leaf^.dlCluster) + 1
        | otherwise = numClusters
    checkFirstLeaf mapLeafs = do
        when (((V.head mapLeafs)^.clContents) /= Constants.contentsSolid) $ -- TODO: head is kinda safe here but would probably be a better idea to use a more safe approach
            Com.comError Constants.errDrop "Map leaf 0 is not CONTENTS_SOLID"
        cmGlobals.cmSolidLeaf .= 0
    findEmptyLeaf mapLeafs =
        case V.findIndex (\leaf -> leaf^.clContents == 0) mapLeafs of
            Nothing -> Com.comError Constants.errDrop "Map does not have an empty leaf"
            Just idx -> cmGlobals.cmEmptyLeaf .= idx

toCLeaf :: DLeafT -> CLeafT
toCLeaf dLeaf = CLeafT
    { _clContents       = dLeaf^.dlContents
    , _clCluster        = fromIntegral (dLeaf^.dlCluster)
    , _clArea           = fromIntegral (dLeaf^.dlArea)
    , _clFirstLeafBrush = dLeaf^.dlFirstLeafBrush
    , _clNumLeafBrushes = dLeaf^.dlNumLeafBrushes
    }

loadLeafBrushes :: BL.ByteString -> LumpT -> Quake ()
loadLeafBrushes buf lump = do
    Com.dprintf "CMod_LoadLeafBrushes()\n"
    checkLump
    Com.dprintf (B.concat [" numbrushes=", encode count, "\n"])
    cmGlobals.cmNumLeafBrushes .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    cmGlobals.cmMapLeafBrushes %= (\v -> UV.update v (UV.imap (\i b -> (i, b)) readMapLeafBrushes))
  where
    count = (lump^.lFileLen) `div` 2
    checkLump = do
        when ((lump^.lFileLen) `mod` 2 /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count < 1) $
            Com.comError Constants.errDrop "Map with no planes"
        when (count > Constants.maxMapLeafBrushes) $
            Com.comError Constants.errDrop "Map has too many leafbrushes"
    readMapLeafBrushes = runGet (UV.replicateM count getWord16le) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

loadPlanes :: BL.ByteString -> LumpT -> Quake ()
loadPlanes buf lump = do
    Com.dprintf "CMod_LoadPlanes()\n"
    checkLump
    Com.dprintf (B.concat [" numplanes=", encode count, "\n"])
    cmGlobals.cmNumPlanes .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    V.imapM_ (\idx p -> writeRef (Ref idx) (toCPlane p)) readDPlanes
    --cmGlobals.cmMapPlanes %= (\v -> V.update v (V.imap (\i p -> (i, toCPlane p)) readDPlanes))
  where
    count = (lump^.lFileLen) `div` dPlaneTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dPlaneTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count < 1) $
            Com.comError Constants.errDrop "Map with no planes"
        when (count > Constants.maxMapPlanes) $
            Com.comError Constants.errDrop "Map has too many planes"
    readDPlanes = runGet (V.replicateM count getDPlaneT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCPlane :: DPlaneT -> CPlaneT
toCPlane dPlane = CPlaneT
    { _cpNormal   = dPlane^.dpNormal
    , _cpDist     = dPlane^.dpDist
    , _cpType     = fromIntegral (dPlane^.dpType)
    , _cpSignBits = getBits (dPlane^.dpNormal)
    , _cpPad      = (0, 0)
    }
  where
    getBits (V3 a b c) =
        let a' = if a < 0 then 1 else 0
            b' = if b < 0 then 2 else 0
            c' = if c < 0 then 4 else 0
        in a' .|. b' .|. c'

loadBrushes :: BL.ByteString -> LumpT -> Quake ()
loadBrushes buf lump = do
    Com.dprintf "CMod_LoadBrushes()\n"
    checkLump
    Com.dprintf (B.concat [" numbrushes=", encode count, "\n"])
    cmGlobals.cmNumBrushes .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    cmGlobals.cmMapBrushes %= (\v -> V.update v (V.imap (\i b -> (i, toCBrush b)) readDBrushes))
  where
    count = (lump^.lFileLen) `div` dBrushTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dBrushTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count > Constants.maxMapBrushes) $
            Com.comError Constants.errDrop "Map has too many brushes"
    readDBrushes = runGet (V.replicateM count getDBrushT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCBrush :: DBrushT -> CBrushT
toCBrush dBrush = CBrushT
    { _cbContents       = dBrush^.dbContents
    , _cbNumSides       = dBrush^.dbNumSides
    , _cbFirstBrushSide = dBrush^.dbFirstSide
    , _cbCheckCount     = 0
    }

loadBrushSides :: BL.ByteString -> LumpT -> Quake ()
loadBrushSides buf lump = do
    Com.dprintf "CMod_LoadBrushSides()\n"
    checkLump
    cmGlobals.cmNumBrushSides .= count
    Com.dprintf (B.concat [" numbrushsides=", encode count, "\n"])
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    let brushSides = readDBrushSides
    validateBrushSides brushSides =<< use (cmGlobals.cmNumTexInfo)
    cmGlobals.cmMapBrushSides %= (\v -> V.update v (V.imap (\i b -> (i, toCBrushSide b)) brushSides))
  where
    count = (lump^.lFileLen) `div` dBrushSideTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dBrushSideTSize /= 0) $
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
toCBrushSide dBrushSide = CBrushSideT
    { _cbsPlane   = Just planeRef
    , _cbsSurface = Just surfaceRef
    }
  where
    j = fromIntegral (dBrushSide^.dbsTexInfo)
    planeRef = Ref (fromIntegral (dBrushSide^.dbsPlaneNum))
    surfaceRef | j == -1 = Ref Constants.maxMapTexInfo
               | otherwise = Ref j

loadSubmodels :: BL.ByteString -> LumpT -> Quake ()
loadSubmodels buf lump = do
    Com.dprintf "CMod_LoadSubmodels()\n"
    checkLump
    Com.dprintf (B.concat [" numcmodels=", encode count, "\n"])
    cmGlobals.cmNumCModels .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    cmGlobals.cmMapCModels %= (\v -> V.update v (V.imap (\i m -> (i, toCModel m)) readDModels))
  where
    count = (lump^.lFileLen) `div` dModelTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dModelTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count < 1) $
            Com.comError Constants.errDrop "Map with no models"
        when (count > Constants.maxMapModels) $
            Com.comError Constants.errDrop "Map has too many models"
    readDModels = runGet (V.replicateM count getDModelT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCModel :: DModelT -> CModelT
toCModel dModel = CModelT
    { _cmMins     = fmap (\a -> a - 1) (dModel^.dmMins)
    , _cmMaxs     = fmap (+1) (dModel^.dmMaxs)
    , _cmOrigin   = dModel^.dmOrigin
    , _cmHeadNode = dModel^.dmHeadNode
    }

loadNodes :: BL.ByteString -> LumpT -> Quake ()
loadNodes buf lump = do
    Com.dprintf "CMod_LoadNodes()\n"
    checkLump
    Com.dprintf (B.concat [" numnodes=", encode count, "\n"])
    cmGlobals.cmNumNodes .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    cmGlobals.cmMapNodes %= (\v -> V.update v (V.imap (\i n -> (i, toCNode n)) readDNodes))
  where
    count = (lump^.lFileLen) `div` dNodeTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dNodeTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count < 1) $
            Com.comError Constants.errDrop "Map with no nodes"
        when (count > Constants.maxMapNodes) $
            Com.comError Constants.errDrop "Map has too many nodes"
    readDNodes = runGet (V.replicateM count getDNodeT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCNode :: DNodeT -> CNodeT
toCNode dNode = CNodeT
    { _cnPlane    = Just (Ref (dNode^.dnPlaneNum))
    , _cnChildren = dNode^.dnChildren
    }

loadAreas :: BL.ByteString -> LumpT -> Quake ()
loadAreas buf lump = do
    Com.dprintf "CMod_LoadAreas()\n"
    checkLump
    Com.dprintf (B.concat [" numareas=", encode count, "\n"])
    cmGlobals.cmNumAreas .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    cmGlobals.cmMapAreas %= (\v -> V.update v (V.imap (\i a -> (i, toCArea a)) readDAreas))
  where
    count = (lump^.lFileLen) `div` dAreaTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dAreaTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count > Constants.maxMapAreas) $
            Com.comError Constants.errDrop "Map has too many areas"
    readDAreas = runGet (V.replicateM count getDAreaT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

toCArea :: DAreaT -> CAreaT
toCArea dArea = CAreaT
    { _caNumAreaPortals  = dArea^.daNumAreaPortals
    , _caFirstAreaPortal = dArea^.daFirstAreaPortal
    , _caFloodNum        = 0
    , _caFloodValid      = 0
    }

loadAreaPortals :: BL.ByteString -> LumpT -> Quake ()
loadAreaPortals buf lump = do
    Com.dprintf "CMod_LoadAreaPortals()\n"
    checkLump
    Com.dprintf (B.concat [" numareaportals=", encode count, "\n"])
    cmGlobals.cmNumAreaPortals .= count
    -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
    cmGlobals.cmMapAreaPortals %= (\v -> V.update v (V.imap (\i ap -> (i, ap)) readDAreaPortals))
  where
    count = (lump^.lFileLen) `div` dAreaPortalTSize
    checkLump = do
        when ((lump^.lFileLen) `mod` dAreaPortalTSize /= 0) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
        when (count > Constants.maxMapAreas) $
            Com.comError Constants.errDrop "Map has too many areas"
    readDAreaPortals = runGet (V.replicateM count getDAreaPortalT) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

loadVisibility :: BL.ByteString -> LumpT -> Quake ()
loadVisibility buf lump = do
    Com.dprintf "CMod_LoadVisibility()\n"
    checkLump
    cmGlobals.cmNumVisibility .= lump^.lFileLen
    Com.dprintf (B.concat [" numvisibility=", encode (lump^.lFileLen), "\n"])
    cmGlobals.cmMapVisibility .= BL.toStrict visData
    cmGlobals.cmMapVis .= runGet getDVisT visData
  where
    checkLump =
        when ((lump^.lFileLen) > Constants.maxMapVisibility) $
            Com.comError Constants.errDrop "Map has too large visibility lump"
    visData = BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)

loadEntityString :: BL.ByteString -> LumpT -> Quake ()
loadEntityString buf lump = do
    Com.dprintf "CMod_LoadEntityString()\n"
    checkLump
    cmGlobals.cmNumEntityChars .= (lump^.lFileLen)
    cmGlobals.cmMapEntityString .= BL.toStrict str
    Com.dprintf (B.concat [ "entitystring=", encode (BL.length str)
                          , " bytes, [", BL.toStrict (BL.take 100 str), "\n"])
  where
    checkLump =
        when ((lump^.lFileLen) > Constants.maxMapEntString) $
            Com.comError Constants.errDrop "Map has too large entity lump"
    str = BL.takeWhile (/= 0) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

initBoxHull :: Quake ()
initBoxHull = do
    numNodes <- use (cmGlobals.cmNumNodes)
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
  where
    boxBrush numBrushSides = CBrushT
        { _cbContents       = Constants.contentsMonster
        , _cbNumSides       = 6
        , _cbFirstBrushSide = numBrushSides
        , _cbCheckCount     = 0
        }
    boxLeaf numLeafBrushes = CLeafT
        { _clContents       = Constants.contentsMonster
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
        ) $ Com.comError Constants.errDrop "Not enough room for box tree"

setBrushSidesNodesAndPlanes :: Int -> Int -> Int -> Int -> Quake ()
setBrushSidesNodesAndPlanes numBrushSides numPlanes numLeafs idx = do
    writeRef (Ref (numBrushSides + idx)) s
    emptyLeaf <- use (cmGlobals.cmEmptyLeaf)
    boxHeadNode <- use (cmGlobals.cmBoxHeadNode)
    writeRef (Ref (boxHeadNode + idx)) (buildNode emptyLeaf boxHeadNode)
    writeRef (Ref (numPlanes + idx * 2)) p1
    writeRef (Ref (numPlanes + idx * 2 + 1)) p2
  where
    side = idx .&. 1
    s = CBrushSideT
        { _cbsPlane   = Just (Ref (numPlanes + idx * 2 + side))
        , _cbsSurface = Nothing
        }
    buildNode emptyLeaf boxHeadNode = CNodeT
        { _cnPlane    = Just (Ref (numPlanes + idx * 2))
        , _cnChildren = calcChildren emptyLeaf boxHeadNode
        }
    calcChildren emptyLeaf boxHeadNode =
        let a = (-1) - emptyLeaf
            b = if idx == 5 then (-1) - numLeafs else boxHeadNode + idx + 1
        in if side == 0 then (a, b) else (b, a)
    p1 = CPlaneT
        { _cpNormal   = getNormal 1
        , _cpDist     = 0
        , _cpType     = fromIntegral (idx `shiftR` 1)
        , _cpSignBits = 0
        , _cpPad      = (0, 0)
        }
    p2 = CPlaneT
        { _cpNormal   = getNormal (-1)
        , _cpDist     = 0
        , _cpType     = fromIntegral (3 + (idx `shiftR` 1))
        , _cpSignBits = 0
        , _cpPad      = (0, 0)
        }
    getNormal v
        | x == 0 = V3 v 0 0
        | x == 1 = V3 0 v 0
        | otherwise = V3 0 0 v
      where x = idx `shiftR` 1

boxTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Quake TraceT
boxTrace start end mins maxs headNode brushMask = do
    -- for multi-check avoidance
    cmGlobals.cmCheckCount += 1
    -- for statistics, may be zeroed
    globals.gCTraces += 1
    -- fill in a default trace
    -- was: memset(& trace_trace, 0, sizeof(trace_trace));
    cmGlobals.cmTraceTrace .= (newTraceT & tFraction .~ 1
                                         & tSurface  .~ Just (nullSurface^.msCSurface))
    numNodes <- use (cmGlobals.cmNumNodes)
    doBoxTrace numNodes
  where
    doBoxTrace numNodes
        | numNodes == 0 = -- map not loaded
            use (cmGlobals.cmTraceTrace)
        | otherwise = do
            cmGlobals.cmTraceContents .= brushMask
            cmGlobals.cmTraceStart .= start
            cmGlobals.cmTraceEnd .= end
            cmGlobals.cmTraceMins .= mins
            cmGlobals.cmTraceMaxs .= maxs
            proceedBoxTrace
    proceedBoxTrace
        | start == end = do
            -- check for position test special case
            let c1 = fmap (subtract 1) (start + mins)
                c2 = fmap (+ 1) (start + maxs)
            (numLeafs, _) <- boxLeafNumsHeadnode c1 c2 (cmGlobals.cmLeafs) 1024 headNode (Just [0])
            checkLeafs 0 numLeafs
            cmGlobals.cmTraceTrace.tEndPos .= start
            use (cmGlobals.cmTraceTrace)
        | otherwise = do
            setTracePointAndExtents
            -- general sweeping through world
            recursiveHullCheck headNode 0 1 start end
            traceFraction <- use (cmGlobals.cmTraceTrace.tFraction)
            cmGlobals.cmTraceTrace.tEndPos .= if traceFraction == 1 then end else start + fmap (* traceFraction) (end - start)
            traceTrace <- use (cmGlobals.cmTraceTrace)
            return traceTrace
    setTracePointAndExtents
        | (mins == nullV3) && (maxs == nullV3) = do
            cmGlobals.cmTraceIsPoint .= True
            cmGlobals.cmTraceExtents .= nullV3
        | otherwise = do
            cmGlobals.cmTraceIsPoint .= False
            let a = if (- (mins^._x)) > (maxs^._x) then (- (mins^._x)) else maxs^._x
                b = if (- (mins^._y)) > (maxs^._y) then (- (mins^._y)) else maxs^._y
                c = if (- (mins^._z)) > (maxs^._z) then (- (mins^._z)) else maxs^._z
            cmGlobals.cmTraceExtents .= V3 a b c
    checkLeafs idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            leafs <- use (cmGlobals.cmLeafs)
            testInLeaf (leafs UV.! idx)
            allSolid <- use (cmGlobals.cmTraceTrace.tAllSolid)
            unless allSolid $
                checkLeafs (idx + 1) maxIdx

testInLeaf :: Int -> Quake ()
testInLeaf leafNum = do
    leaf <- fmap (V.! leafNum) (use (cmGlobals.cmMapLeafs))
    traceContents <- use (cmGlobals.cmTraceContents)
    when ((leaf^.clContents) .&. traceContents /= 0) $
        -- trace line against all brushes in the leaf
        traceLine (fromIntegral (leaf^.clFirstLeafBrush)) 0 (fromIntegral (leaf^.clNumLeafBrushes))
  where
    traceLine firstLeafBrush idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
              checkCount <- use (cmGlobals.cmCheckCount)
              mapLeafBrushes <- use (cmGlobals.cmMapLeafBrushes)
              let brushRef = Ref (fromIntegral (mapLeafBrushes UV.! (firstLeafBrush + idx)))
              brush <- readRef brushRef
              if (brush^.cbCheckCount) == checkCount
                  then
                      traceLine firstLeafBrush (idx + 1) maxIdx
                  else do
                      modifyRef brushRef (\v -> v & cbCheckCount .~ checkCount)
                      traceContents <- use (cmGlobals.cmTraceContents)
                      if (brush^.cbContents) .&. traceContents == 0
                          then
                              traceLine firstLeafBrush (idx + 1) maxIdx
                          else do
                              traceMins <- use (cmGlobals.cmTraceMins)
                              traceMaxs <- use (cmGlobals.cmTraceMaxs)
                              traceStart <- use (cmGlobals.cmTraceStart)
                              testBoxInBrush traceMins traceMaxs traceStart (cmGlobals.cmTraceTrace) brush
                              fraction <- use (cmGlobals.cmTraceTrace.tFraction)
                              unless (fraction == 0) $
                                  traceLine firstLeafBrush (idx + 1) maxIdx

testBoxInBrush :: V3 Float -> V3 Float -> V3 Float -> Lens' QuakeState TraceT -> CBrushT -> Quake ()
testBoxInBrush mins maxs p1 traceLens brush = do
    unless ((brush^.cbNumSides) == 0) $ do
        done <- checkIntersection (brush^.cbFirstBrushSide) 0 (brush^.cbNumSides)
        unless done $ do
            traceLens %= (\v -> v & tStartSolid .~ True
                                  & tAllSolid   .~ True
                                  & tFraction   .~ 0
                                  & tContents   .~ brush^.cbContents)
  where
    checkIntersection firstBrushSide idx maxIdx
        | idx >= maxIdx = return False
        | otherwise = do
              side <- readRef (Ref (firstBrushSide + idx))
              planeRef <- maybe planeError return (side^.cbsPlane)
              plane <- readRef planeRef
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
    planeError = do
        Com.fatalError "CM.testBoxInBrush#checkIntersection side^.cbsPlane is Nothing"
        return (Ref (-1))

-- fills in a list of all the leafs touched
boxLeafNums :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> Maybe [Int] -> Quake (Int, Maybe [Int])
boxLeafNums mins maxs list listSize topnode = do
    cmodel <- readRef (Ref 0)
    boxLeafNumsHeadnode mins maxs list listSize (cmodel^.cmHeadNode) topnode

boxLeafNumsHeadnode :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> Int -> Maybe [Int] -> Quake (Int, Maybe [Int])
boxLeafNumsHeadnode mins maxs list listSize headnode topnode = do
    cmGlobals.cmLeafCount .= 0
    cmGlobals.cmLeafTopNode .= (-1)
    boxLeafNumsR mins maxs list listSize headnode
    leafCount <- use (cmGlobals.cmLeafCount)
    leafTopNode <- use (cmGlobals.cmLeafTopNode)
    return (maybe (leafCount, Nothing) (\v -> (leafCount, Just (leafTopNode : tail v))) topnode)

boxLeafNumsR :: V3 Float -> V3 Float -> Lens' QuakeState (UV.Vector Int) -> Int -> Int -> Quake ()
boxLeafNumsR mins maxs leafList leafMaxCount nodenum
    | nodenum < 0 = do
        leafCount <- use (cmGlobals.cmLeafCount)
        updateLeafCount leafCount
    | otherwise = do
        node <- readRef (Ref nodenum)
        maybe planeError (doBoxLeafNumsR node) (node^.cnPlane)
  where
    updateLeafCount leafCount
        | leafCount >= leafMaxCount =
            Com.dprintf "CM_BoxLeafnums_r: overflow\n"
        | otherwise = do
            leafList.ix leafCount .= (-1) - nodenum
            cmGlobals.cmLeafCount += 1
    planeError = Com.fatalError "CM.boxLeafNumsR node^.cnPlane is Nothing"
    doBoxLeafNumsR node planeRef = do
        p <- readRef planeRef
        proceedBoxLeafNumsR node (Math3D.boxOnPlaneSide mins maxs p)
    proceedBoxLeafNumsR node s
        | s == 1 = boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._1)
        | s == 2 = boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._2)
        | otherwise = do
            leafTopNode <- use (cmGlobals.cmLeafTopNode)
            when (leafTopNode == -1) $ do
                cmGlobals.cmLeafTopNode .= nodenum
            boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._1)
            boxLeafNumsR mins maxs leafList leafMaxCount (node^.cnChildren._2)

recursiveHullCheck :: Int -> Float -> Float -> V3 Float -> V3 Float -> Quake ()
recursiveHullCheck num p1f p2f p1 p2 = do
    traceFraction <- use (cmGlobals.cmTraceTrace.tFraction)
    -- do nothing if we already hit something nearer
    unless (traceFraction <= p1f) $ do
        doHullCheck
  where
    doHullCheck
        | num < 0 = traceToLeaf ((-1) - num)
        | otherwise = do
            -- find the point distances to the separating plane
            -- and the offset for the size of the box
            node <- readRef (Ref num)
            (t1, t2, offset) <- findDistancesAndOffset (node^.cnPlane)
            considerSides node t1 t2 offset
    considerSides node t1 t2 offset
        | t1 >= offset && t2 >= offset = recursiveHullCheck (node^.cnChildren._1) p1f p2f p1 p2
        | t1 < (-offset) && t2 < (-offset) = recursiveHullCheck (node^.cnChildren._2) p1f p2f p1 p2
        | otherwise = do
            -- put the crosspoint DIST_EPSILON pixels on the near side
            let (side, tmpFrac, tmpFrac2) 
                    | t1 < t2 = let idist' = 1 / (t1 - t2)
                                    side' = 1 :: Int
                                    frac2' = (t1 + offset + distEpsilon) * idist'
                                    frac' = (t1 - offset + distEpsilon) * idist'
                                in (side', frac', frac2')
                    | t1 > t2 = let idist' = 1 / (t1 - t2)
                                    side' = 0 :: Int
                                    frac2' = (t1 - offset - distEpsilon) * idist'
                                    frac' = (t1 + offset + distEpsilon) * idist'
                                in (side', frac', frac2')
                    | otherwise = (0, 1, 0)
                frac | tmpFrac < 0 = 0
                     | tmpFrac > 1 = 1
                     | otherwise   = tmpFrac
                frac2 | tmpFrac2 < 0 = 0
                      | tmpFrac2 > 1 = 1
                      | otherwise    = tmpFrac2
            moveUpTheNode side frac node
            goPastTheNode side frac2 node
    findDistancesAndOffset Nothing = do
        Com.fatalError "CM.recursiveHullCheck node^.cnPlane is Nothing"
        return (0, 0, 0)
    findDistancesAndOffset (Just planeRef) = do
        plane <- readRef planeRef
        let pType = fromIntegral (plane^.cpType) :: Int
        traceIsPoint <- use (cmGlobals.cmTraceIsPoint)
        traceExtents <- use (cmGlobals.cmTraceExtents)
        return (doFindDistanceAndOffset plane pType traceIsPoint traceExtents)
    doFindDistanceAndOffset plane pType traceIsPoint traceExtents
        | pType < 3 =
            let t1 = p1^.(Math3D.v3Access pType) - (plane^.cpDist)
                t2 = p2^.(Math3D.v3Access pType) - (plane^.cpDist)
                offset = traceExtents^.(Math3D.v3Access pType)
            in (t1, t2, offset)
        | otherwise =
          let t1 = dot (plane^.cpNormal) p1 - (plane^.cpDist)
              t2 = dot (plane^.cpNormal) p2 - (plane^.cpDist)
              offset = if traceIsPoint then 0 else dot (fmap abs traceExtents) (fmap abs (plane^.cpNormal))
          in (t1, t2, offset)
    moveUpTheNode side frac node = do
      let midf = p1f + (p2f - p1f) * frac
          mid = p1 + fmap (* frac) (p2 - p1)
      recursiveHullCheck (node^.cnChildren.(if side == 0 then _1 else _2)) p1f midf p1 mid
    goPastTheNode side frac2 node = do
      let midf = p1f + (p2f - p1f) * frac2
          mid = p1 + fmap (* frac2) (p2 - p1)
      recursiveHullCheck (node^.cnChildren.(if side == 0 then _2 else _1)) midf p2f mid p2

traceToLeaf :: Int -> Quake ()
traceToLeaf leafNum = do
    leaf <- readRef (Ref leafNum)
    traceContents <- use (cmGlobals.cmTraceContents)
    unless ((leaf^.clContents) .&. traceContents == 0) $ do
        -- trace line against all brushes in the leaf
        traceLineAgainstAllBrushes (fromIntegral (leaf^.clFirstLeafBrush)) 0 (fromIntegral (leaf^.clNumLeafBrushes))

traceLineAgainstAllBrushes :: Int -> Int -> Int -> Quake ()
traceLineAgainstAllBrushes firstLeafBrush idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        checkCount <- use (cmGlobals.cmCheckCount)
        leafBrushes <- use (cmGlobals.cmMapLeafBrushes)
        let brushRef = Ref (fromIntegral (leafBrushes UV.! (firstLeafBrush + idx)))
        brush <- readRef brushRef
        doTrace brushRef brush checkCount
  where
    doTrace brushRef brush checkCount
        | (brush^.cbCheckCount) == checkCount = -- already checked this brush in another leaf
            traceLineAgainstAllBrushes firstLeafBrush (idx + 1) maxIdx
        | otherwise = do
            modifyRef brushRef (\v -> v & cbCheckCount .~ checkCount)
            traceContents <- use (cmGlobals.cmTraceContents)
            proceedTrace brush traceContents
    proceedTrace brush traceContents
        | (brush^.cbContents) .&. traceContents == 0 =
            traceLineAgainstAllBrushes firstLeafBrush (idx + 1) maxIdx
        | otherwise = do
            traceMins <- use (cmGlobals.cmTraceMins)
            traceMaxs <- use (cmGlobals.cmTraceMaxs)
            traceStart <- use (cmGlobals.cmTraceStart)
            traceEnd <- use (cmGlobals.cmTraceEnd)
            clipBoxToBrush traceMins traceMaxs traceStart traceEnd (cmGlobals.cmTraceTrace) brush
            traceTraceFraction <- use (cmGlobals.cmTraceTrace.tFraction)
            unless (traceTraceFraction == 0) $
                traceLineAgainstAllBrushes firstLeafBrush (idx + 1) maxIdx

clipBoxToBrush :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Lens' QuakeState TraceT -> CBrushT -> Quake ()
clipBoxToBrush mins maxs p1 p2 traceLens brush = do
    unless ((brush^.cbNumSides) == 0) $ do
        globals.gCBrushTraces += 1
        (done, enterFrac, leaveFrac, clipPlane, getOut, startOut, leadSide) <- findIntersections (-1) 1 Nothing False False Nothing 0 (brush^.cbNumSides)
        unless done $
            doClip enterFrac leaveFrac clipPlane getOut startOut leadSide
  where
    doClip enterFrac leaveFrac clipPlane getOut startOut leadSide
        | not startOut = do -- origin point was inside brush
            traceLens.tStartSolid .= True
            unless getOut $
                traceLens.tAllSolid .= True
        | otherwise =
            when (enterFrac < leaveFrac) $ do
                traceT <- use traceLens
                when (enterFrac > (-1) && enterFrac < (traceT^.tFraction)) $ do
                    plane <- getPlane clipPlane
                    brushSide <- getBrushSide leadSide
                    surface <- getSurface (brushSide^.cbsSurface)
                    traceLens.tFraction .= if enterFrac < 0 then 0 else enterFrac
                    traceLens.tPlane .= plane
                    traceLens.tSurface .= Just (surface^.msCSurface) -- TODO: this might be an issue! maybe hold the reference to mapSurfaceT ?
                    traceLens.tContents .= (brush^.cbContents)
    getPlane Nothing = do
        Com.fatalError "CM.clipBoxToBrush clipPlane is Nothing"
        undefined -- IMPROVE ?
    getPlane (Just planeRef) = readRef planeRef
    getBrushSide Nothing = do
        Com.fatalError "CM.clipBoxToBrush leadSide is Nothing"
        undefined -- IMPROVE ?
    getBrushSide (Just idx) = readRef (Ref idx)
    getSurface Nothing = return nullSurface
    getSurface (Just surfaceRef) = readRef surfaceRef
    -- findIntersections :: Float -> Float -> Maybe Int -> Bool -> Bool -> Maybe Int -> Int -> Int -> Quake (Bool, Float, Float, Maybe Int, Bool, Bool, Maybe Int)
    findIntersections enterFrac leaveFrac clipPlane getOut startOut leadSide idx maxIdx
        | idx >= maxIdx = return (False, enterFrac, leaveFrac, clipPlane, getOut, startOut, leadSide)
        | otherwise = do
            brushSide <- readRef (Ref ((brush^.cbFirstBrushSide) + idx))
            plane <- readRef (fromJust (brushSide^.cbsPlane)) -- TODO: avoid using fromJust
            -- FIXME: special case for axial
            traceIsPoint <- use (cmGlobals.cmTraceIsPoint)
            let dist | traceIsPoint = plane^.cpDist -- special point case
                     | otherwise = -- general box case
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
                getOut' | d2 > 0 = True -- endpoint is not in solid
                        | otherwise = getOut
                startOut' | d1 > 0 = True
                          | otherwise = startOut
            case () of
                _ | d1 > 0 && d2 >= d1 -> return (True, enterFrac, leaveFrac, clipPlane, getOut', startOut', leadSide) -- completely in front of face, no intersection
                  | d1 <= 0 && d2 <= 0 -> findIntersections enterFrac leaveFrac clipPlane getOut' startOut' leadSide (idx + 1) maxIdx
                  | d1 > d2 -> do -- crosses face
                      let f = (d1 - distEpsilon) / (d1 - d2)
                          enterFrac' = if f > enterFrac then f else enterFrac
                          clipPlane' = if f > enterFrac then brushSide^.cbsPlane else clipPlane
                          leadSide' = if f > enterFrac then Just ((brush^.cbFirstBrushSide) + idx) else leadSide
                      findIntersections enterFrac' leaveFrac clipPlane' getOut' startOut' leadSide' (idx + 1) maxIdx
                  | otherwise -> do
                      let f = (d1 + distEpsilon) / (d1 - d2)
                          leaveFrac' = if f < leaveFrac then f else leaveFrac
                      findIntersections enterFrac leaveFrac' clipPlane getOut' startOut' leadSide (idx + 1) maxIdx

-- Returns a tag that describes the content of the point
pointContents :: V3 Float -> Int -> Quake Int
pointContents p headNode = do
    numNodes <- use (cmGlobals.cmNumNodes)
    getPointContents numNodes
  where
    getPointContents numNodes
        | numNodes == 0 = return 0 -- map not loaded
        | otherwise = do
            idx <- pointLeafNumR p headNode
            mapLeafs <- use (cmGlobals.cmMapLeafs)
            return ((mapLeafs V.! idx)^.clContents)

-- Searches the leaf number that contains the 3d point
pointLeafNum :: V3 Float -> Quake Int
pointLeafNum p = do
    -- sound may call this without map loaded
    numPlanes <- use (cmGlobals.cmNumPlanes)
    getPointLeafNum numPlanes
  where
    getPointLeafNum numPlanes
        | numPlanes == 0 = return 0
        | otherwise = pointLeafNumR p 0

pointLeafNumR :: V3 Float -> Int -> Quake Int
pointLeafNumR p num = do
    updatedNum <- findNum num
    globals.gCPointContents += 1 -- optimize counter
    return ((-1) - updatedNum)
  where
    findNum n
        | n >= 0 = do
            node <- readRef (Ref n)
            maybe planeError (doFindNum node) (node^.cnPlane)
        | otherwise = return n
    planeError = do
        Com.fatalError "CM.pointLeafNumR node^.cnPlane is Nothing"
        return (-1)
    doFindNum node planeRef = do
        plane <- readRef planeRef
        let d | plane^.cpType < 3 = p^.(Math3D.v3Access (fromIntegral (plane^.cpType))) - (plane^.cpDist)
              | otherwise         = dot (plane^.cpNormal) p - (plane^.cpDist)
        let v | d < 0     = node^.cnChildren._2
              | otherwise = node^.cnChildren._1
        findNum v

transformedPointContents :: V3 Float -> Int -> V3 Float -> V3 Float -> Quake Int
transformedPointContents p headNode origin angles = do
    boxHeadNode <- use (cmGlobals.cmBoxHeadNode)
    -- rotate start and end into the models frame of reference
    let pL' = if headNode /= boxHeadNode && ((angles^._x) /= 0 || (angles^._y) /= 0 || (angles^._z) /= 0)
                  then let (forward, right, up) = Math3D.angleVectors angles True True True
                       in V3 (pL `dot` forward) (-(pL `dot` right)) (pL `dot` up)
                  else pL
    idx <- pointLeafNumR pL' headNode
    mapLeafs <- use (cmGlobals.cmMapLeafs)
    return ((mapLeafs V.! idx)^.clContents)
  where
    pL = p - origin

writePortalState :: QuakeFile -> Quake ()
writePortalState saveFile = do
    portalOpen <- use (cmGlobals.cmPortalOpen)
    io (UV.mapM_ writePortal portalOpen) -- IMPROVE: catch exception?
  where 
    writePortal True = QuakeFile.writeInt saveFile 1
    writePortal False = QuakeFile.writeInt saveFile 0

transformedBoxTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> V3 Float -> V3 Float -> Quake TraceT
transformedBoxTrace start end mins maxs headNode brushMask origin angles = do
    -- subtract origin offset
    let startL = start - origin
        endL = end - origin
    -- rotate start and end into the models frame of reference
    boxHeadNode <- use (cmGlobals.cmBoxHeadNode)
    let rotated = headNode /= boxHeadNode && ((angles^._x) /= 0 || (angles^._y) /= 0 || (angles^._z) /= 0)
        (startL', endL') =
            if rotated
                then let (forward, right, up) = Math3D.angleVectors angles True True True
                         s = V3 (startL `dot` forward) (- (startL `dot` right)) (startL `dot` up)
                         e = V3 (endL `dot` forward) (- (endL `dot` right)) (endL `dot` up)
                     in (s, e)
                else (startL, endL)
    -- sweep the box through the model
    traceT <- boxTrace startL' endL' mins maxs headNode brushMask
    let traceT' = if rotated && (traceT^.tFraction) /= 1
                      then let (forward, right, up) = Math3D.angleVectors angles True True True
                               temp = traceT^.tPlane.cpNormal
                           in traceT { _tPlane = (traceT^.tPlane) { _cpNormal = V3 (temp `dot` forward) (- (temp `dot` right)) (temp `dot` up) } }
                      else traceT
        endPos = start + fmap (* (traceT^.tFraction)) (end - start)
    return (traceT' & tEndPos .~ endPos)

headnodeForBox :: V3 Float -> V3 Float -> Quake Int
headnodeForBox mins maxs = do
    numPlanes <- use (cmGlobals.cmNumPlanes)
    boxHeadNode <- use (cmGlobals.cmBoxHeadNode)
    modifyRef (Ref (numPlanes +  0)) (\v -> v & cpDist .~ maxs^._x)
    modifyRef (Ref (numPlanes +  1)) (\v -> v & cpDist .~ - (maxs^._x))
    modifyRef (Ref (numPlanes +  2)) (\v -> v & cpDist .~ mins^._x)
    modifyRef (Ref (numPlanes +  3)) (\v -> v & cpDist .~ - (mins^._x))
    modifyRef (Ref (numPlanes +  4)) (\v -> v & cpDist .~ maxs^._y)
    modifyRef (Ref (numPlanes +  5)) (\v -> v & cpDist .~ - (maxs^._y))
    modifyRef (Ref (numPlanes +  6)) (\v -> v & cpDist .~ mins^._y)
    modifyRef (Ref (numPlanes +  7)) (\v -> v & cpDist .~ - (mins^._y))
    modifyRef (Ref (numPlanes +  8)) (\v -> v & cpDist .~ maxs^._z)
    modifyRef (Ref (numPlanes +  9)) (\v -> v & cpDist .~ - (maxs^._z))
    modifyRef (Ref (numPlanes + 10)) (\v -> v & cpDist .~ mins^._z)
    modifyRef (Ref (numPlanes + 11)) (\v -> v & cpDist .~ - (mins^._z))
    return boxHeadNode

headNodeVisible :: Int -> UV.Vector Word8 -> Quake Bool
headNodeVisible nodeNum visbits
    | nodeNum < 0 = do
        mapLeafs <- use (cmGlobals.cmMapLeafs)
        let leafNum = -1 - nodeNum
            cluster = (mapLeafs V.! leafNum)^.clCluster
        if cluster == -1
            then return False
            else return ((visbits UV.! (cluster `shiftR` 3)) .&. (1 `shiftL` (cluster .&. 7)) /= 0)
    | otherwise = do
        mapNodes <- use $ cmGlobals.cmMapNodes
        let node = mapNodes V.! nodeNum
        v <- headNodeVisible (node^.cnChildren._1) visbits
        if v then return True else headNodeVisible (node^.cnChildren._2) visbits

-- IMPROVE: begs for performance improvement?
writeAreaBits :: Traversal' QuakeState (SV.Vector Word8) -> Int -> Quake Int
writeAreaBits bufferLens area = do
    numAreas <- use (cmGlobals.cmNumAreas)
    noAreasValue <- fmap (^.cvValue) mapNoAreasCVar
    let bytes = (numAreas + 7) `shiftR` 3
    if noAreasValue /= 0
        then
            bufferLens %= \buffer -> (SV.replicate bytes 255) SV.++ (SV.drop bytes buffer)
        else do
            mapAreas <- use (cmGlobals.cmMapAreas)
            Just buffer <- preuse bufferLens
            let buffer' = (SV.replicate bytes 0) SV.++ (SV.drop bytes buffer)
                floodNum = (mapAreas V.! area)^.caFloodNum
            bufferLens .= constructAreaBits floodNum mapAreas buffer' 0 numAreas
    return bytes
  where
    constructAreaBits floodNum mapAreas buffer idx maxIdx
        | idx >= maxIdx = buffer
        | ((mapAreas V.! idx)^.caFloodNum) == floodNum || area == 0 =
            constructAreaBits floodNum mapAreas (buffer SV.// [(idx `shiftR` 3, (buffer SV.! (idx `shiftR` 3)) .|. (1 `shiftL` (fromIntegral idx .&. 7)))]) (idx + 1) maxIdx
        | otherwise =
            constructAreaBits floodNum mapAreas buffer (idx + 1) maxIdx
