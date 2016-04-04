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
import           QCommon.QFiles.BSP.DHeaderT
import qualified Constants
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary (encode, getMany)
import qualified Util.Lib as Lib
import           Util.Vector (pipeToVector)

import           Control.Lens (use, (.=), (%=), (+=), (^.), (&), (.~))
import           Control.Monad (void, unless, when)
import           Control.Monad.State.Strict (runStateT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3)
import           Pipes.Binary (deMessage)
import qualified Pipes.Binary as PB
import qualified Pipes.ByteString as PBS
import qualified Pipes.Parse as PP
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
     (res, _) <- runStateT (PB.decodeGet getDHeaderT) (PBS.fromLazy buf)
     either dHeaderTParseError (loadBSP name checksum buf len) res
  where dHeaderTParseError err =
          do Com.fatalError (BC.pack (deMessage err))
             return (Ref 0, 0 : tail checksum)

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
     cmGlobals.cmNumTexInfo .= count
     Com.dprintf (B.concat [" numtexinfo=", encode count, "\n"])
     -- TODO: skipped the debugLoadMap part, should probably introduce it at some point
     texInfo <- request (pipeToVector texInfoProducer) :: Quake (V.Vector TexInfoT)
     -- TODO: make sure texInfo is of size count
     cmGlobals.cmMapSurfaces %= (\v -> V.update v (V.imap (\i t -> (i, toMapSurface t)) texInfo))
  where count = (lump^.lFileLen) `div` texInfoTSize
        checkLump =
          do when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $
               Com.comError Constants.errDrop "MOD_LoadBmodel: funny lump size"
             when (count < 1) $
               Com.comError Constants.errDrop "Map with no surfaces"
             when (count > Constants.maxMapTexInfo) $
               Com.comError Constants.errDrop "Map has too many surfaces"
        texInfoProducer = (getMany getTexInfoT (PBS.fromLazy (BL.drop (fromIntegral (lump^.lFileOfs)) buf))) ^. PP.splitAt count

toMapSurface :: TexInfoT -> MapSurfaceT
toMapSurface texInfo = MapSurfaceT { _msCSurface = cSurface, _msRName = Just (texInfo^.tiTexture) }
  where cSurface = CSurfaceT { _csName = texInfo^.tiTexture
                             , _csFlags = texInfo^.tiFlags
                             , _csValue = texInfo^.tiValue
                             }

loadLeafs :: BL.ByteString -> LumpT -> Quake ()
loadLeafs = error "CM.loadLeafs" -- TODO

loadLeafBrushes :: BL.ByteString -> LumpT -> Quake ()
loadLeafBrushes = error "CM.loadLeafBrushes" -- TODO

loadPlanes :: BL.ByteString -> LumpT -> Quake ()
loadPlanes = error "CM.loadPlanes" -- TODO

loadBrushes :: BL.ByteString -> LumpT -> Quake ()
loadBrushes = error "CM.loadBrushes" -- TODO

loadBrushSides :: BL.ByteString -> LumpT -> Quake ()
loadBrushSides = error "CM.loadBrushSides" -- TODO

loadSubmodels :: BL.ByteString -> LumpT -> Quake ()
loadSubmodels = error "CM.loadSubmodels" -- TODO

loadNodes :: BL.ByteString -> LumpT -> Quake ()
loadNodes = error "CM.loadNodes" -- TODO

loadAreas :: BL.ByteString -> LumpT -> Quake ()
loadAreas = error "CM.loadAreas" -- TODO

loadAreaPortals :: BL.ByteString -> LumpT -> Quake ()
loadAreaPortals = error "CM.loadAreaPortals" -- TODO

loadVisibility :: BL.ByteString -> LumpT -> Quake ()
loadVisibility = error "CM.loadVisibility" -- TODO

loadEntityString :: BL.ByteString -> LumpT -> Quake ()
loadEntityString = error "CM.loadEntityString" -- TODO

initBoxHull :: Quake ()
initBoxHull = error "CM.initBoxHull" -- TODO