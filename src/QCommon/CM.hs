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
import qualified QCommon.MD4 as MD4
import           QCommon.QFiles.BSP.DAreaPortalT
import qualified Constants
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib as Lib

import           Control.Lens (use, (.=), (%=), (+=), (^.), (&), (.~))
import           Control.Monad (void, unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3)
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
     doLoadMap name clientLoad checksum mapName flushMap

doLoadMap :: B.ByteString -> Bool -> [Int] -> B.ByteString -> Float -> Quake (Ref CModelT, [Int])
doLoadMap name clientLoad checksum mapName flushMap
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
         maybe loadMapError (proceedLoadMap checksum) fileHandle
  where resetCommonCMGlobals =
          do cmGlobals.cmNumCModels .= 0
             cmGlobals.cmNumVisibility .= 0
             cmGlobals.cmNumEntityChars .= 0
             cmGlobals.cmMapEntityString .= B.empty
             cmGlobals.cmMapName .= B.empty
        loadMapError =
          do Com.comError Constants.errDrop ("Couldn't load " `B.append` name)
             return (Ref 0, 0 : tail checksum)

proceedLoadMap :: [Int] -> (Handle, Int) -> Quake (Ref CModelT, [Int])
proceedLoadMap checksum (fileHandle, len) =
  do buf <- request (io (BL.hGet fileHandle len))
     cmGlobals.cmLastChecksum .= MD4.blockChecksum buf (fromIntegral len)
     error "CM.proceedLoadMap" -- TODO
     return (Ref 0, 0 : tail checksum) -- TODO: fix this
{-
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
           -}

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
