{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.CM where

import Control.Lens (use, (%=), (.=), (^.))
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
import QCommon.LumpT
import QCommon.QFiles.BSP.DHeaderT
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
           let updatedChecksum = lastChecksum : (tail checksum)

           unless clientLoad $ do
             cmGlobals.cmPortalOpen %= UV.map (const False)
             floodAreaConnections

           -- still have the right version
           cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (cModel, updatedChecksum)

       | B.length name == 0 -> do
           cmGlobals.cmNumNodes .= 0
           cmGlobals.cmNumLeafs .= 1
           cmGlobals.cmNumCModels .= 0
           cmGlobals.cmNumVisibility .= 0
           cmGlobals.cmNumEntityChars .= 0
           cmGlobals.cmMapEntityString .= ""
           cmGlobals.cmMapName .= ""
           cmGlobals.cmNumClusters .= 1
           cmGlobals.cmNumAreas .= 1

           -- cinematic servers won't have anything at all
           let updatedChecksum = 0 : tail checksum
           cModel <- liftM (V.! 0) (use $ cmGlobals.cmMapCModels)
           return (cModel, updatedChecksum)

       | otherwise -> do
           cmGlobals.cmNumNodes .= 0
           cmGlobals.cmNumLeafs .= 0
           cmGlobals.cmNumCModels .= 0
           cmGlobals.cmNumVisibility .= 0
           cmGlobals.cmNumEntityChars .= 0
           cmGlobals.cmMapEntityString .= ""
           cmGlobals.cmMapName .= ""

           --
           -- load the file
           --
           loadedFile <- FS.loadFile name

           when (isNothing loadedFile) $
             Com.comError Constants.errDrop ("Couldn't load " `B.append` name)

           let Just buf = BL.fromStrict <$> loadedFile
               len = BL.length buf
               bufChecksum = MD4.blockChecksum buf len
               updatedChecksum = bufChecksum : (tail checksum)

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

loadSubmodels :: LumpT -> Quake ()
loadSubmodels _ = io (putStrLn "CM.loadSubmodels") >> undefined -- TODO

loadSurfaces :: LumpT -> Quake ()
loadSurfaces _ = io (putStrLn "CM.loadSurfaces") >> undefined -- TODO

loadNodes :: LumpT -> Quake ()
loadNodes _ = io (putStrLn "CM.loadNodes") >> undefined -- TODO

loadBrushes :: LumpT -> Quake ()
loadBrushes _ = io (putStrLn "CM.loadBrushes") >> undefined -- TODO

loadLeafs :: LumpT -> Quake ()
loadLeafs _ = io (putStrLn "CM.loadLeafs") >> undefined -- TODO

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
