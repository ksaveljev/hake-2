{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Model where

import Control.Lens ((.=), (+=), preuse, ix, (^.), zoom, use)
import Control.Monad (when, liftM)
import Data.Maybe (isNothing, fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.QFiles.BSP.DHeaderT
import QCommon.QFiles.MD2.DMdlT
import QCommon.QFiles.SP2.DSpriteT
import QCommon.XCommandT
import Render.MEdgeT
import Render.MVertexT
import Render.OpenGL.GLDriver
import Util.Binary
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Render.Fast.Polygon as Polygon
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Util.Lib as Lib

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO

modInit :: Quake ()
modInit = do
    -- init mod_known
    fastRenderAPIGlobals.frModKnown .= V.replicate maxModKnown newModelT
    fastRenderAPIGlobals.frModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF

rBeginRegistration :: GLDriver -> B.ByteString -> Quake ()
rBeginRegistration _ model = do
    resetModelArrays
    Polygon.reset

    fastRenderAPIGlobals.frRegistrationSequence += 1
    fastRenderAPIGlobals.frOldViewCluster .= (-1) -- force markleafs

    let fullName = "maps/" `B.append` model `B.append` ".bsp"

    -- explicitly free the old map if different
    -- this guarantees that mod_known[0] is the world map
    Just flushMap <- CVar.get "flushmap" "0" 0
    Just currentName <- preuse $ fastRenderAPIGlobals.frModKnown.ix 0.mName

    when (currentName /= fullName || (flushMap^.cvValue) /= 0) $
      modFree (ModKnownReference 0)

    modelRef <- modForName fullName True
    fastRenderAPIGlobals.frWorldModel .= modelRef

    fastRenderAPIGlobals.frViewCluster .= (-1)

modFree :: ModelReference -> Quake ()
modFree (ModInlineReference modelIdx) = fastRenderAPIGlobals.frModInline.ix modelIdx .= newModelT
modFree (ModKnownReference modelIdx) = fastRenderAPIGlobals.frModKnown.ix modelIdx .= newModelT

{-
==================
Mod_ForName

Loads in a model for the given name
==================
-}
modForName :: B.ByteString -> Bool -> Quake (Maybe ModelReference)
modForName name crash = do
    when (B.length name == 0) $
      Com.comError Constants.errDrop "Mod_ForName: NULL name"

    -- inline models are grabbed only from worldmodel
    if name `BC.index` 0 == '*'
      then do
        worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
        let i = Lib.atoi (B.drop 1 name)
        err <- if i < 1 || isNothing worldModelRef
                 then return True
                 else do
                   Just worldModel <- case fromJust worldModelRef of
                                        ModKnownReference modelIdx -> preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx
                                        ModInlineReference modelIdx -> preuse $ fastRenderAPIGlobals.frModInline.ix modelIdx

                   return $ if i >= (worldModel^.mNumSubModels)
                              then True
                              else False

        when err $ Com.comError Constants.errDrop "bad inline model number"
        return $ Just (ModInlineReference i)
      else do
        -- search the currently loaded models
        modNumKnown <- use $ fastRenderAPIGlobals.frModNumKnown
        modKnown <- liftM (V.take modNumKnown) (use $ fastRenderAPIGlobals.frModKnown)
        let foundIndex = V.findIndex (\m -> (m^.mName) == name) modKnown

        case foundIndex of
          Just idx -> return $ Just (ModKnownReference idx)
          Nothing -> do
            -- find a free model slot spot
            let emptySpot = V.findIndex (\m -> B.null (m^.mName)) modKnown
            emptySpotIdx <- case emptySpot of
                              Just i -> return i
                              Nothing -> do
                                when (modNumKnown == maxModKnown) $
                                  Com.comError Constants.errDrop "mod_numknown == MAX_MOD_KNOWN"
                                fastRenderAPIGlobals.frModNumKnown += 1
                                return modNumKnown

            fastRenderAPIGlobals.frModKnown.ix emptySpotIdx.mName .= name

            -- load the file
            fileBuffer <- FS.loadFile name

            case fileBuffer of
              Nothing -> do
                when crash $
                  Com.comError Constants.errDrop ("Mod_NumForName: " `B.append` name `B.append` " not found\n")

                fastRenderAPIGlobals.frModKnown.ix emptySpotIdx.mName .= ""
                return Nothing

              Just buffer -> do
                let modelRef = ModKnownReference emptySpotIdx
                fastRenderAPIGlobals.frLoadModel .= modelRef

                -- fill it in
                -- call the apropriate loader
                let header = B.take 4 buffer

                if | header == idAliasHeader -> loadAliasModel modelRef buffer
                   | header == idSpriteHeader -> loadSpriteModel modelRef buffer
                   | header == idBSPHeader -> loadBrushModel modelRef buffer
                   | otherwise -> Com.comError Constants.errDrop ("Mod_NumForName: unknown fileid for " `B.append` name)

                return $ Just modelRef

resetModelArrays :: Quake ()
resetModelArrays = do
    zoom (fastRenderAPIGlobals) $ do
      frModelTextureCoordIdx .= 0
      frModelVertexIndexIdx  .= 0

loadAliasModel :: ModelReference -> B.ByteString -> Quake ()
loadAliasModel _ _ = do
    io (putStrLn "Model.loadAliasModel") >> undefined -- TODO

loadSpriteModel :: ModelReference -> B.ByteString -> Quake ()
loadSpriteModel _ _ = do
    io (putStrLn "Model.loadSpriteModel") >> undefined -- TODO

loadBrushModel :: ModelReference -> B.ByteString -> Quake ()
loadBrushModel modelRef buffer = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel
    -- assume these can only be ModKnownReference (TODO: are we sure??)
    let ModKnownReference modelIdx = modelRef
        ModKnownReference loadModelIdx = loadModelRef

    fastRenderAPIGlobals.frModKnown.ix loadModelIdx.mType .= RenderAPIConstants.modBrush

    when (loadModelIdx /= 0) $
      Com.comError Constants.errDrop "Loaded a brush model after the world"

    let header = newDHeaderT (BL.fromStrict buffer)

    when ((header^.dhVersion) /= Constants.bspVersion) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("Mod_LoadBrushModel: " `B.append` name `B.append` " has wrong version number (" `B.append` BC.pack (show (header^.dhVersion)) `B.append` " should be " `B.append` BC.pack (show Constants.bspVersion) `B.append` ")")

    -- load into heap
    loadVertexes     buffer ((header^.dhLumps) V.! Constants.lumpVertexes)
    loadEdges        buffer ((header^.dhLumps) V.! Constants.lumpEdges)
    loadSurfEdges    buffer ((header^.dhLumps) V.! Constants.lumpSurfEdges)
    loadLighting     buffer ((header^.dhLumps) V.! Constants.lumpLighting)
    loadPlanes       buffer ((header^.dhLumps) V.! Constants.lumpPlanes)
    loadTexInfo      buffer ((header^.dhLumps) V.! Constants.lumpTexInfo)
    loadFaces        buffer ((header^.dhLumps) V.! Constants.lumpFaces)
    loadMarkSurfaces buffer ((header^.dhLumps) V.! Constants.lumpLeafFaces)
    loadVisibility   buffer ((header^.dhLumps) V.! Constants.lumpVisibility)
    loadLeafs        buffer ((header^.dhLumps) V.! Constants.lumpLeafs)
    loadNodes        buffer ((header^.dhLumps) V.! Constants.lumpNodes)
    loadSubmodels    buffer ((header^.dhLumps) V.! Constants.lumpModels)
    io (putStrLn "Model.loadBrushModel") >> undefined -- TODO

loadVertexes :: B.ByteString -> LumpT -> Quake ()
loadVertexes buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` diskSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` diskSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        vertexes = runGet (getVertexes count) buf
    
    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumVertexes .= count
      mVertexes .= vertexes

  where getVertexes :: Int -> Get (V.Vector MVertexT)
        getVertexes count = V.replicateM count getMVertexT

loadEdges :: B.ByteString -> LumpT -> Quake ()
loadEdges buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` diskSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` diskSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        edges = runGet (getEdges count) buf
    
    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumEdges .= count
      mEdges .= edges

  where getEdges :: Int -> Get (V.Vector MEdgeT)
        getEdges count = V.replicateM count getMEdgeT

loadSurfEdges :: B.ByteString -> LumpT -> Quake ()
loadSurfEdges _ _ = do
    io (putStrLn "Model.loadSurfEdges") >> undefined -- TODO

loadLighting :: B.ByteString -> LumpT -> Quake ()
loadLighting _ _ = do
    io (putStrLn "Model.loadLighting") >> undefined -- TODO

loadPlanes :: B.ByteString -> LumpT -> Quake ()
loadPlanes _ _ = do
    io (putStrLn "Model.loadPlanes") >> undefined -- TODO

loadTexInfo :: B.ByteString -> LumpT -> Quake ()
loadTexInfo _ _ = do
    io (putStrLn "Model.loadTexInfo") >> undefined -- TODO

loadFaces :: B.ByteString -> LumpT -> Quake ()
loadFaces _ _ = do
    io (putStrLn "Model.loadFaces") >> undefined -- TODO

loadMarkSurfaces :: B.ByteString -> LumpT -> Quake ()
loadMarkSurfaces _ _ = do
    io (putStrLn "Model.loadMarkSurfaces") >> undefined -- TODO

loadVisibility :: B.ByteString -> LumpT -> Quake ()
loadVisibility _ _ = do
    io (putStrLn "Model.loadVisibility") >> undefined -- TODO

loadLeafs :: B.ByteString -> LumpT -> Quake ()
loadLeafs _ _ = do
    io (putStrLn "Model.loadLeafs") >> undefined -- TODO

loadNodes :: B.ByteString -> LumpT -> Quake ()
loadNodes _ _ = do
    io (putStrLn "Model.loadNodes") >> undefined -- TODO

loadSubmodels :: B.ByteString -> LumpT -> Quake ()
loadSubmodels _ _ = do
    io (putStrLn "Model.loadSubmodels") >> undefined -- TODO
