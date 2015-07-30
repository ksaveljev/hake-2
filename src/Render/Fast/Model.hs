{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Model where

import Control.Lens ((.=), (+=), preuse, ix, (^.), zoom, use, (%=), Traversal', _1, _2)
import Control.Monad (when, liftM)
import Data.Binary.IEEE754 (wordToFloat)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Int (Int8, Int32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Maybe (isNothing, fromJust, isJust)
import Data.Monoid ((<>), mempty)
import Data.Word (Word8, Word16, Word32)
import Linear (V3(..), V4(..), norm, _x, _y, _z, dot)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.QFiles.BSP.DFaceT
import QCommon.QFiles.BSP.DHeaderT
import QCommon.QFiles.BSP.DLeafT
import QCommon.QFiles.BSP.DModelT
import QCommon.QFiles.BSP.DNodeT
import QCommon.QFiles.BSP.DPlaneT
import QCommon.QFiles.MD2.DAliasFrameT
import QCommon.QFiles.MD2.DMdlT
import QCommon.QFiles.MD2.DSTVertT
import QCommon.QFiles.MD2.DTriangleT
import QCommon.QFiles.SP2.DSpriteT
import QCommon.TexInfoT
import QCommon.XCommandT
import Util.Binary
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Polygon as Polygon
import {-# SOURCE #-} qualified Render.Fast.Surf as Surf
import qualified Render.Fast.Warp as Warp
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Util.Lib as Lib

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO

modInit :: Quake ()
modInit = do
    -- init mod_known
    models <- io $ V.replicateM maxModKnown (newIORef newModelT)
    fastRenderAPIGlobals.frModKnown .= models
    fastRenderAPIGlobals.frModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF

rBeginRegistration :: B.ByteString -> Quake ()
rBeginRegistration model = do
    resetModelArrays
    Polygon.reset

    fastRenderAPIGlobals.frRegistrationSequence += 1
    fastRenderAPIGlobals.frOldViewCluster .= (-1) -- force markleafs

    let fullName = "maps/" `B.append` model `B.append` ".bsp"

    -- explicitly free the old map if different
    -- this guarantees that mod_known[0] is the world map
    Just flushMap <- CVar.get "flushmap" "0" 0
    Just modelRef <- preuse $ fastRenderAPIGlobals.frModKnown.ix 0
    model <- io $ readIORef modelRef
    let currentName = model^.mName

    when (currentName /= fullName || (flushMap^.cvValue) /= 0) $
      modFree modelRef

    modelRef <- modForName fullName True
    fastRenderAPIGlobals.frWorldModel .= modelRef

    fastRenderAPIGlobals.frViewCluster .= (-1)

modFree :: IORef ModelT -> Quake ()
modFree modelRef = io $ writeIORef modelRef newModelT

resetModelArrays :: Quake ()
resetModelArrays = do
    zoom (fastRenderAPIGlobals) $ do
      frModelTextureCoordIdx .= 0
      frModelVertexIndexIdx  .= 0

{-
==================
Mod_ForName
Loads in a model for the given name
==================
-}
modForName :: B.ByteString -> Bool -> Quake (Maybe (IORef ModelT))
modForName name crash = do
    when (B.null name) $
      Com.comError Constants.errDrop "Mod_ForName: NULL name"

    -- inline models are grabbed only from worldmodel
    if name `BC.index` 0 == '*'
      then do
        worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
        let i = Lib.atoi (B.drop 1 name)
        err <- if i < 1 || isNothing worldModelRef
                 then return True
                 else do
                   worldModel <- io $ readIORef (fromJust worldModelRef)

                   return $ if i >= (worldModel^.mNumSubModels)
                              then True
                              else False

        when err $ Com.comError Constants.errDrop "bad inline model number"
        preuse $ fastRenderAPIGlobals.frModInline.ix i
      else do
        -- search the currently loaded models
        modNumKnown <- use $ fastRenderAPIGlobals.frModNumKnown
        modKnown <- use $ fastRenderAPIGlobals.frModKnown
        foundRef <- findWithSameName modKnown 0 modNumKnown

        case foundRef of
          Just ref -> return (Just ref)
          Nothing -> do
            -- find a free model slot spot
            emptySpot <- findEmptySpot modKnown 0 modNumKnown
            emptySpotRef <- case emptySpot of
                              Just ref -> return ref 
                              Nothing -> do
                                when (modNumKnown == maxModKnown) $
                                  Com.comError Constants.errDrop "mod_numknown == MAX_MOD_KNOWN"
                                fastRenderAPIGlobals.frModNumKnown += 1
                                return (modKnown V.! modNumKnown)

            io $ modifyIORef' emptySpotRef (\v -> v { _mName = name })

            -- load the file
            fileBuffer <- FS.loadFile name

            case fileBuffer of
              Nothing -> do
                when crash $
                  Com.comError Constants.errDrop ("Mod_NumForName: " `B.append` name `B.append` " not found\n")

                io $ modifyIORef' emptySpotRef (\v -> v { _mName = "" })
                return Nothing

              Just buffer -> do
                fastRenderAPIGlobals.frLoadModel .= emptySpotRef

                -- fill it in
                -- call the apropriate loader
                let header = B.take 4 buffer

                if | header == idAliasHeader -> loadAliasModel emptySpotRef buffer
                   | header == idSpriteHeader -> loadSpriteModel emptySpotRef buffer
                   | header == idBSPHeader -> loadBrushModel emptySpotRef buffer
                   | otherwise -> Com.comError Constants.errDrop ("Mod_NumForName: unknown fileid for " `B.append` name)

                return $ Just emptySpotRef

  where findWithSameName :: V.Vector (IORef ModelT) -> Int -> Int -> Quake (Maybe (IORef ModelT))
        findWithSameName modKnown idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              model <- io $ readIORef (modKnown V.! idx)
              if (model^.mName) == name
                then return $ Just (modKnown V.! idx)
                else findWithSameName modKnown (idx + 1) maxIdx

        findEmptySpot :: V.Vector (IORef ModelT) -> Int -> Int -> Quake (Maybe (IORef ModelT))
        findEmptySpot modKnown idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              model <- io $ readIORef (modKnown V.! idx)
              if B.null (model^.mName)
                then return $ Just (modKnown V.! idx)
                else findEmptySpot modKnown (idx + 1) maxIdx

loadAliasModel :: IORef ModelT -> B.ByteString -> Quake ()
loadAliasModel modelRef buffer = do
    let lazyBuffer = BL.fromStrict buffer
        pheader = newDMdlT lazyBuffer

    model <- io $ readIORef modelRef

    checkForErrors pheader (model^.mName)

    -- load base s and t vertices (not used in gl version)
    let pOutST = runGet (V.replicateM (pheader^.dmNumST) getDSTVertT) (BL.drop (fromIntegral $ pheader^.dmOfsST) lazyBuffer)

    -- load triangle lists
    let pOutTri = runGet (V.replicateM (pheader^.dmNumTris) getDTriangleT) (BL.drop (fromIntegral $ pheader^.dmOfsTris) lazyBuffer)

    -- load the frames
    let pOutFrame = runGet (V.replicateM (pheader^.dmNumFrames) (getDAliasFrameT (pheader^.dmNumXYZ))) (BL.drop (fromIntegral $ pheader^.dmOfsFrames) lazyBuffer)

    -- load the glcmds
    let pOutCmd = runGet (UV.replicateM (pheader^.dmNumGlCmds) getWord32le) (BL.drop (fromIntegral $ pheader^.dmOfsGlCmds) lazyBuffer)

    -- register all skins
    let skinNames = V.map (B.takeWhile (/= 0)) $ runGet (V.replicateM (pheader^.dmNumSkins) (getByteString Constants.maxSkinName)) (BL.drop (fromIntegral $ pheader^.dmOfsSkins) lazyBuffer)
    skins <- V.mapM (\name -> Image.glFindImage name RenderAPIConstants.itSkin) skinNames

    let pheader' = pheader { _dmSkinNames   = Just skinNames
                           , _dmSTVerts     = Just pOutST
                           , _dmTriAngles   = Just pOutTri
                           , _dmGlCmds      = Just pOutCmd
                           , _dmAliasFrames = Just pOutFrame
                           }

    pheader'' <- precompileGLCmds pheader'

    io $ modifyIORef' modelRef (\v -> v { _mType = RenderAPIConstants.modAlias
                                        , _mExtraData = Just (AliasModelExtra pheader'')
                                        , _mMins = V3 (-32) (-32) (-32)
                                        , _mMaxs = V3 32 32 32
                                        , _mSkins = skins
                                        })

  where checkForErrors :: DMdlT -> B.ByteString -> Quake ()
        checkForErrors pheader modelName = do
          when ((pheader^.dmVersion) /= Constants.aliasVersion) $
            Com.comError Constants.errDrop (modelName `B.append` " has wrong version number (" `B.append` BC.pack (show (pheader^.dmVersion)) `B.append` " should be " `B.append` BC.pack (show Constants.aliasVersion) `B.append` ")") -- IMPROVE ?

          when ((pheader^.dmSkinHeight) > RenderAPIConstants.maxLBMHeight) $
            Com.comError Constants.errDrop ("model " `B.append` modelName `B.append` " has a skin taller than " `B.append` BC.pack (show RenderAPIConstants.maxLBMHeight))

          when ((pheader^.dmNumXYZ) <= 0) $
            Com.comError Constants.errDrop ("model " `B.append` modelName `B.append` " has no vertices")

          when ((pheader^.dmNumXYZ) > Constants.maxVerts) $
            Com.comError Constants.errDrop ("model " `B.append` modelName `B.append` " has too many vertices")

          when ((pheader^.dmNumST) <= 0) $
            Com.comError Constants.errDrop ("model " `B.append` modelName `B.append` " has no st vertices")

          when ((pheader^.dmNumTris) <= 0) $
            Com.comError Constants.errDrop ("model " `B.append` modelName `B.append` " has no triangles")

          when ((pheader^.dmNumFrames) <= 0) $
            Com.comError Constants.errDrop ("model " `B.append` modelName `B.append` " has no frames")

loadSpriteModel :: IORef ModelT -> B.ByteString -> Quake ()
loadSpriteModel modelRef buffer = do
    let sprOut = newDSpriteT (BL.fromStrict buffer)

    when ((sprOut^.dsVersion) /= spriteVersion) $ do
      model <- io $ readIORef modelRef
      Com.comError Constants.errDrop ((model^.mName) `B.append` " has wrong version number (" `B.append` BC.pack (show $ sprOut^.dsVersion) `B.append` " should be " `B.append` BC.pack (show spriteVersion) `B.append` ")") -- IMPROVE?

    when ((sprOut^.dsNumFrames) > Constants.maxMd2Skins) $ do
      model <- io $ readIORef modelRef
      Com.comError Constants.errDrop ((model^.mName) `B.append` " has too many frames (" `B.append` BC.pack (show $ sprOut^.dsNumFrames) `B.append` " > " `B.append` BC.pack (show Constants.maxMd2Skins) `B.append` ")") -- IMPROVE?

    skins <- V.mapM (\frame -> Image.glFindImage (frame^.dsfName) RenderAPIConstants.itSprite) (sprOut^.dsFrames)

    io $ modifyIORef' modelRef (\v -> v { _mSkins = skins
                                        , _mType = RenderAPIConstants.modSprite
                                        , _mExtraData = Just (SpriteModelExtra sprOut)
                                        })

loadBrushModel :: IORef ModelT -> B.ByteString -> Quake ()
loadBrushModel modelRef buffer = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    io $ modifyIORef' loadModelRef (\v -> v { _mType = RenderAPIConstants.modBrush })

    Just modKnown0 <- preuse $ fastRenderAPIGlobals.frModKnown.ix 0
    when (loadModelRef /= modKnown0) $
      Com.comError Constants.errDrop "Loaded a brush model after the world"

    let header = newDHeaderT (BL.fromStrict buffer)

    when ((header^.dhVersion) /= Constants.bspVersion) $ do
      model <- io $ readIORef modelRef
      Com.comError Constants.errDrop ("Mod_LoadBrushModel: " `B.append` (model^.mName) `B.append` " has wrong version number (" `B.append` BC.pack (show (header^.dhVersion)) `B.append` " should be " `B.append` BC.pack (show Constants.bspVersion) `B.append` ")")

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

    io $ modifyIORef' modelRef (\v -> v { _mNumFrames = 2 }) -- regular and alternate animation

    -- set up the submodels
    model <- io $ readIORef modelRef
    setupSubmodels model 0 (model^.mNumSubModels)

  where setupSubmodels :: ModelT -> Int -> Int -> Quake ()
        setupSubmodels model idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

              bm <- io $ readIORef $ (model^.mSubModels) V.! idx
              loadModel <- io $ readIORef loadModelRef
              
              starModRef <- io $ newIORef loadModel
              fastRenderAPIGlobals.frModInline.ix idx .= starModRef

              io $ modifyIORef' starModRef (\v -> v { _mFirstModelSurface = bm^.mmFirstFace
                                                    , _mNumModelSurfaces = bm^.mmNumFaces
                                                    , _mFirstNode = bm^.mmHeadNode
                                                    , _mMaxs = bm^.mmMaxs
                                                    , _mMins = bm^.mmMins
                                                    , _mRadius = bm^.mmRadius
                                                    })

              when ((bm^.mmHeadNode) >= (loadModel^.mNumNodes)) $
                Com.comError Constants.errDrop ("Inline model " `B.append` BC.pack (show idx) `B.append` " has bad firstnode") -- IMPROVE?

              when (idx == 0) $ do
                starMod <- io $ readIORef starModRef
                loadModelRef' <- io $ newIORef starMod
                fastRenderAPIGlobals.frLoadModel .= loadModelRef'

              io $ modifyIORef' starModRef (\v -> v { _mNumLeafs = bm^.mmVisLeafs })

              setupSubmodels model (idx + 1) maxIdx

loadVertexes :: B.ByteString -> LumpT -> Quake ()
loadVertexes buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` mVertexDiskSize /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` mVertexDiskSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        vertexes = runGet (getVertexes count) buf

    vertexes' <- io $ V.mapM newIORef vertexes

    io $ modifyIORef' loadModelRef (\v -> v { _mNumVertexes = count
                                            , _mVertexes = vertexes'
                                            })

  where getVertexes :: Int -> Get (V.Vector MVertexT)
        getVertexes count = V.replicateM count getMVertexT

loadEdges :: B.ByteString -> LumpT -> Quake ()
loadEdges buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` mEdgeDiskSize /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` mEdgeDiskSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        edges = runGet (getEdges count) buf

    edges' <- io $ V.mapM newIORef edges

    io $ modifyIORef' loadModelRef (\v -> v { _mNumEdges = count
                                            , _mEdges = edges'
                                            })

  where getEdges :: Int -> Get (V.Vector MEdgeT)
        getEdges count = V.replicateM count getMEdgeT

loadSurfEdges :: B.ByteString -> LumpT -> Quake ()
loadSurfEdges buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` Constants.sizeOfInt /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` Constants.sizeOfInt

    when (count < 1 || count >= Constants.maxMapSurfEdges) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel bad surfedges count in " `B.append` (model^.mName) `B.append` ": " `B.append` BC.pack (show count)) -- IMPROVE?

    let buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        offsets = runGet (getOffsets count) buf

    io $ modifyIORef' loadModelRef (\v -> v { _mNumSurfEdges = count
                                            , _mSurfEdges = offsets
                                            })

  where getOffsets :: Int -> Get (V.Vector Int)
        getOffsets count = V.replicateM count getInt

loadLighting :: B.ByteString -> LumpT -> Quake ()
loadLighting buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    let lightData = if (lump^.lFileLen) == 0
                      then Nothing
                      else Just (B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer))

    io $ modifyIORef' loadModelRef (\v -> v { _mLightdata = lightData })

loadPlanes :: B.ByteString -> LumpT -> Quake ()
loadPlanes buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` dPlaneTSize /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` dPlaneTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dplanes = runGet (getDPlanes count) buf

    planes <- io $ V.mapM toCPlane dplanes

    io $ modifyIORef' loadModelRef (\v -> v { _mNumPlanes = count
                                            , _mPlanes = planes
                                            })

  where getDPlanes :: Int -> Get (V.Vector DPlaneT)
        getDPlanes count = V.replicateM count getDPlaneT

        toCPlane :: DPlaneT -> IO (IORef CPlaneT)
        toCPlane dPlaneT = newIORef CPlaneT { _cpNormal = dPlaneT^.dpNormal
                                            , _cpDist = dPlaneT^.dpDist
                                            , _cpType = fromIntegral (dPlaneT^.dpType)
                                            , _cpSignBits = flagBits (dPlaneT^.dpNormal)
                                            , _cpPad = (0, 0)
                                            }

        flagBits :: V3 Float -> Int8
        flagBits (V3 a b c) =
          let a' :: Int8 = if a < 0 then 1 `shiftL` 0 else 0
              b' :: Int8 = if b < 0 then 1 `shiftL` 1 else 0
              c' :: Int8 = if c < 0 then 1 `shiftL` 2 else 0
          in a' .|. b' .|. c'

loadTexInfo :: B.ByteString -> LumpT -> Quake ()
loadTexInfo buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` texInfoTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        texInfoT = runGet (getTexInfo count) buf

    mti <- io $ V.replicateM count (newIORef undefined)
    io $ modifyIORef' loadModelRef (\v -> v { _mNumTexInfo = count
                                            , _mTexInfo = mti
                                            })

    model <- io $ readIORef loadModelRef
    V.imapM_ (toMTexInfoT model) texInfoT
    countAnimationFrames model 0 count

  where getTexInfo :: Int -> Get (V.Vector TexInfoT)
        getTexInfo count = V.replicateM count getTexInfoT

        toMTexInfoT :: ModelT -> Int -> TexInfoT -> Quake ()
        toMTexInfoT model idx texInfoT = do
          let name = "textures/" `B.append` (texInfoT^.tiTexture) `B.append` ".wal"
          foundImage <- Image.glFindImage name RenderAPIConstants.itWall
          imgRef <- case foundImage of
                      Just ref -> return ref
                      Nothing -> do
                        VID.printf Constants.printAll ("Couldn't load " `B.append` name `B.append` "\n")
                        use $ fastRenderAPIGlobals.frNoTexture

          let mTexInfoRef = (model^.mTexInfo) V.! idx
          io $ writeIORef mTexInfoRef MTexInfoT { _mtiVecs = texInfoT^.tiVecs
                                                , _mtiFlags = texInfoT^.tiFlags
                                                , _mtiNumFrames = 1
                                                , _mtiNext = if (texInfoT^.tiNextTexInfo) > 0 then Just ((model^.mTexInfo) V.! (texInfoT^.tiNextTexInfo)) else Nothing
                                                , _mtiImage = Just imgRef
                                                }

        countAnimationFrames :: ModelT -> Int -> Int -> Quake ()
        countAnimationFrames model idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let mTexInfoRef = (model^.mTexInfo) V.! idx
              texInfo <- io $ readIORef mTexInfoRef
              num <- countFrames mTexInfoRef (texInfo^.mtiNext) 1
              io $ modifyIORef' mTexInfoRef (\v -> v { _mtiNumFrames = num })
              countAnimationFrames model (idx + 1) maxIdx

        countFrames :: IORef MTexInfoT -> Maybe (IORef MTexInfoT) -> Int -> Quake Int
        countFrames _ Nothing n = return n
        countFrames initial (Just current) n = do
          if current == initial
            then return n
            else do
              texInfo <- io $ readIORef current
              countFrames initial (texInfo^.mtiNext) (n + 1)

loadFaces :: B.ByteString -> LumpT -> Quake ()
loadFaces buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` dFaceTSize /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` dFaceTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dFaces = runGet (getDFaces count) buf

    fastRenderAPIGlobals.frCurrentModel .= Just loadModelRef
    Surf.glBeginBuildingLightmaps loadModelRef

    surfaces <- V.mapM toMSurfaceT dFaces

    io $ modifyIORef' loadModelRef (\v -> v { _mNumSurfaces = count
                                            , _mSurfaces = surfaces
                                            })

    Surf.glEndBuildingLightmaps
      
  where getDFaces :: Int -> Get (V.Vector DFaceT)
        getDFaces count = V.replicateM count getDFaceT

        toMSurfaceT :: DFaceT -> Quake (IORef MSurfaceT)
        toMSurfaceT dface = do
          loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel
          model <- io $ readIORef loadModelRef

          let ti = fromIntegral (dface^.dfTexInfo)

          when (ti < 0 || ti >= (model^.mNumTexInfo)) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: bad texinfo number"

          let planeNum = fromIntegral (dface^.dfPlaneNum)
              side = dface^.dfSide
              flags = if side /= 0 then Constants.surfPlaneBack else 0
              i = dface^.dfLightOfs
              texInfoRef = (model^.mTexInfo) V.! ti

          texInfo <- io $ readIORef texInfoRef

          let texInfoFlags = texInfo^.mtiFlags
          surfRef <- io $ newIORef newMSurfaceT { _msFirstEdge = dface^.dfFirstEdge
                                                , _msNumEdges = fromIntegral (dface^.dfNumEdges)
                                                , _msFlags = flags
                                                , _msPolys = Nothing
                                                , _msPlane = Just ((model^.mPlanes) V.! planeNum)
                                                , _msTexInfo = texInfo
                                                , _msStyles = dface^.dfStyles -- TODO: should we limit it by Constants.maxLightMaps ?
                                                , _msSamples = if i == -1 then Nothing else Just (B.drop i (fromJust (model^.mLightdata)))
                                                                          }
          calcSurfaceExtents model texInfo surfRef

          when (texInfoFlags .&. Constants.surfWarp /= 0) $ do
            io $ modifyIORef' surfRef (\v -> v { _msFlags = flags .|. Constants.surfWarp
                                               , _msExtents = (16384, 16384)
                                               , _msTextureMins = (-8192, -8192)
                                               })
            Warp.glSubdivideSurface surfRef

          when (texInfoFlags .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) == 0) $
            Surf.glCreateSurfaceLightmap surfRef

          when (texInfoFlags .&. Constants.surfWarp == 0) $
            Surf.glBuildPolygonFromSurface surfRef

          return surfRef

calcSurfaceExtents :: ModelT -> MTexInfoT -> IORef MSurfaceT -> Quake ()
calcSurfaceExtents model texInfo surfRef = do
    surf <- io $ readIORef surfRef
    (mins, maxs) <- io $ calcMinsMaxs surf 0 (surf^.msNumEdges) (999999, 999999) (-99999, -99999)
    let bmins = mapTuple (floor . (/ 16)) mins
        bmaxs = mapTuple (ceiling . (/ 16)) maxs

    io $ modifyIORef' surfRef (\v -> v { _msTextureMins = mapTuple (* 16) bmins
                                       , _msExtents = mapTuple (* 16) (fst bmaxs - fst bmins, snd bmaxs - snd bmins)
                                       })

  where calcMinsMaxs :: MSurfaceT -> Int -> Int -> (Float, Float) -> (Float, Float) -> IO ((Float, Float), (Float, Float))
        calcMinsMaxs surf idx maxIdx mins maxs
          | idx >= maxIdx = return (mins, maxs)
          | otherwise = do
              let e = (model^.mSurfEdges) V.! ((surf^.msFirstEdge) + idx)
              v <- if e >= 0
                     then do
                       let edgeRef = (model^.mEdges) V.! e
                       edge <- io $ readIORef edgeRef
                       let vertexRef = (model^.mVertexes) V.! (fromIntegral $ edge^.meV._1)
                       io $ readIORef vertexRef
                     else do
                       let edgeRef = (model^.mEdges) V.! (negate e)
                       edge <- io $ readIORef edgeRef
                       let vertexRef = (model^.mVertexes) V.! (fromIntegral $ edge^.meV._2)
                       io $ readIORef vertexRef
              let V3 a b c = v^.mvPosition
                  (V4 a1 b1 c1 d1, V4 a2 b2 c2 d2) = texInfo^.mtiVecs
                  val1 = a * a1 + b * b1 + c * c1 + d1
                  val2 = a * a2 + b * b2 + c * c2 + d2
                  mins' = (if val1 < fst mins then val1 else fst mins, if val2 < snd mins then val2 else snd mins)
                  maxs' = (if val1 > fst maxs then val1 else fst maxs, if val2 > snd maxs then val2 else snd maxs)

              calcMinsMaxs surf (idx + 1) maxIdx mins' maxs'

        mapTuple :: (a -> b) -> (a, a) -> (b, b)
        mapTuple f (a1, a2) = (f a1, f a2)

loadMarkSurfaces :: B.ByteString -> LumpT -> Quake ()
loadMarkSurfaces buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel
    model <- io $ readIORef loadModelRef

    when ((lump^.lFileLen) `mod` Constants.sizeOfShort /= 0) $ do
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` Constants.sizeOfShort
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        surfaceIndexes = runGet (getSurfaceIndexes count) buf

    markSurfaces <- V.mapM (toMSurfaceT model) surfaceIndexes

    io $ modifyIORef' loadModelRef (\v -> v { _mNumMarkSurfaces = count
                                            , _mMarkSurfaces = markSurfaces
                                            })

  where getSurfaceIndexes :: Int -> Get (V.Vector Word16)
        getSurfaceIndexes count = V.replicateM count getWord16le

        toMSurfaceT :: ModelT -> Word16 -> Quake (IORef MSurfaceT)
        toMSurfaceT model idx = do
          let i = fromIntegral idx
          when (i < 0 || i >= (model^.mNumSurfaces)) $
            Com.comError Constants.errDrop "Mod_ParseMarksurfaces: bad surface number"
          return $ (model^.mSurfaces) V.! i

loadVisibility :: B.ByteString -> LumpT -> Quake ()
loadVisibility buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    if (lump^.lFileLen) == 0
      then
        io $ modifyIORef' loadModelRef (\v -> v { _mVis = Nothing })
      else do
        let buf = B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
            buf' = BL.fromStrict buf
            vis = newDVisT buf'

        fastRenderAPIGlobals.frModelVisibility .= Just buf
        io $ modifyIORef' loadModelRef (\v -> v { _mVis = Just vis })

loadLeafs :: B.ByteString -> LumpT -> Quake ()
loadLeafs buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel
    model <- io $ readIORef loadModelRef

    when ((lump^.lFileLen) `mod` dLeafTSize /= 0) $ do
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` dLeafTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dLeafs = runGet (getDLeafs count) buf
        leafs = V.map (toMLeafT model) dLeafs

    leafs' <- io $ V.mapM newIORef leafs

    io $ modifyIORef' loadModelRef (\v -> v { _mNumLeafs = count
                                            , _mLeafs = leafs'
                                            })

  where getDLeafs :: Int -> Get (V.Vector DLeafT)
        getDLeafs count = V.replicateM count getDLeafT

        toMLeafT :: ModelT -> DLeafT -> MLeafT
        toMLeafT model dLeaf = MLeafT { _mlContents        = dLeaf^.dlContents
                                      , _mlVisFrame        = 0
                                      , _mlMins            = fmap fromIntegral (dLeaf^.dlMins)
                                      , _mlMaxs            = fmap fromIntegral (dLeaf^.dlMaxs)
                                      , _mlParent          = Nothing
                                      , _mlCluster         = fromIntegral (dLeaf^.dlCluster)
                                      , _mlArea            = fromIntegral (dLeaf^.dlArea)
                                      , _mlNumMarkSurfaces = fromIntegral (dLeaf^.dlNumLeafFaces)
                                      , _mlMarkIndex       = fromIntegral (dLeaf^.dlFirstLeafFace)
                                      }

loadNodes :: B.ByteString -> LumpT -> Quake ()
loadNodes buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` dNodeTSize /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` dNodeTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dNodes = runGet (getDNodes count) buf

    nodes <- io $ V.replicateM count (newIORef undefined)
    io $ modifyIORef' loadModelRef (\v -> v { _mNumNodes = count
                                            , _mNodes = nodes
                                            })

    model <- io $ readIORef loadModelRef
    V.imapM_ (toMNodeT model) dNodes

    io $ setParent (MNodeChildReference $ (model^.mNodes) V.! 0) Nothing -- sets nodes and leafs

  where getDNodes :: Int -> Get (V.Vector DNodeT)
        getDNodes count = V.replicateM count getDNodeT

        toMNodeT :: ModelT -> Int -> DNodeT -> Quake ()
        toMNodeT loadModel idx dNode = do
          let nodeRef = (loadModel^.mNodes) V.! idx

          io $ writeIORef nodeRef MNodeT { _mnContents     = (-1) -- differentiate from leafs
                                         , _mnVisFrame     = 0
                                         , _mnMins         = fmap fromIntegral (dNode^.dnMins)
                                         , _mnMaxs         = fmap fromIntegral (dNode^.dnMaxs)
                                         , _mnParent       = Nothing
                                         , _mnPlane        = (loadModel^.mPlanes) V.! (dNode^.dnPlaneNum)
                                         , _mnChildren     = getChildren loadModel (dNode^.dnChildren)
                                         , _mnFirstSurface = fromIntegral (dNode^.dnFirstFace)
                                         , _mnNumSurfaces  = fromIntegral (dNode^.dnNumFaces)
                                         }

        getChildren :: ModelT -> (Int, Int) -> (MNodeChild, MNodeChild)
        getChildren loadModel (p1, p2) =
          let a = if p1 >= 0
                    then MNodeChildReference ((loadModel^.mNodes) V.! p1)
                    else MLeafChildReference ((loadModel^.mLeafs) V.! ((-1) - p1))
              b = if p2 >= 0
                    then MNodeChildReference ((loadModel^.mNodes) V.! p2)
                    else MLeafChildReference ((loadModel^.mLeafs) V.! ((-1) - p2))
          in (a, b)

setParent :: MNodeChild -> Maybe (IORef MNodeT) -> IO ()
setParent child parent =
    case child of
      MNodeChildReference nodeRef -> do
        modifyIORef' nodeRef (\v -> v { _mnParent = parent })
        node <- readIORef nodeRef
        setParent (node^.mnChildren._1) (Just nodeRef)
        setParent (node^.mnChildren._2) (Just nodeRef)
      MLeafChildReference leafRef ->
        modifyIORef' leafRef (\v -> v { _mlParent = parent })

loadSubmodels :: B.ByteString -> LumpT -> Quake ()
loadSubmodels buffer lump = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` dModelTSize /= 0) $ do
      model <- io $ readIORef loadModelRef
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` dModelTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dModels = runGet (getDModels count) buf
        models = V.map toMModelT dModels

    models' <- io $ V.mapM newIORef models

    io $ modifyIORef' loadModelRef (\v -> v { _mNumSubModels = count
                                            , _mSubModels = models'
                                            })

  where getDModels :: Int -> Get (V.Vector DModelT)
        getDModels count = V.replicateM count getDModelT

        toMModelT :: DModelT -> MModelT
        toMModelT dModel =
          -- spread the mins / maxs by a pixel
          let mins = fmap (subtract 1) (dModel^.dmMins)
              maxs = fmap (+1) (dModel^.dmMaxs)
          in MModelT { _mmMins      = mins
                     , _mmMaxs      = maxs
                     , _mmOrigin    = dModel^.dmOrigin
                     , _mmRadius    = radiusFromBounds mins maxs
                     , _mmHeadNode  = dModel^.dmHeadNode
                     , _mmVisLeafs  = 0
                     , _mmFirstFace = dModel^.dmFirstFace
                     , _mmNumFaces  = dModel^.dmNumFaces
                     }

        radiusFromBounds :: V3 Float -> V3 Float -> Float
        radiusFromBounds mins maxs =
          let a = if abs (mins^._x) > abs (maxs^._x) then abs (mins^._x) else abs (maxs^._x)
              b = if abs (mins^._y) > abs (maxs^._y) then abs (mins^._y) else abs (maxs^._y)
              c = if abs (mins^._z) > abs (maxs^._z) then abs (mins^._z) else abs (maxs^._z)
          in norm (V3 a b c)

precompileGLCmds :: DMdlT -> Quake DMdlT
precompileGLCmds model = do
    textureBuf <- use $ fastRenderAPIGlobals.frModelTextureCoordBuf
    vertexBuf <- use $ fastRenderAPIGlobals.frModelVertexIndexBuf
    modelTextureCoordIdx <- use $ fastRenderAPIGlobals.frModelTextureCoordIdx
    modelVertexIndexIdx <- use $ fastRenderAPIGlobals.frModelVertexIndexIdx

    -- tmp is in reversed order, this should be taken into accounts in the
    -- next calculations and assignments
    (tmp, modelTextureCoordIdx', modelVertexIndexIdx') <- setTextureAndVertex textureBuf vertexBuf (fromJust $ model^.dmGlCmds) modelTextureCoordIdx modelVertexIndexIdx 0 []

    let counts = UV.fromList (reverse tmp)
        indexElements = collectIndexElements tmp modelVertexIndexIdx 0 []

    zoom fastRenderAPIGlobals $ do
      frModelTextureCoordIdx .= modelTextureCoordIdx'
      frModelVertexIndexIdx .= modelVertexIndexIdx'

    return model { _dmTextureCoordBufIdx = modelTextureCoordIdx
                 , _dmVertexIndexBufIdx = modelVertexIndexIdx
                 , _dmCounts = counts
                 , _dmIndexElements = indexElements
                 }

  where setTextureAndVertex :: MSV.IOVector Float -> MSV.IOVector Int32 -> UV.Vector Word32 -> Int -> Int -> Int -> [Int32] -> Quake ([Int32], Int, Int)
        setTextureAndVertex textureBuf vertexBuf order textureCoordIdx vertexIndexIdx orderIndex tmp = do
          let count :: Int32 = fromIntegral (order UV.! orderIndex)

          if (count == 0)
            then do
              let count' = if count < 0 then -count else count
              (textureCoordIdx', vertexIndexIdx', orderIndex') <- setCoords textureBuf vertexBuf order textureCoordIdx vertexIndexIdx orderIndex count'
              setTextureAndVertex textureBuf vertexBuf order textureCoordIdx' vertexIndexIdx' orderIndex' (count : tmp)
            else
              return (tmp, textureCoordIdx, vertexIndexIdx)

        setCoords :: MSV.IOVector Float -> MSV.IOVector Int32 -> UV.Vector Word32 -> Int -> Int -> Int -> Int32 -> Quake (Int, Int, Int)
        setCoords textureBuf vertexBuf order textureCoordIdx vertexIndexIdx orderIndex count
          | count <= 0 = return (textureCoordIdx, vertexIndexIdx, orderIndex)
          | otherwise = do
              io $ MSV.write textureBuf textureCoordIdx (wordToFloat $ order UV.! orderIndex)
              io $ MSV.write textureBuf (textureCoordIdx + 1) (wordToFloat $ order UV.! (orderIndex + 1))
              io $ MSV.write vertexBuf vertexIndexIdx (fromIntegral $ order UV.! (orderIndex + 2))
              setCoords textureBuf vertexBuf order (textureCoordIdx + 2) (vertexIndexIdx + 1) (orderIndex + 3) (count - 1)
              
        collectIndexElements :: [Int32] -> Int -> Int -> [(Int, Int)] -> V.Vector (Int, Int)
        collectIndexElements [] _ _ acc = V.fromList acc
        collectIndexElements (x:xs) idx pos acc =
          let count = fromIntegral $ if x < 0 then -x else x
          in collectIndexElements xs idx (pos + count) ((idx + pos, count) : acc)

rRegisterModel :: B.ByteString -> Quake (Maybe (IORef ModelT))
rRegisterModel name = do
    modelRef <- modForName name False

    when (isJust modelRef) $
      registerModelImages (fromJust modelRef)

    return modelRef

  where registerModelImages :: IORef ModelT -> Quake ()
        registerModelImages modelRef = do
          model <- io $ readIORef modelRef

          regSeq <- use $ fastRenderAPIGlobals.frRegistrationSequence
          io $ modifyIORef' modelRef (\v -> v { _mRegistrationSequence = regSeq })

          if | model^.mType == RenderAPIConstants.modSprite -> do
                 let Just (SpriteModelExtra sprOut) = model^.mExtraData
                 skins <- V.mapM (\frame -> Image.glFindImage (frame^.dsfName) RenderAPIConstants.itSprite) (sprOut^.dsFrames)
                 io $ modifyIORef' modelRef (\v -> v { _mSkins = skins })

             | model^.mType == RenderAPIConstants.modAlias -> do
                 let Just (AliasModelExtra pheader) = model^.mExtraData
                 skins <- V.mapM (\skinName -> Image.glFindImage skinName RenderAPIConstants.itSkin) (fromJust $ pheader^.dmSkinNames)
                 io $ modifyIORef' modelRef (\v -> v { _mSkins = skins
                                                     , _mNumFrames = pheader^.dmNumFrames
                                                     })

             | model^.mType == RenderAPIConstants.modBrush -> do
                 updateImageRegistrationSequence model regSeq 0 (model^.mNumTexInfo)

             | otherwise -> return ()

        updateImageRegistrationSequence :: ModelT -> Int -> Int -> Int -> Quake ()
        updateImageRegistrationSequence model regSeq idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              texInfo <- io $ readIORef ((model^.mTexInfo) V.! idx)
              io $ modifyIORef' (fromJust $ texInfo^.mtiImage) (\v -> v { _iRegistrationSequence = regSeq })
              updateImageRegistrationSequence model regSeq (idx + 1) maxIdx

rEndRegistration :: Quake ()
rEndRegistration = do
    modNumKnown <- use $ fastRenderAPIGlobals.frModNumKnown
    modKnown <- use $ fastRenderAPIGlobals.frModKnown
    regSeq <- use $ fastRenderAPIGlobals.frRegistrationSequence

    checkModels modKnown regSeq 0 modNumKnown

    Image.glFreeUnusedImages

  where checkModels :: V.Vector (IORef ModelT) -> Int -> Int -> Int -> Quake ()
        checkModels modKnown regSeq idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              model <- io $ readIORef (modKnown V.! idx)

              if | B.null (model^.mName) -> return ()
                 | (model^.mRegistrationSequence) /= regSeq -> modFree (modKnown V.! idx)
                 | otherwise -> when ((model^.mType) == RenderAPIConstants.modAlias) $ do
                                  let Just (AliasModelExtra pheader) = model^.mExtraData
                                  pheader' <- precompileGLCmds pheader
                                  io $ modifyIORef' (modKnown V.! idx) (\v -> v { _mExtraData = Just (AliasModelExtra pheader') })

              checkModels modKnown regSeq (idx + 1) maxIdx

pointInLeaf :: V3 Float -> ModelT -> Quake MLeafT
pointInLeaf p model = do
    let rootNode = MNodeChildReference ((model^.mNodes) V.! 0)
    findLeaf rootNode

  where findLeaf :: MNodeChild -> Quake MLeafT
        findLeaf (MNodeChildReference nodeRef) = do
          node <- io $ readIORef nodeRef
          plane <- io $ readIORef (node^.mnPlane)

          let d = p `dot` (plane^.cpNormal) - (plane^.cpDist)
              childRef = if d > 0
                           then node^.mnChildren._1
                           else node^.mnChildren._2

          case childRef of
            MLeafChildReference leafRef -> io $ readIORef leafRef
            nodeChild -> findLeaf nodeChild

clusterPVS :: Int -> ModelT -> Quake B.ByteString
clusterPVS cluster model = do
    if cluster == -1 || isNothing (model^.mVis)
      then
        use $ fastRenderAPIGlobals.frModNoVis
      else do
        modelVisibility <- use $ fastRenderAPIGlobals.frModelVisibility
        let vis = fromJust (model^.mVis)
        -- TODO: instead of directly using _1 we should somehow use Constants.dvisPvs
        return $ decompressVis modelVisibility (((vis^.dvBitOfs) V.! cluster)^._1) model

decompressVis :: Maybe B.ByteString -> Int -> ModelT -> B.ByteString
decompressVis maybeModelVisibility offset model =
    let vis = fromJust (model^.mVis)
        row = ((vis^.dvNumClusters) + 7) `shiftR` 3
    in case maybeModelVisibility of
         Nothing -> B.unfoldr (\idx -> if | idx >= Constants.maxMapLeafs `div` 8 -> Nothing
                                          | idx < row -> Just (0xFF, idx + 1)
                                          | otherwise -> Just (0, idx + 1)

                              ) 0
         Just modelVisibility -> buildVis modelVisibility row offset 0 mempty

  where buildVis :: B.ByteString -> Int -> Int -> Int -> BB.Builder -> B.ByteString
        buildVis modelVisibility row inp outp builder =
          if (modelVisibility `B.index` inp) /= 0
            then let builder' = builder <> BB.word8 (modelVisibility `B.index` inp)
                 in if outp + 1 < row
                      then buildVis modelVisibility row (inp + 1) (outp + 1) builder'
                      else let result = BL.toStrict (BB.toLazyByteString builder')
                               diff = Constants.maxMapLeafs `div` 8 - (B.length result)
                           in if diff > 0
                                then result `B.append` (B.replicate diff 0)
                                else result
            else let c = modelVisibility `B.index` (inp + 1)
                     builder' = buildEmpty builder c
                 in if outp + fromIntegral c < row
                      then buildVis modelVisibility row (inp + 2) (outp + fromIntegral c) builder'
                      else let result = BL.toStrict (BB.toLazyByteString builder')
                               diff = Constants.maxMapLeafs `div` 8 - (B.length result)
                           in if diff > 0
                                then result `B.append` (B.replicate diff 0)
                                else result

        buildEmpty :: BB.Builder -> Word8 -> BB.Builder
        buildEmpty builder c
          | c <= 0 = builder
          | otherwise = buildEmpty (builder <> (BB.word8 0)) (c - 1)
