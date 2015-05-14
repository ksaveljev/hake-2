{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Model where

import Control.Lens ((.=), (+=), preuse, ix, (^.), zoom, use, (%=))
import Control.Monad (when, liftM)
import Data.Bits ((.|.), (.&.), shiftL)
import Data.Int (Int8)
import Data.Maybe (isNothing, fromJust)
import Data.Word (Word16)
import Linear (V3(..), V4(..), norm, _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.QFiles.BSP.DFaceT
import QCommon.QFiles.BSP.DHeaderT
import QCommon.QFiles.BSP.DLeafT
import QCommon.QFiles.BSP.DModelT
import QCommon.QFiles.BSP.DNodeT
import QCommon.QFiles.BSP.DPlaneT
import QCommon.QFiles.MD2.DMdlT
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
import qualified Render.Fast.Surf as Surf
import qualified Render.Fast.Warp as Warp
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Util.Lib as Lib

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO

modInit :: Quake ()
modInit = do
    -- init mod_known
    fastRenderAPIGlobals.frModKnown .= V.replicate maxModKnown newModelT
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

    fastRenderAPIGlobals.frModKnown.ix modelIdx.mNumFrames .= 2 -- regular and alternate animation

    -- set up the submodels
    Just model <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx
    setupSubmodels model loadModelIdx 0 (model^.mNumSubModels)

  where setupSubmodels :: ModelT -> Int -> Int -> Int -> Quake ()
        setupSubmodels model loadModelIdx idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let bm = (model^.mSubModels) V.! idx
              Just loadModel <- preuse $ fastRenderAPIGlobals.frModKnown.ix loadModelIdx

              let starMod = loadModel { _mFirstModelSurface = bm^.mmFirstFace
                                      , _mNumModelSurfaces = bm^.mmNumFaces
                                      , _mFirstNode = bm^.mmHeadNode
                                      , _mMaxs = bm^.mmMaxs
                                      , _mMins = bm^.mmMins
                                      , _mRadius = bm^.mmRadius
                                      }

              when (idx == 0) $
                fastRenderAPIGlobals.frModKnown.ix loadModelIdx .= starMod

              fastRenderAPIGlobals.frModInline.ix idx .= starMod { _mNumLeafs = bm^.mmVisLeafs }

              setupSubmodels model loadModelIdx (idx + 1) maxIdx

loadVertexes :: B.ByteString -> LumpT -> Quake ()
loadVertexes buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` mVertexDiskSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` mVertexDiskSize
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

    when ((lump^.lFileLen) `mod` mEdgeDiskSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` mEdgeDiskSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        edges = runGet (getEdges count) buf

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumEdges .= count
      mEdges .= edges

  where getEdges :: Int -> Get (V.Vector MEdgeT)
        getEdges count = V.replicateM count getMEdgeT

loadSurfEdges :: B.ByteString -> LumpT -> Quake ()
loadSurfEdges buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` Constants.sizeOfInt /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` Constants.sizeOfInt

    when (count < 1 || count >= Constants.maxMapSurfEdges) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel bad surfedges count in " `B.append` name `B.append` ": " `B.append` BC.pack (show count)) -- IMPROVE?

    let buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        offsets = runGet (getOffsets count) buf

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumSurfEdges .= count
      mSurfEdges .= offsets

  where getOffsets :: Int -> Get (V.Vector Int)
        getOffsets count = V.replicateM count getInt

loadLighting :: B.ByteString -> LumpT -> Quake ()
loadLighting buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    if (lump^.lFileLen) == 0
      then fastRenderAPIGlobals.frModKnown.ix modelIdx.mLightdata .= Nothing
      else fastRenderAPIGlobals.frModKnown.ix modelIdx.mLightdata .= Just (B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer))

loadPlanes :: B.ByteString -> LumpT -> Quake ()
loadPlanes buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` dPlaneTSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` dPlaneTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dplanes = runGet (getDPlanes count) buf
        planes = V.map toCPlane dplanes

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumPlanes .= count
      mPlanes .= planes

  where getDPlanes :: Int -> Get (V.Vector DPlaneT)
        getDPlanes count = V.replicateM count getDPlaneT

        toCPlane :: DPlaneT -> CPlaneT
        toCPlane dPlaneT = CPlaneT { _cpNormal = dPlaneT^.dpNormal
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
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` texInfoTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        texInfoT = runGet (getTexInfo count) buf

    mTexInfoT <- V.mapM toMTexInfoT texInfoT
    let mTexInfoT' = V.imap (countFrames mTexInfoT) mTexInfoT

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumTexInfo .= count
      mTexInfo .= mTexInfoT'

  where getTexInfo :: Int -> Get (V.Vector TexInfoT)
        getTexInfo count = V.replicateM count getTexInfoT

        toMTexInfoT :: TexInfoT -> Quake MTexInfoT
        toMTexInfoT texInfoT = do
          let name = "textures/" `B.append` (texInfoT^.tiTexture) `B.append` ".wal"
          foundImage <- Image.glFindImage name RenderAPIConstants.itWall
          imgRef <- case foundImage of
                      Just ref -> return ref
                      Nothing -> do
                        VID.printf Constants.printAll ("Couldn't load " `B.append` name `B.append` "\n")
                        use $ fastRenderAPIGlobals.frNoTexture

          return MTexInfoT { _mtiVecs = texInfoT^.tiVecs
                           , _mtiFlags = texInfoT^.tiFlags
                           , _mtiNumFrames = 1
                           , _mtiNext = if (texInfoT^.tiNextTexInfo) > 0 then Just (texInfoT^.tiNextTexInfo) else Nothing
                           , _mtiImage = Just imgRef
                           }

        countFrames :: V.Vector MTexInfoT -> Int -> MTexInfoT -> MTexInfoT
        countFrames allTexInfo idx current = current { _mtiNumFrames = countNumFrames allTexInfo idx current 1 }

        countNumFrames :: V.Vector MTexInfoT -> Int -> MTexInfoT -> Int -> Int
        countNumFrames allTexInfo idx current count =
          case current^.mtiNext of
            Nothing -> count
            Just nextIdx -> if nextIdx == idx
                              then count
                              else countNumFrames allTexInfo idx (allTexInfo V.! nextIdx) (count + 1)

loadFaces :: B.ByteString -> LumpT -> Quake ()
loadFaces buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` dFaceTSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` dFaceTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dFaces = runGet (getDFaces count) buf

    use (fastRenderAPIGlobals.frLoadModel) >>= \loadModel -> do
      fastRenderAPIGlobals.frCurrentModel .= loadModel
      Surf.glBeginBuildingLightmaps loadModel

    surfaces <- V.sequence $ V.map toMSurfaceT dFaces

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumSurfaces .= count
      mSurfaces .= surfaces

    Surf.glEndBuildingLightmaps
      
  where getDFaces :: Int -> Get (V.Vector DFaceT)
        getDFaces count = V.replicateM count getDFaceT

        toMSurfaceT :: DFaceT -> Quake MSurfaceT
        toMSurfaceT dface = do
          loadModel <- use $ fastRenderAPIGlobals.frLoadModel

          let ModKnownReference modelIdx = loadModel
              ti = fromIntegral (dface^.dfTexInfo)

          Just model <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx

          when (ti < 0 || ti >= (model^.mNumTexInfo)) $
            Com.comError Constants.errDrop "MOD_LoadBmodel: bad texinfo number"

          let planeNum = fromIntegral (dface^.dfPlaneNum)
              side = dface^.dfSide
              flags = if side /= 0 then Constants.surfPlaneback else 0
              i = dface^.dfLightOfs
              texInfoFlags = ((model^.mTexInfo) V.! ti)^.mtiFlags
              initialMSurfaceT = newMSurfaceT { _msFirstEdge = dface^.dfFirstEdge
                                              , _msNumEdges = fromIntegral (dface^.dfNumEdges)
                                              , _msFlags = flags
                                              , _msPolys = Nothing
                                              , _msPlane = Just ((model^.mPlanes) V.! planeNum) -- TODO: are we sure it is not a reference to (model, planeNum) ?
                                              , _msTexInfo = (model^.mTexInfo) V.! ti -- TODO: are we sure it is not a reference to (model, texinfo) ?
                                              , _msStyles = dface^.dfStyles -- TODO: should we limit it by Constants.maxLightMaps ?
                                              , _msSamples = if i == -1 then Nothing else Just (B.drop i (fromJust (model^.mLightdata)))
                                              }
              mSurfaceT = calcSurfaceExtents model ((model^.mTexInfo) V.! ti) initialMSurfaceT

          mSurfaceT' <- if texInfoFlags .&. Constants.surfWarp /= 0
                          then Warp.glSubdivideSurface mSurfaceT { _msFlags = flags .|. Constants.surfWarp, _msExtents = (16384, 16384), _msTextureMins = (-8192, -8192) }
                          else return mSurfaceT

          mSurfaceT'' <- if texInfoFlags .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) == 0
                           then Surf.glCreateSurfaceLightmap mSurfaceT'
                           else return mSurfaceT'

          if texInfoFlags .&. Constants.surfWarp == 0
            then Surf.glBuildPolygonFromSurface mSurfaceT''
            else return mSurfaceT''

calcSurfaceExtents :: ModelT -> MTexInfoT -> MSurfaceT -> MSurfaceT
calcSurfaceExtents model texInfo surface = 
    let (mins, maxs) = calcMinsMaxs 0 (surface^.msNumEdges) (999999, 999999) (-99999, -99999)
        bmins = mapTuple (floor . (/ 16)) mins
        bmaxs = mapTuple (ceiling . (/ 16)) maxs
    in surface { _msTextureMins = mapTuple (* 16) bmins
               , _msExtents = mapTuple (* 16) (fst bmaxs - fst bmins, snd bmaxs - snd bmins)
               }

  where calcMinsMaxs :: Int -> Int -> (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float))
        calcMinsMaxs idx maxIdx mins maxs
          | idx >= maxIdx = (mins, maxs)
          | otherwise =
              let e = (model^.mSurfEdges) V.! ((surface^.msFirstEdge) + idx)
                  v = if e >= 0
                        then (model^.mVertexes) V.! fromIntegral (fst (((model^.mEdges) V.! e)^.meV))
                        else (model^.mVertexes) V.! fromIntegral (snd (((model^.mEdges) V.! (-e))^.meV))
                  V3 a b c = v^.mvPosition
                  (V4 a1 b1 c1 d1, V4 a2 b2 c2 d2) = texInfo^.mtiVecs
                  val1 = a * a1 + b * b1 + c * c1 + d1
                  val2 = a * a2 + b * b2 + c * c2 + d2
                  mins' = (if val1 < fst mins then val1 else fst mins, if val2 < snd mins then val2 else snd mins)
                  maxs' = (if val1 > fst maxs then val1 else fst maxs, if val2 > snd maxs then val2 else snd maxs)
              in calcMinsMaxs (idx + 1) maxIdx mins' maxs'

        mapTuple :: (a -> b) -> (a, a) -> (b, b)
        mapTuple f (a1, a2) = (f a1, f a2)

loadMarkSurfaces :: B.ByteString -> LumpT -> Quake ()
loadMarkSurfaces buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel
    Just model <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx

    when ((lump^.lFileLen) `mod` Constants.sizeOfShort /= 0) $ do
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` Constants.sizeOfShort
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        surfaceIndexes = runGet (getSurfaceIndexes count) buf

    markSurfaces <- V.mapM (toMSurfaceT model) surfaceIndexes

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumMarkSurfaces .= count
      mMarkSurfaces .= markSurfaces

  where getSurfaceIndexes :: Int -> Get (V.Vector Word16)
        getSurfaceIndexes count = V.replicateM count getWord16le

        toMSurfaceT :: ModelT -> Word16 -> Quake MSurfaceT
        toMSurfaceT model idx = do
          let i = fromIntegral idx
          when (i < 0 || i >= (model^.mNumSurfaces)) $
            Com.comError Constants.errDrop "Mod_ParseMarksurfaces: bad surface number"
          return $ (model^.mSurfaces) V.! i

loadVisibility :: B.ByteString -> LumpT -> Quake ()
loadVisibility buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    if (lump^.lFileLen) == 0
      then
        fastRenderAPIGlobals.frModKnown.ix modelIdx.mVis .= Nothing
      else do
        let buf = B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
            buf' = BL.fromStrict buf
            vis = newDVisT buf'

        fastRenderAPIGlobals.frModelVisibility .= buf
        fastRenderAPIGlobals.frModKnown.ix modelIdx.mVis .= Just vis

loadLeafs :: B.ByteString -> LumpT -> Quake ()
loadLeafs buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel
    Just model <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx

    when ((lump^.lFileLen) `mod` dLeafTSize /= 0) $ do
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` dLeafTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dLeafs = runGet (getDLeafs count) buf
        leafs = V.map (toMLeafT model) dLeafs

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumLeafs .= count
      mLeafs .= leafs

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
                                      , _mlMarkSurfaces    = model^.mMarkSurfaces
                                      }

loadNodes :: B.ByteString -> LumpT -> Quake ()
loadNodes buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel
    Just model <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx

    when ((lump^.lFileLen) `mod` dNodeTSize /= 0) $ do
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))

    let count = (lump^.lFileLen) `div` dNodeTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dNodes = runGet (getDNodes count) buf
        nodes = V.map (toMNodeT model) dNodes
        (nodeUpdates, leafUpdates) = setParent nodes (model^.mLeafs) (MNodeChildReference 0) Nothing

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumNodes .= count
      mNodes .= (nodes V.// nodeUpdates)
      mLeafs %= (V.// leafUpdates)

    --setParent modelIdx nodes (MNodeChildReference 0) Nothing

  where getDNodes :: Int -> Get (V.Vector DNodeT)
        getDNodes count = V.replicateM count getDNodeT

        toMNodeT :: ModelT -> DNodeT -> MNodeT
        toMNodeT model dNode =
          MNodeT { _mnContents     = (-1)
                 , _mnVisFrame     = 0
                 , _mnMins         = fmap fromIntegral (dNode^.dnMins)
                 , _mnMaxs         = fmap fromIntegral (dNode^.dnMaxs)
                 , _mnParent       = Nothing
                 , _mnPlane        = (model^.mPlanes) V.! (dNode^.dnPlaneNum)
                 , _mnChildren     = getChildren (dNode^.dnChildren)
                 , _mnFirstSurface = fromIntegral (dNode^.dnFirstFace)
                 , _mnNumSurfaces  = fromIntegral (dNode^.dnNumFaces)
                 }

        getChildren :: (Int, Int) -> (MNodeChild, MNodeChild)
        getChildren (p1, p2) =
          let a = if p1 >= 0
                    then MNodeChildReference p1
                    else MLeafChildReference ((-1) - p1)
              b = if p2 >= 0
                    then MNodeChildReference p2
                    else MLeafChildReference ((-1) - p2)
          in (a, b)

setParent :: V.Vector MNodeT -> V.Vector MLeafT -> MNodeChild -> Maybe MNodeReference -> ([(Int, MNodeT)], [(Int, MLeafT)])
setParent nodes leafs childRef parentRef = collectUpdates childRef parentRef [] []
  where collectUpdates :: MNodeChild -> Maybe MNodeReference -> [(Int, MNodeT)] -> [(Int, MLeafT)] -> ([(Int, MNodeT)], [(Int, MLeafT)])
        collectUpdates c p nodeAcc leafAcc =
          case c of
            MNodeChildReference idx ->
              let nodeAcc' = (idx, (nodes V.! idx) { _mnParent = p }) : nodeAcc
                  (a, b) = (nodes V.! idx)^.mnChildren
                  (nodeAcc'', leafAcc') = collectUpdates a (Just $ MNodeReference idx) nodeAcc' leafAcc
              in collectUpdates b (Just $ MNodeReference idx) nodeAcc'' leafAcc'

            MLeafChildReference idx ->
              (nodeAcc, (idx, (leafs V.! idx) { _mlParent = p }) : leafAcc)
        

{-
setParent :: Int -> V.Vector MNodeT -> MNodeChild -> Maybe MNodeReference -> Quake ()
setParent modelIdx nodes childRef parentRef =
    case childRef of
      MNodeChildReference idx -> do
        fastRenderAPIGlobals.frModKnown.ix modelIdx.mNodes.ix idx.mnParent .= parentRef
        let (a, b) = (nodes V.! idx)^.mnChildren
        setParent modelIdx nodes a (Just $ MNodeReference idx)
        setParent modelIdx nodes b (Just $ MNodeReference idx)

      MLeafChildReference idx ->
        fastRenderAPIGlobals.frModKnown.ix modelIdx.mLeafs.ix idx.mlParent .= parentRef
        -}

loadSubmodels :: B.ByteString -> LumpT -> Quake ()
loadSubmodels buffer lump = do
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel

    when ((lump^.lFileLen) `mod` dModelTSize /= 0) $ do
      Just name <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx.mName
      Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` name)

    let count = (lump^.lFileLen) `div` dModelTSize
        buf = BL.fromStrict $ B.take (lump^.lFileLen) (B.drop (lump^.lFileOfs) buffer)
        dModels = runGet (getDModels count) buf
        models = V.map toMModelT dModels

    zoom (fastRenderAPIGlobals.frModKnown.ix modelIdx) $ do
      mNumSubModels .= count
      mSubModels .= models

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
