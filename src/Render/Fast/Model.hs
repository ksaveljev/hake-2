module Render.Fast.Model
    ( freeAll
    , modelListF
    , modInit
    , rBeginRegistration
    , rRegisterModel
    , rEndRegistration
    ) where

import           Control.Lens (use, (^.), (.=), (+=), (%=), (&), (.~), (%~), _1, _2)
import           Control.Monad (when)
import           Data.Binary.Get (runGet, getWord16le)
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..), V4(..))
import           System.IO (Handle)

import {-# SOURCE #-} qualified Client.VID as VID
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import           QCommon.LumpT
import           QCommon.QFiles.BSP.DFaceT
import           QCommon.QFiles.BSP.DHeaderT
import           QCommon.QFiles.BSP.DPlaneT
import           QCommon.QFiles.BSP.DVisT
import           QCommon.QFiles.SP2.DSpriteT
import           QCommon.QFiles.MD2.DMdlT
import           QCommon.TexInfoT
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Polygon as Polygon
import qualified Render.Fast.Surf as Surf
import qualified Render.Fast.Warp as Warp
import           Render.MEdgeT
import           Render.ModelT
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Render.MVertexT
import           Types
import           Util.Binary (encode, getInt)
import qualified Util.Lib as Lib

modInit :: Quake ()
modInit =
  do fastRenderAPIGlobals.frModKnown %= fmap (const newModelT)
     fastRenderAPIGlobals.frModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF

rBeginRegistration :: B.ByteString -> Quake ()
rBeginRegistration modelName =
  do resetModelArrays
     Polygon.reset
     fastRenderAPIGlobals.frRegistrationSequence += 1
     fastRenderAPIGlobals.frOldViewCluster .= (-1) -- force markleafs
     flushMap <- CVar.get "flushmap" "0" 0
     doBeginRegistration flushMap
  where fullName = B.concat ["maps/", modelName, ".bsp"]
        doBeginRegistration Nothing= Com.fatalError "Model.rBeginRegistration flushMap is Nothing"
        doBeginRegistration (Just flushMap)=
          do model <- readRef (Ref 0)
             when ((model^.mName) /= fullName || (flushMap^.cvValue) /= 0) $
               modFree (Ref 0)
             modelRef <- modForName fullName True
             fastRenderAPIGlobals.frWorldModel .= modelRef
             fastRenderAPIGlobals.frViewCluster .= (-1)

rRegisterModel :: B.ByteString -> Quake (Maybe (Ref ModelT))
rRegisterModel = error "Model.rRegisterModel" -- TODO

rEndRegistration :: Quake ()
rEndRegistration = error "Model.rEndRegistration" -- TODO

freeAll :: Quake ()
freeAll = fastRenderAPIGlobals.frModKnown %= fmap freeModel

freeModel :: ModelT -> ModelT
freeModel model =
  case model^.mExtraData of
    Nothing -> model
    Just _ -> newModelT

modelListF :: XCommandT
modelListF = error "Model.modelListF" -- TODO

modFree :: Ref ModelT -> Quake ()
modFree modelRef = writeRef modelRef newModelT

modForName :: B.ByteString -> Bool -> Quake (Maybe (Ref ModelT))
modForName name crash
  | B.null name =
      do Com.comError Constants.errDrop "Mod_ForName: NULL name"
         return Nothing
  | BC.head name == '*' = getInlineModel name
  | otherwise =
      do modKnown <- use (fastRenderAPIGlobals.frModKnown)
         maybe (loadModel modKnown name crash) (return . Just . Ref) (findExisting modKnown)
  where findExisting = V.findIndex (\m -> (m^.mName) == name)

getInlineModel :: B.ByteString -> Quake (Maybe (Ref ModelT))
getInlineModel name =
  do worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
     err <- checkForError worldModelRef
     when err $ Com.comError Constants.errDrop "bad inline model number"
     return (Just (Ref (idx + Constants.maxModKnown)))
  where idx = Lib.atoi (B.drop 1 name)
        checkForError Nothing = return True
        checkForError (Just worldModelRef)
          | idx < 1 = return True
          | otherwise =
              do worldModel <- readRef worldModelRef
                 return (idx >= (worldModel^.mNumSubModels))

loadModel :: V.Vector ModelT -> B.ByteString -> Bool -> Quake (Maybe (Ref ModelT))
loadModel modKnown name crash =
  do emptySpotRef <- maybe noEmptySpot (return . Ref) (findFree modKnown)
     modifyRef emptySpotRef (\v -> v & mName .~ name)
     fileHandle <- FS.fOpenFileWithLength name
     maybe (modelNotFound emptySpotRef) (doLoadModel name emptySpotRef) fileHandle
  where findFree = V.findIndex (\m -> B.null (m^.mName))
        noEmptySpot =
          do modNumKnown <- use (fastRenderAPIGlobals.frModNumKnown)
             when (modNumKnown == Constants.maxModKnown) $
               Com.comError Constants.errDrop "mod_numknown == MAX_MOD_KNOWN"
             fastRenderAPIGlobals.frModNumKnown += 1
             return (Ref modNumKnown)
        modelNotFound emptySpotRef =
          do when crash $
               Com.comError Constants.errDrop (B.concat ["Mod_NumForName: ", name, " not found\n"])
             modifyRef emptySpotRef (\v -> v & mName .~ B.empty)
             return Nothing

doLoadModel :: B.ByteString -> Ref ModelT -> (Handle, Int) -> Quake (Maybe (Ref ModelT))
doLoadModel name emptySpotRef (fileHandle, len) =
  do buf <- request (io (BL.hGet fileHandle len))
     fastRenderAPIGlobals.frLoadModel .= emptySpotRef
     loadModelType buf (BL.toStrict (BL.take 4 buf))
     return (Just emptySpotRef)
  where loadModelType buf header
          | header == idAliasHeader = loadAliasModel emptySpotRef buf
          | header == idSpriteHeader = loadSpriteModel emptySpotRef buf
          | header == idBSPHeader = loadBrushModel emptySpotRef buf
          | otherwise = Com.comError Constants.errDrop ("Mod_NumForName: unknown fileid for " `B.append` name)

loadAliasModel :: Ref ModelT -> BL.ByteString -> Quake ()
loadAliasModel = error "Model.loadAliasModel" -- TODO

loadSpriteModel :: Ref ModelT -> BL.ByteString -> Quake ()
loadSpriteModel = error "Model.loadSpriteModel" -- TODO

loadBrushModel :: Ref ModelT -> BL.ByteString -> Quake ()
loadBrushModel modelRef buf =
  do loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
     modifyRef loadModelRef (\v -> v & mType .~ Constants.modBrush)
     checkModelRef loadModelRef
     checkHeader
     loadVertexes     loadModelRef buf (lumps V.! Constants.lumpVertexes)
     loadEdges        loadModelRef buf (lumps V.! Constants.lumpEdges)
     loadSurfEdges    loadModelRef buf (lumps V.! Constants.lumpSurfEdges)
     loadLighting     loadModelRef buf (lumps V.! Constants.lumpLighting)
     loadPlanes       loadModelRef buf (lumps V.! Constants.lumpPlanes)
     loadTexInfo      loadModelRef buf (lumps V.! Constants.lumpTexInfo)
     loadFaces        loadModelRef buf (lumps V.! Constants.lumpFaces)
     loadMarkSurfaces loadModelRef buf (lumps V.! Constants.lumpLeafFaces)
     loadVisibility   loadModelRef buf (lumps V.! Constants.lumpVisibility)
     loadLeafs        loadModelRef buf (lumps V.! Constants.lumpLeafs)
     loadNodes        loadModelRef buf (lumps V.! Constants.lumpNodes)
     loadSubmodels    loadModelRef buf (lumps V.! Constants.lumpModels)
     modifyRef modelRef (\v -> v & mNumFrames .~ 2) -- regular and alternate animation
     model <- readRef modelRef
     setupSubmodels model 0 (model^.mNumSubModels)
  where header = runGet getDHeaderT buf
        lumps = header^.dhLumps
        checkModelRef loadModelRef =
          when (loadModelRef /= (Ref 0)) $
            Com.comError Constants.errDrop "Loaded a brush model after the world"
        checkHeader =
          when ((header^.dhVersion) /= Constants.bspVersion) $
            do model <- readRef modelRef
               Com.comError Constants.errDrop (B.concat ["Mod_LoadBrushModel: ", model^.mName, " has wrong version number (", encode (header^.dhVersion), " should be ", encode Constants.bspVersion, ")"])

resetModelArrays :: Quake ()
resetModelArrays =
  do fastRenderAPIGlobals.frModelTextureCoordIdx .= 0
     fastRenderAPIGlobals.frModelVertexIndexIdx .= 0

loadVertexes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadVertexes loadModelRef buf lump =
  do checkLump
     modifyRef loadModelRef (\v -> v & mNumVertexes .~ count
                                     & mVertexes .~ readVertexes)
  where checkLump =
          when ((lump^.lFileLen) `mod` mVertexDiskSize /= 0) $
            do model <- readRef loadModelRef
               Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
        count = (lump^.lFileLen) `div` mVertexDiskSize
        readVertexes = runGet (V.replicateM count getMVertexT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

loadEdges :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadEdges loadModelRef buf lump =
  do checkLump
     modifyRef loadModelRef (\v -> v & mNumEdges .~ count
                                     & mEdges .~ readEdges)
  where checkLump =
          when ((lump^.lFileLen) `mod` mEdgeDiskSize /= 0) $
            do model <- readRef loadModelRef
               Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
        count = (lump^.lFileLen) `div` mEdgeDiskSize
        readEdges = runGet (V.replicateM count getMEdgeT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

loadSurfEdges :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadSurfEdges loadModelRef buf lump =
  do checkLump
     modifyRef loadModelRef (\v -> v & mNumSurfEdges .~ count
                                     & mSurfEdges .~ readOffsets)
  where count = (lump^.lFileLen) `div` Constants.sizeOfInt
        checkLump =
          do when ((lump^.lFileLen) `mod` Constants.sizeOfInt /= 0) $
               do model <- readRef loadModelRef
                  Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
             when (count < 1 || count >= Constants.maxMapSurfEdges) $
               do model <- readRef loadModelRef
                  Com.comError Constants.errDrop (B.concat ["MOD_LoadBmodel bad surfedges count in ", model^.mName, ": ", encode count])
        readOffsets = runGet (UV.replicateM count getInt) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

loadLighting :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadLighting loadModelRef buf lump =
  modifyRef loadModelRef (\v -> v & mLightdata .~ lightData)
  where lightData | (lump^.lFileLen) == 0 = Nothing
                  | otherwise = Just (BL.toStrict (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)))

loadPlanes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadPlanes loadModelRef buf lump =
  do checkLump
     modifyRef loadModelRef (\v -> v & mNumPlanes .~ count
                                     & mPlanes .~ V.map toCPlane getPlanes)
  where count = (lump^.lFileLen) `div` dPlaneTSize
        checkLump =
          when ((lump^.lFileLen) `mod` dPlaneTSize /= 0) $
            do model <- readRef loadModelRef
               Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
        getPlanes = runGet (V.replicateM count getDPlaneT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

-- TODO: same as in QCommon/CM
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

-- TODO: optimize allocation rate / memory consumption using mutable vectors
loadTexInfo :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadTexInfo loadModelRef buf lump =
  do checkLump
     texInfo <- V.mapM toMTexInfo readTexInfo
     modifyRef loadModelRef (\v -> v & mNumTexInfo .~ count
                                     & mTexInfo .~ V.imap (countAnimationFrames texInfo) texInfo)
  where checkLump =
          when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $
            do model <- readRef loadModelRef
               Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
        count = (lump^.lFileLen) `div` texInfoTSize
        readTexInfo = runGet (V.replicateM count getTexInfoT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

toMTexInfo :: TexInfoT -> Quake MTexInfoT
toMTexInfo texInfo =
  do foundImage <- Image.glFindImage name Constants.itWall
     imageRef <- maybe returnNoTexture return foundImage
     return MTexInfoT { _mtiVecs = texInfo^.tiVecs
                      , _mtiFlags = texInfo^.tiFlags
                      , _mtiNumFrames = 1
                      , _mtiNext = if (texInfo^.tiNextTexInfo) > 0 then Just (Ref (texInfo^.tiNextTexInfo)) else Nothing
                      , _mtiImage = Just imageRef
                      }
  where name = B.concat ["textures/", texInfo^.tiTexture, ".wal"]
        returnNoTexture =
          do VID.printf Constants.printAll (B.concat ["Couldn't load ", name, "\n"])
             use (fastRenderAPIGlobals.frNoTexture)

countAnimationFrames :: V.Vector MTexInfoT -> Int -> MTexInfoT -> MTexInfoT
countAnimationFrames texInfo idx currentTexInfo =
  currentTexInfo & mtiNumFrames .~ countFrames (Ref idx) (currentTexInfo^.mtiNext) 1
  where countFrames _ Nothing count = count
        countFrames initialRef (Just currentRef@(Ref currentIdx)) count
          | currentRef == initialRef = count
          | otherwise = countFrames initialRef ((texInfo V.! currentIdx)^.mtiNext) (count + 1)

loadFaces :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadFaces loadModelRef buf lump =
  do checkLump
     modifyRef loadModelRef (\v -> v & mNumSurfaces .~ count
                                     & mSurfaces .~ V.replicate count newMSurfaceT)
     fastRenderAPIGlobals.frCurrentModel .= Just loadModelRef
     Surf.glBeginBuildingLightmaps loadModelRef
     V.imapM_ (constructMSurfaceT loadModelRef) dFaces
     Surf.glEndBuildingLightmaps
  where checkLump =
          when ((lump^.lFileLen) `mod` dFaceTSize /= 0) $
            do model <- readRef loadModelRef
               Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
        count = (lump^.lFileLen) `div` dFaceTSize
        dFaces = runGet (V.replicateM count getDFaceT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

constructMSurfaceT :: Ref ModelT -> Int -> DFaceT -> Quake ()
constructMSurfaceT loadModelRef idx dFace =
  do model <- readRef loadModelRef
     when (ti < 0 || ti >= (model^.mNumTexInfo)) $
       Com.comError Constants.errDrop "MOD_LoadBmodel: bad texinfo number"
     modifyRef surfaceRef (\v -> v & msFirstEdge .~ dFace^.dfFirstEdge
                                   & msNumEdges .~ fromIntegral (dFace^.dfNumEdges)
                                   & msFlags .~ (if (dFace^.dfSide) /= 0 then Constants.surfPlaneBack else 0)
                                   & msPolys .~ Nothing
                                   & msPlane .~ Just (Ref (fromIntegral (dFace^.dfPlaneNum)))
                                   & msTexInfo .~ (Ref ti)
                                   & msStyles .~ dFace^.dfStyles -- TODO: should we limit it by Constants.maxLightMaps ?
                                   & msSamples .~ if (dFace^.dfLightOfs) == -1 then Nothing else Just (dFace^.dfLightOfs))
     calcSurfaceExtents loadModelRef surfaceRef
     createWarps surfaceRef
     createLightmaps surfaceRef (model^.mLightdata)
     createPolygons surfaceRef
  where surfaceRef = Ref idx
        ti = fromIntegral (dFace^.dfTexInfo)

calcSurfaceExtents :: Ref ModelT -> Ref MSurfaceT -> Quake ()
calcSurfaceExtents loadModelRef surfaceRef =
  do model <- readRef loadModelRef
     surface <- readRef surfaceRef
     texInfo <- readRef (surface^.msTexInfo)
     let (mins, maxs) = calcMinsMaxs model texInfo surface 0 (999999, 999999) (-99999, -99999)
         bmins = mapTuple (floor . (/ 16)) mins
         bmaxs = mapTuple (ceiling . (/ 16)) maxs
     modifyRef surfaceRef (\v -> v & msTextureMins .~ mapTuple (* 16) bmins
                                   & msExtents .~ mapTuple (* 16) (fst bmaxs - fst bmins, snd bmaxs - snd bmins))
  where mapTuple f (a1, a2) = (f a1, f a2)

calcMinsMaxs :: ModelT -> MTexInfoT -> MSurfaceT -> Int -> (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float))
calcMinsMaxs model texInfo surface idx mins maxs
  | idx >= (surface^.msNumEdges) = (mins, maxs)
  | otherwise =
      let e = (model^.mSurfEdges) UV.! ((surface^.msFirstEdge) + idx)
          v = getVertex e
          V3 a b c = v^.mvPosition
          (V4 a1 b1 c1 d1, V4 a2 b2 c2 d2) = texInfo^.mtiVecs
          val1 = a * a1 + b * b1 + c * c1 + d1
          val2 = a * a2 + b * b2 + c * c2 + d2
          mins' = (min val1 (fst mins), min val2 (snd mins))
          maxs' = (max val1 (fst maxs), max val2 (snd maxs))
      in calcMinsMaxs model texInfo surface (idx + 1) mins' maxs'
  where getVertex e
          | e >= 0 =
              let edge = (model^.mEdges) V.! e
              in (model^.mVertexes) V.! (fromIntegral (edge^.meV._1))
          | otherwise =
              let edge = (model^.mEdges) V.! (negate e)
              in (model^.mVertexes) V.! (fromIntegral (edge^.meV._2))

createWarps :: Ref MSurfaceT -> Quake ()
createWarps surfaceRef =
  do surface <- readRef surfaceRef
     texInfo <- readRef (surface^.msTexInfo)
     when ((texInfo^.mtiFlags) .&. Constants.surfWarp /= 0) $
       do modifyRef surfaceRef (\v -> v & msFlags %~ (.|. Constants.surfDrawTurb)
                                        & msExtents .~ (16384, 16384)
                                        & msTextureMins .~ (-8192, -8192))
          Warp.glSubdivideSurface surfaceRef

createLightmaps :: Ref MSurfaceT -> Maybe B.ByteString -> Quake ()
createLightmaps surfaceRef lightData =
  do surface <- readRef surfaceRef
     texInfo <- readRef (surface^.msTexInfo)
     when ((texInfo^.mtiFlags) .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) == 0) $
       Surf.glCreateSurfaceLightmap surfaceRef lightData

createPolygons :: Ref MSurfaceT -> Quake ()
createPolygons surfaceRef =
  do surface <- readRef surfaceRef
     texInfo <- readRef (surface^.msTexInfo)
     when ((texInfo^.mtiFlags) .&. Constants.surfWarp == 0) $
       Surf.glBuildPolygonFromSurface surfaceRef

loadMarkSurfaces :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadMarkSurfaces loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumMarkSurfaces .~ count
                                    & mMarkSurfaces .~ V.map toMSurfaceRef getMarkSurfaces)
  where
    count = (lump^.lFileLen) `div` Constants.sizeOfShort
    checkLump =
        when ((lump^.lFileLen) `mod` Constants.sizeOfShort /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    getMarkSurfaces = runGet (V.replicateM count getWord16le) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))
    toMSurfaceRef idx = Ref (fromIntegral idx)

loadVisibility :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadVisibility loadModelRef buf lump
    | (lump^.lFileLen) == 0 =
        modifyRef loadModelRef (\v -> v & mVis .~ Nothing)
    | otherwise = do
        let buffer = BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)
            buffer' = BL.toStrict buffer
            vis = runGet getDVisT buffer
        fastRenderAPIGlobals.frModelVisibility .= Just buffer'
        modifyRef loadModelRef (\v -> v & mVis .~ Just vis)

loadLeafs :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadLeafs = error "Model.loadLeafs" -- TODO

loadNodes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadNodes = error "Model.loadNodes" -- TODO

loadSubmodels :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadSubmodels = error "Model.loadSubmodels" -- TODO

setupSubmodels :: ModelT -> Int -> Int -> Quake ()
setupSubmodels = error "Model.setupSubmodels" -- TODO
