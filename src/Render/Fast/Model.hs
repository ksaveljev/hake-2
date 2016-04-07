module Render.Fast.Model
  ( freeAll
  , modelListF
  , modInit
  , rBeginRegistration
  , rRegisterModel
  , rEndRegistration
  ) where

import {-# SOURCE #-} qualified Client.VID as VID
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import           QCommon.LumpT
import           QCommon.QFiles.BSP.DHeaderT
import           QCommon.QFiles.BSP.DPlaneT
import           QCommon.QFiles.SP2.DSpriteT
import           QCommon.QFiles.MD2.DMdlT
import           QCommon.TexInfoT
import           QuakeIOState
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Polygon as Polygon
import           Render.MEdgeT
import           Render.ModelT
import           Render.MTexInfoT
import           Render.MVertexT
import           Types
import           Util.Binary (encode, getInt)
import qualified Util.Lib as Lib

import           Control.Monad.ST (ST, runST)

import           Control.Lens (use, preuse, ix, (^.), (.=), (+=), (%=), (&), (.~))
import           Control.Monad (when, unless)
import           Data.Binary.Get (runGet)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..))
import           System.IO (Handle)

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
loadFaces = error "Model.loadFaces" -- TODO

loadMarkSurfaces :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadMarkSurfaces = error "Model.loadMarkSurfaces" -- TODO

loadVisibility :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadVisibility = error "Model.loadVisibility" -- TODO

loadLeafs :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadLeafs = error "Model.loadLeafs" -- TODO

loadNodes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadNodes = error "Model.loadNodes" -- TODO

loadSubmodels :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadSubmodels = error "Model.loadSubmodels" -- TODO

setupSubmodels :: ModelT -> Int -> Int -> Quake ()
setupSubmodels = error "Model.setupSubmodels" -- TODO
