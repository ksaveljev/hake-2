module Render.Fast.Model
  ( freeAll
  , modelListF
  , modInit
  , rBeginRegistration
  , rRegisterModel
  , rEndRegistration
  ) where

import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import           QCommon.QFiles.BSP.DHeaderT
import           QCommon.QFiles.SP2.DSpriteT
import           QCommon.QFiles.MD2.DMdlT
import           QuakeIOState
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Polygon as Polygon
import           Render.ModelT
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib

import           Control.Lens (use, preuse, ix, (^.), (.=), (+=), (%=), (&), (.~))
import           Control.Monad (when, unless)
import           Data.Binary.Get (runGet)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
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
loadVertexes = error "Model.loadVertexes" -- TODO

loadEdges :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadEdges = error "Model.loadEdges" -- TODO

loadSurfEdges :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadSurfEdges = error "Model.loadSurfEdges" -- TODO

loadLighting :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadLighting = error "Model.loadLighting" -- TODO

loadPlanes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadPlanes = error "Model.loadPlanes" -- TODO

loadTexInfo :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadTexInfo = error "Model.loadTexInfo" -- TODO

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
