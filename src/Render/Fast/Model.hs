module Render.Fast.Model
    ( freeAll
    , modelListF
    , modInit
    , rBeginRegistration
    , rRegisterModel
    , rEndRegistration
    ) where

import           Control.Lens                    (use, (^.), (.=), (+=), (%=))
import           Control.Lens                    ((&), (.~), (%~), _1, _2)
import           Control.Monad                   (when)
import           Data.Binary.Get                 (runGet, getWord16le, getWord32le, getByteString)
import           Data.Binary.IEEE754             (wordToFloat)
import           Data.Bits                       ((.&.), (.|.))
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.Lazy            as BL
import           Data.Int                        (Int32)
import           Data.Maybe                      (fromJust)
import qualified Data.Vector                     as V
import qualified Data.Vector.Storable.Mutable    as MSV
import qualified Data.Vector.Unboxed             as UV
import           Data.Word                       (Word32)
import           Linear                          (V3(..), V4(..), norm, _x, _y, _z)
import           System.IO                       (Handle)

import {-# SOURCE #-} qualified Client.VID       as VID
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com                     as Com
import qualified QCommon.CVar                    as CVar
import qualified QCommon.FS                      as FS
import           QCommon.LumpT
import           QCommon.QFiles.BSP.DFaceT
import           QCommon.QFiles.BSP.DHeaderT
import           QCommon.QFiles.BSP.DLeafT
import           QCommon.QFiles.BSP.DModelT
import           QCommon.QFiles.BSP.DNodeT
import           QCommon.QFiles.BSP.DPlaneT
import           QCommon.QFiles.BSP.DVisT
import           QCommon.QFiles.SP2.DSprFrameT
import           QCommon.QFiles.SP2.DSpriteT
import           QCommon.QFiles.MD2.DAliasFrameT
import           QCommon.QFiles.MD2.DMdlT
import           QCommon.QFiles.MD2.DSTVertT
import           QCommon.QFiles.MD2.DTriangleT
import           QCommon.TexInfoT
import           QuakeIOState
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Image               as Image
import qualified Render.Fast.Polygon             as Polygon
import qualified Render.Fast.Surf                as Surf
import qualified Render.Fast.Warp                as Warp
import           Render.ImageT
import           Render.MEdgeT
import           Render.MModelT
import           Render.ModelT
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Render.MVertexT
import           Types
import           Util.Binary                     (encode, getInt)
import qualified Util.Lib                        as Lib

modInit :: Quake ()
modInit = do
    fastRenderAPIGlobals.frModKnown %= fmap (const newModelT)
    fastRenderAPIGlobals.frModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF

rBeginRegistration :: B.ByteString -> Quake ()
rBeginRegistration modelName = do
    resetModelArrays
    Polygon.reset
    fastRenderAPIGlobals.frRegistrationSequence += 1
    fastRenderAPIGlobals.frOldViewCluster .= (-1) -- force markleafs
    flushMap <- CVar.get "flushmap" "0" 0
    doBeginRegistration flushMap
  where
    fullName = B.concat ["maps/", modelName, ".bsp"]
    doBeginRegistration Nothing = Com.fatalError "Model.rBeginRegistration flushMap is Nothing"
    doBeginRegistration (Just flushMap) = do
        model <- readRef (Ref 0)
        when ((model^.mName) /= fullName || (flushMap^.cvValue) /= 0) $
            modFree (Ref 0)
        modelRef <- modForName fullName True
        fastRenderAPIGlobals.frWorldModel .= modelRef
        fastRenderAPIGlobals.frViewCluster .= (-1)

rRegisterModel :: B.ByteString -> Quake (Maybe (Ref ModelT))
rRegisterModel name = do
    modelRef <- modForName name False
    registerModelImages modelRef
    return modelRef

registerModelImages :: Maybe (Ref ModelT) -> Quake ()
registerModelImages Nothing = return ()
registerModelImages (Just modelRef) = do
    model <- readRef modelRef
    regSeq <- use (fastRenderAPIGlobals.frRegistrationSequence)
    modifyRef modelRef (\v -> v & mRegistrationSequence .~ regSeq)
    doRegisterModelImages modelRef model regSeq

doRegisterModelImages :: Ref ModelT -> ModelT -> Int -> Quake ()
doRegisterModelImages modelRef model regSeq
    | (model^.mType) == Constants.modSprite =
        updateSpriteSkins modelRef (model^.mExtraData)
    | (model^.mType) == Constants.modAlias =
        updateAliasSkins modelRef (model^.mExtraData)
    | (model^.mType) == Constants.modBrush =
        V.mapM_ (updateImageRegSeq regSeq) (model^.mTexInfo)
    | otherwise = return ()

updateSpriteSkins :: Ref ModelT -> Maybe ModelExtra -> Quake ()
updateSpriteSkins modelRef (Just (SpriteModelExtra sprOut)) = do
    skins <- V.mapM (\frame -> Image.glFindImage (frame^.dsfName) Constants.itSprite) (sprOut^.dsFrames)
    modifyRef modelRef (\v -> v & mSkins .~ skins )
updateSpriteSkins _ _ =
    Com.fatalError "Model.updateSpriteSkins modelExtra is NOT SpriteModelExtra"

updateAliasSkins :: Ref ModelT -> Maybe ModelExtra -> Quake ()
updateAliasSkins modelRef (Just (AliasModelExtra pheader)) = do
    maybe skinNamesError doUpdateAliasSkins (pheader^.dmSkinNames)
  where
    skinNamesError = Com.fatalError "Model.updateAliasSkins pheader^.dmSkinNames is Nothing"
    doUpdateAliasSkins skinNames = do
        skins <- V.mapM (\skinName -> Image.glFindImage skinName Constants.itSkin) skinNames
        modifyRef modelRef (\v -> v & mSkins .~ skins
                                    & mNumFrames .~ pheader^.dmNumFrames)
updateAliasSkins _ _ =
    Com.fatalError "Model.updateAliasSkins modelExtra is NOT AliasModelExtra"

updateImageRegSeq :: Int -> MTexInfoT -> Quake ()
updateImageRegSeq regSeq texInfo =
    maybe (return ()) setImageRegSeq (texInfo^.mtiImage)
  where
    setImageRegSeq imageRef =
        modifyRef imageRef (\v -> v & iRegistrationSequence .~ regSeq)

rEndRegistration :: Quake ()
rEndRegistration = error "Model.rEndRegistration" -- TODO

freeAll :: Quake ()
freeAll = fastRenderAPIGlobals.frModKnown %= fmap freeModel

freeModel :: ModelT -> ModelT
freeModel model = maybe model (const newModelT) (model^.mExtraData)

modelListF :: XCommandT
modelListF = error "Model.modelListF" -- TODO

modFree :: Ref ModelT -> Quake ()
modFree modelRef = writeRef modelRef newModelT

modForName :: B.ByteString -> Bool -> Quake (Maybe (Ref ModelT))
modForName name crash
    | B.null name = do
        Com.comError Constants.errDrop "Mod_ForName: NULL name"
        return Nothing
    | BC.head name == '*' = getInlineModel name
    | otherwise = do
        modKnown <- use (fastRenderAPIGlobals.frModKnown)
        maybe (loadModel modKnown name crash) (return . Just . Ref) (findExisting modKnown)
  where
    findExisting = V.findIndex (\m -> (m^.mName) == name)

getInlineModel :: B.ByteString -> Quake (Maybe (Ref ModelT))
getInlineModel name = do
    worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
    err <- checkForError worldModelRef
    when err $ Com.comError Constants.errDrop "bad inline model number"
    return (Just (Ref (idx + Constants.maxModKnown)))
  where
    idx = Lib.atoi (B.drop 1 name)
    checkForError Nothing = return True
    checkForError (Just worldModelRef)
        | idx < 1 = return True
        | otherwise = do
            worldModel <- readRef worldModelRef
            return (idx >= (worldModel^.mNumSubModels))

loadModel :: V.Vector ModelT -> B.ByteString -> Bool -> Quake (Maybe (Ref ModelT))
loadModel modKnown name crash = do
    emptySpotRef <- maybe noEmptySpot (return . Ref) (findFree modKnown)
    modifyRef emptySpotRef (\v -> v & mName .~ name)
    fileHandle <- FS.fOpenFileWithLength name
    maybe (modelNotFound emptySpotRef) (doLoadModel name emptySpotRef) fileHandle
  where
    findFree = V.findIndex (\m -> B.null (m^.mName))
    noEmptySpot = do
        modNumKnown <- use (fastRenderAPIGlobals.frModNumKnown)
        when (modNumKnown == Constants.maxModKnown) $
            Com.comError Constants.errDrop "mod_numknown == MAX_MOD_KNOWN"
        fastRenderAPIGlobals.frModNumKnown += 1
        return (Ref modNumKnown)
    modelNotFound emptySpotRef = do
        when crash $
            Com.comError Constants.errDrop (B.concat ["Mod_NumForName: ", name, " not found\n"])
        modifyRef emptySpotRef (\v -> v & mName .~ B.empty)
        return Nothing

doLoadModel :: B.ByteString -> Ref ModelT -> (Handle, Int) -> Quake (Maybe (Ref ModelT))
doLoadModel name emptySpotRef (fileHandle, len) = do
    buf <- request (io (BL.hGet fileHandle len))
    fastRenderAPIGlobals.frLoadModel .= emptySpotRef
    loadModelType buf (BL.toStrict (BL.take 4 buf))
    return (Just emptySpotRef)
  where
    loadModelType buf header
        | header == idAliasHeader = loadAliasModel emptySpotRef buf
        | header == idSpriteHeader = loadSpriteModel emptySpotRef buf
        | header == idBSPHeader = loadBrushModel emptySpotRef buf
        | otherwise = Com.comError Constants.errDrop ("Mod_NumForName: unknown fileid for " `B.append` name)

loadAliasModel :: Ref ModelT -> BL.ByteString -> Quake ()
loadAliasModel modelRef buffer = do
    model <- readRef modelRef
    checkForErrors (model^.mName)
    skins <- V.mapM (\name -> Image.glFindImage name Constants.itSkin) skinNames
    pheader'' <- precompileGLCmds pheader'
    modifyRef modelRef (\v -> v & mType .~ Constants.modAlias
                                & mExtraData .~ Just (AliasModelExtra pheader'')
                                & mMins .~ V3 (-32) (-32) (-32)
                                & mMaxs .~ V3 32 32 32
                                & mSkins .~ skins)
  where
    pheader = runGet getDMdlT buffer
    pOutST = runGet (V.replicateM (pheader^.dmNumST) getDSTVertT) (BL.drop (fromIntegral (pheader^.dmOfsST)) buffer)
    pOutTri = runGet (V.replicateM (pheader^.dmNumTris) getDTriangleT) (BL.drop (fromIntegral (pheader^.dmOfsTris)) buffer)
    pOutFrame = runGet (V.replicateM (pheader^.dmNumFrames) (getDAliasFrameT (pheader^.dmNumXYZ))) (BL.drop (fromIntegral (pheader^.dmOfsFrames)) buffer)
    pOutCmd = runGet (UV.replicateM (pheader^.dmNumGlCmds) getWord32le) (BL.drop (fromIntegral (pheader^.dmOfsGlCmds)) buffer)
    skinNames = V.map (B.takeWhile (/= 0)) $ runGet (V.replicateM (pheader^.dmNumSkins) (getByteString Constants.maxSkinName)) (BL.drop (fromIntegral (pheader^.dmOfsSkins)) buffer)
    pheader' = pheader & dmSkinNames   .~ Just skinNames
                       & dmSTVerts     .~ Just pOutST
                       & dmTriAngles   .~ Just pOutTri
                       & dmGlCmds      .~ Just pOutCmd
                       & dmAliasFrames .~ Just pOutFrame
    checkForErrors modelName = do
        when ((pheader^.dmVersion) /= Constants.aliasVersion) $
            Com.comError Constants.errDrop (B.concat [modelName, " has wrong version number (", encode (pheader^.dmVersion), " should be ", encode Constants.aliasVersion, ")"])
        when ((pheader^.dmSkinHeight) > Constants.maxLBMHeight) $
            Com.comError Constants.errDrop (B.concat ["model ", modelName, " has a skin taller than ", encode Constants.maxLBMHeight])
        when ((pheader^.dmNumXYZ) <= 0) $
            Com.comError Constants.errDrop (B.concat ["model ", modelName, " has no vertices"])
        when ((pheader^.dmNumXYZ) > Constants.maxVerts) $
            Com.comError Constants.errDrop (B.concat ["model ", modelName, " has too many vertices"])
        when ((pheader^.dmNumST) <= 0) $
            Com.comError Constants.errDrop (B.concat ["model ", modelName, " has no st vertices"])
        when ((pheader^.dmNumTris) <= 0) $
            Com.comError Constants.errDrop (B.concat ["model ", modelName, " has no triangles"])
        when ((pheader^.dmNumFrames) <= 0) $
            Com.comError Constants.errDrop (B.concat ["model ", modelName, " has no frames"])

loadSpriteModel :: Ref ModelT -> BL.ByteString -> Quake ()
loadSpriteModel = error "Model.loadSpriteModel" -- TODO

loadBrushModel :: Ref ModelT -> BL.ByteString -> Quake ()
loadBrushModel modelRef buf = do
    loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
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
    mapM_ (setupSubmodel loadModelRef model) [0..(model^.mNumSubModels) - 1]
  where
    header = runGet getDHeaderT buf
    lumps = header^.dhLumps
    checkModelRef loadModelRef =
        when (loadModelRef /= (Ref 0)) $
            Com.comError Constants.errDrop "Loaded a brush model after the world"
    checkHeader =
      when ((header^.dhVersion) /= Constants.bspVersion) $ do
          model <- readRef modelRef
          Com.comError Constants.errDrop (B.concat ["Mod_LoadBrushModel: ", model^.mName, " has wrong version number (", encode (header^.dhVersion), " should be ", encode Constants.bspVersion, ")"])

resetModelArrays :: Quake ()
resetModelArrays = do
    fastRenderAPIGlobals.frModelTextureCoordIdx .= 0
    fastRenderAPIGlobals.frModelVertexIndexIdx .= 0

loadVertexes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadVertexes loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumVertexes .~ count
                                    & mVertexes .~ readVertexes)
  where
    checkLump =
        when ((lump^.lFileLen) `mod` mVertexDiskSize /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    count = (lump^.lFileLen) `div` mVertexDiskSize
    readVertexes = runGet (V.replicateM count getMVertexT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

loadEdges :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadEdges loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumEdges .~ count
                                    & mEdges .~ readEdges)
  where
    checkLump =
        when ((lump^.lFileLen) `mod` mEdgeDiskSize /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    count = (lump^.lFileLen) `div` mEdgeDiskSize
    readEdges = runGet (V.replicateM count getMEdgeT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

loadSurfEdges :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadSurfEdges loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumSurfEdges .~ count
                                    & mSurfEdges .~ readOffsets)
  where
    count = (lump^.lFileLen) `div` Constants.sizeOfInt
    checkLump = do
        when ((lump^.lFileLen) `mod` Constants.sizeOfInt /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
        when (count < 1 || count >= Constants.maxMapSurfEdges) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop (B.concat ["MOD_LoadBmodel bad surfedges count in ", model^.mName, ": ", encode count])
    readOffsets = runGet (UV.replicateM count getInt) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

loadLighting :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadLighting loadModelRef buf lump =
    modifyRef loadModelRef (\v -> v & mLightdata .~ lightData)
  where
    lightData | (lump^.lFileLen) == 0 = Nothing
              | otherwise = Just (BL.toStrict (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)))

loadPlanes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadPlanes loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumPlanes .~ count
                                    & mPlanes .~ V.map toCPlane getPlanes)
  where
    count = (lump^.lFileLen) `div` dPlaneTSize
    checkLump =
        when ((lump^.lFileLen) `mod` dPlaneTSize /= 0) $ do
            model <- readRef loadModelRef
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
  where
    getBits (V3 a b c) =
        let a' = if a < 0 then 1 else 0
            b' = if b < 0 then 2 else 0
            c' = if c < 0 then 4 else 0
        in a' .|. b' .|. c'

-- TODO: optimize allocation rate / memory consumption using mutable vectors
loadTexInfo :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadTexInfo loadModelRef buf lump = do
    checkLump
    texInfo <- V.mapM toMTexInfo readTexInfo
    modifyRef loadModelRef (\v -> v & mNumTexInfo .~ count
                                    & mTexInfo .~ V.imap (countAnimationFrames texInfo) texInfo)
  where
    checkLump =
        when ((lump^.lFileLen) `mod` texInfoTSize /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    count = (lump^.lFileLen) `div` texInfoTSize
    readTexInfo = runGet (V.replicateM count getTexInfoT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

toMTexInfo :: TexInfoT -> Quake MTexInfoT
toMTexInfo texInfo = do
    foundImage <- Image.glFindImage name Constants.itWall
    imageRef <- maybe returnNoTexture return foundImage
    return MTexInfoT { _mtiVecs = texInfo^.tiVecs
                     , _mtiFlags = texInfo^.tiFlags
                     , _mtiNumFrames = 1
                     , _mtiNext = if (texInfo^.tiNextTexInfo) > 0 then Just (Ref (texInfo^.tiNextTexInfo)) else Nothing
                     , _mtiImage = Just imageRef
                     }
  where
    name = B.concat ["textures/", texInfo^.tiTexture, ".wal"]
    returnNoTexture = do
        VID.printf Constants.printAll (B.concat ["Couldn't load ", name, "\n"])
        use (fastRenderAPIGlobals.frNoTexture)

countAnimationFrames :: V.Vector MTexInfoT -> Int -> MTexInfoT -> MTexInfoT
countAnimationFrames texInfo idx currentTexInfo =
    currentTexInfo & mtiNumFrames .~ countFrames (Ref idx) (currentTexInfo^.mtiNext) 1
  where
    countFrames _ Nothing count = count
    countFrames initialRef (Just currentRef@(Ref currentIdx)) count
        | currentRef == initialRef = count
        | otherwise = countFrames initialRef ((texInfo V.! currentIdx)^.mtiNext) (count + 1)

loadFaces :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadFaces loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumSurfaces .~ count
                                    & mSurfaces .~ V.replicate count newMSurfaceT)
    fastRenderAPIGlobals.frCurrentModel .= Just loadModelRef
    Surf.glBeginBuildingLightmaps loadModelRef
    V.imapM_ (constructMSurfaceT loadModelRef) dFaces
    Surf.glEndBuildingLightmaps
  where
    checkLump =
        when ((lump^.lFileLen) `mod` dFaceTSize /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    count = (lump^.lFileLen) `div` dFaceTSize
    dFaces = runGet (V.replicateM count getDFaceT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

constructMSurfaceT :: Ref ModelT -> Int -> DFaceT -> Quake ()
constructMSurfaceT loadModelRef idx dFace = do
    model <- readRef loadModelRef
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
  where
    surfaceRef = Ref idx
    ti = fromIntegral (dFace^.dfTexInfo)

calcSurfaceExtents :: Ref ModelT -> Ref MSurfaceT -> Quake ()
calcSurfaceExtents loadModelRef surfaceRef = do
    model <- readRef loadModelRef
    surface <- readRef surfaceRef
    texInfo <- readRef (surface^.msTexInfo)
    let (mins, maxs) = calcMinsMaxs model texInfo surface 0 (999999, 999999) (-99999, -99999)
        bmins = mapTuple (floor . (/ 16)) mins
        bmaxs = mapTuple (ceiling . (/ 16)) maxs
    modifyRef surfaceRef (\v -> v & msTextureMins .~ mapTuple (* 16) bmins
                                  & msExtents .~ mapTuple (* 16) (fst bmaxs - fst bmins, snd bmaxs - snd bmins))
  where
    mapTuple f (a1, a2) = (f a1, f a2)

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
  where
    getVertex e
        | e >= 0 =
            let edge = (model^.mEdges) V.! e
            in (model^.mVertexes) V.! (fromIntegral (edge^.meV._1))
        | otherwise =
            let edge = (model^.mEdges) V.! (negate e)
            in (model^.mVertexes) V.! (fromIntegral (edge^.meV._2))

createWarps :: Ref MSurfaceT -> Quake ()
createWarps surfaceRef = do
    surface <- readRef surfaceRef
    texInfo <- readRef (surface^.msTexInfo)
    when ((texInfo^.mtiFlags) .&. Constants.surfWarp /= 0) $ do
        modifyRef surfaceRef (\v -> v & msFlags %~ (.|. Constants.surfDrawTurb)
                                      & msExtents .~ (16384, 16384)
                                      & msTextureMins .~ (-8192, -8192))
        Warp.glSubdivideSurface surfaceRef

createLightmaps :: Ref MSurfaceT -> Maybe B.ByteString -> Quake ()
createLightmaps surfaceRef lightData = do
    surface <- readRef surfaceRef
    texInfo <- readRef (surface^.msTexInfo)
    when ((texInfo^.mtiFlags) .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) == 0) $
        Surf.glCreateSurfaceLightmap surfaceRef lightData

createPolygons :: Ref MSurfaceT -> Quake ()
createPolygons surfaceRef = do
    surface <- readRef surfaceRef
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
loadLeafs loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumLeafs .~ count
                                    & mLeafs .~ V.map toMLeaf getLeafs)
  where
    count = (lump^.lFileLen) `div` dLeafTSize
    checkLump =
        when ((lump^.lFileLen) `mod` dLeafTSize /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    getLeafs = runGet (V.replicateM count getDLeafT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf)) 

toMLeaf :: DLeafT -> MLeafT
toMLeaf dLeaf = MLeafT { _mlContents        = dLeaf^.dlContents
                       , _mlVisFrame        = 0
                       , _mlMins            = fmap fromIntegral (dLeaf^.dlMins)
                       , _mlMaxs            = fmap fromIntegral (dLeaf^.dlMaxs)
                       , _mlParent          = Nothing
                       , _mlCluster         = fromIntegral (dLeaf^.dlCluster)
                       , _mlArea            = fromIntegral (dLeaf^.dlArea)
                       , _mlNumMarkSurfaces = fromIntegral (dLeaf^.dlNumLeafFaces)
                       , _mlMarkIndex       = fromIntegral (dLeaf^.dlFirstLeafFace)
                       }

loadNodes :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadNodes loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumNodes .~ count
                                    & mNodes .~ V.map toMNode getNodes)
  where
    count = (lump^.lFileLen) `div` dNodeTSize
    checkLump =
        when ((lump^.lFileLen) `mod` dNodeTSize /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    getNodes = runGet (V.replicateM count getDNodeT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

toMNode :: DNodeT -> MNodeT
toMNode dNode = MNodeT { _mnContents     = (-1) -- differentiate from leafs
                       , _mnVisFrame     = 0
                       , _mnMins         = fmap fromIntegral (dNode^.dnMins)
                       , _mnMaxs         = fmap fromIntegral (dNode^.dnMaxs)
                       , _mnParent       = Nothing
                       , _mnPlane        = Ref (dNode^.dnPlaneNum)
                       , _mnChildren     = getChildren (dNode^.dnChildren)
                       , _mnFirstSurface = fromIntegral (dNode^.dnFirstFace)
                       , _mnNumSurfaces  = fromIntegral (dNode^.dnNumFaces)
                       }

getChildren :: (Int, Int) -> (MNodeChild, MNodeChild)
getChildren (p1, p2) =
          let a = if p1 >= 0
                    then MNodeChildRef (Ref p1)
                    else MLeafChildRef (Ref ((-1) - p1))
              b = if p2 >= 0
                    then MNodeChildRef (Ref p2)
                    else MLeafChildRef (Ref ((-1) - p2))
          in (a, b)

loadSubmodels :: Ref ModelT -> BL.ByteString -> LumpT -> Quake ()
loadSubmodels loadModelRef buf lump = do
    checkLump
    modifyRef loadModelRef (\v -> v & mNumSubModels .~ count
                                    & mSubModels .~ V.map toMModel getModels)
  where
    count = (lump^.lFileLen) `div` dModelTSize
    checkLump =
        when ((lump^.lFileLen) `mod` dModelTSize /= 0) $ do
            model <- readRef loadModelRef
            Com.comError Constants.errDrop ("MOD_LoadBmodel: funny lump size in " `B.append` (model^.mName))
    getModels = runGet (V.replicateM count getDModelT) (BL.take (fromIntegral (lump^.lFileLen)) (BL.drop (fromIntegral (lump^.lFileOfs)) buf))

toMModel :: DModelT -> MModelT
toMModel dModel = MModelT { _mmMins      = mins
                          , _mmMaxs      = maxs
                          , _mmOrigin    = dModel^.dmOrigin
                          , _mmRadius    = radiusFromBounds mins maxs
                          , _mmHeadNode  = dModel^.dmHeadNode
                          , _mmVisLeafs  = 0
                          , _mmFirstFace = dModel^.dmFirstFace
                          , _mmNumFaces  = dModel^.dmNumFaces
                          }
  where
    mins = fmap (subtract 1) (dModel^.dmMins)
    maxs = fmap (+1) (dModel^.dmMaxs)

radiusFromBounds :: V3 Float -> V3 Float -> Float
radiusFromBounds mins maxs =
    let a = if abs (mins^._x) > abs (maxs^._x) then abs (mins^._x) else abs (maxs^._x)
        b = if abs (mins^._y) > abs (maxs^._y) then abs (mins^._y) else abs (maxs^._y)
        c = if abs (mins^._z) > abs (maxs^._z) then abs (mins^._z) else abs (maxs^._z)
    in norm (V3 a b c)

setupSubmodel :: Ref ModelT -> ModelT -> Int -> Quake ()
setupSubmodel loadModelRef model idx = do
    loadModel' <- readRef loadModelRef
    writeRef starModRef (loadModel' & mFirstModelSurface .~ bm^.mmFirstFace
                                    & mNumModelSurfaces .~ bm^.mmNumFaces
                                    & mFirstNode .~ bm^.mmHeadNode
                                    & mMaxs .~ bm^.mmMaxs
                                    & mMins .~ bm^.mmMins
                                    & mRadius .~ bm^.mmRadius)
    when ((bm^.mmHeadNode) >= (loadModel'^.mNumNodes)) $
        Com.comError Constants.errDrop (B.concat ["Inline model ", encode idx, " has bad firstnode"])
    when (idx == 0) $ do
        starMod <- readRef starModRef
        writeRef loadModelRef starMod
    modifyRef starModRef (\v -> v & mNumLeafs .~ bm^.mmVisLeafs)
  where
    starModRef = Ref (idx + Constants.maxModKnown)
    bm = (model^.mSubModels) V.! idx

-- TODO: old implemenation, needs some refactoring
precompileGLCmds :: DMdlT -> Quake DMdlT
precompileGLCmds model = do
    modelTextureCoordIdx <- use (fastRenderAPIGlobals.frModelTextureCoordIdx)
    modelVertexIndexIdx <- use (fastRenderAPIGlobals.frModelVertexIndexIdx)
    (tmp, modelTextureCoordIdx', modelVertexIndexIdx') <- request $ do
        textureBuf <- use frModelTextureCoordBuf
        vertexBuf <- use frModelVertexIndexBuf
        setTextureAndVertex textureBuf vertexBuf (fromJust (model^.dmGlCmds)) modelTextureCoordIdx modelVertexIndexIdx 0 []
    let rtmp = reverse tmp
        counts = UV.fromList rtmp
        indexElements = collectIndexElements rtmp modelVertexIndexIdx 0 []
    fastRenderAPIGlobals.frModelTextureCoordIdx .= modelTextureCoordIdx'
    fastRenderAPIGlobals.frModelVertexIndexIdx .= modelVertexIndexIdx'
    return model { _dmTextureCoordBufIdx = modelTextureCoordIdx
                 , _dmVertexIndexBufIdx = modelVertexIndexIdx
                 , _dmCounts = counts
                 , _dmIndexElements = indexElements
                 }

setTextureAndVertex :: MSV.IOVector Float -> MSV.IOVector Int32 -> UV.Vector Word32 -> Int -> Int -> Int -> [Int32] -> QuakeIO ([Int32], Int, Int)
setTextureAndVertex textureBuf vertexBuf order textureCoordIdx vertexIndexIdx orderIndex tmp
    | count /= 0 = do
        let count' = if count < 0 then -count else count
        (textureCoordIdx', vertexIndexIdx', orderIndex') <- setCoords textureBuf vertexBuf order textureCoordIdx vertexIndexIdx (orderIndex + 1) count'
        setTextureAndVertex textureBuf vertexBuf order textureCoordIdx' vertexIndexIdx' orderIndex' (count : tmp)
    | otherwise = return (tmp, textureCoordIdx, vertexIndexIdx)
  where
    count = fromIntegral (order UV.! orderIndex) :: Int32

setCoords :: MSV.IOVector Float -> MSV.IOVector Int32 -> UV.Vector Word32 -> Int -> Int -> Int -> Int32 -> QuakeIO (Int, Int, Int)
setCoords textureBuf vertexBuf order textureCoordIdx vertexIndexIdx orderIndex count
    | count <= 0 = return (textureCoordIdx, vertexIndexIdx, orderIndex)
    | otherwise = do
        io $ MSV.write textureBuf textureCoordIdx (wordToFloat (order UV.! orderIndex))
        io $ MSV.write textureBuf (textureCoordIdx + 1) (wordToFloat (order UV.! (orderIndex + 1)))
        io $ MSV.write vertexBuf vertexIndexIdx (fromIntegral (order UV.! (orderIndex + 2)))
        setCoords textureBuf vertexBuf order (textureCoordIdx + 2) (vertexIndexIdx + 1) (orderIndex + 3) (count - 1)
              
collectIndexElements :: [Int32] -> Int -> Int -> [(Int, Int)] -> V.Vector (Int, Int)
collectIndexElements [] _ _ acc = V.fromList (reverse acc)
collectIndexElements (x:xs) idx pos acc =
    let count = fromIntegral (if x < 0 then negate x else x)
    in collectIndexElements xs idx (pos + count) ((idx + pos, count) : acc)