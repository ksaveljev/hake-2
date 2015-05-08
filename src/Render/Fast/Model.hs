{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
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
import Render.OpenGL.GLDriver
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Render.Fast.Polygon as Polygon
import qualified Util.Lib as Lib
import qualified Util.Binary as Binary

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
loadBrushModel _ _ = do
    io (putStrLn "Model.loadBrushModel") >> undefined -- TODO
