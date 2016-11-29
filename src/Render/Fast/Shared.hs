module Render.Fast.Shared 
    ( clusterPVS
    ) where

import           Control.Lens             (use, (^.), _1)
import           Data.Bits                (shiftR)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (isNothing)
import           Data.Monoid              ((<>), mempty)
import qualified Data.Vector              as V
import           Data.Word                (Word8)

import qualified Constants
import qualified QCommon.Com              as Com
import           QCommon.QFiles.BSP.DVisT
import           QuakeState
import           Render.ModelT
import           Types

clusterPVS :: Int -> ModelT -> Quake B.ByteString
clusterPVS cluster model
    | cluster == -1 || isNothing (model^.mVis) =
        use (fastRenderAPIGlobals.frModNoVis)
    | otherwise = do
        modelVisibility <- use (fastRenderAPIGlobals.frModelVisibility)
        -- TODO: instead of directly using _1 we should somehow use Constants.dvisPvs
        maybe visibilityError (return . decompressVis cluster modelVisibility model) (model^.mVis)
  where
    visibilityError = do
        Com.fatalError "Model.clusterPVS model^.mVis is Nothing"
        return B.empty

decompressVis :: Int -> Maybe B.ByteString -> ModelT -> DVisT -> B.ByteString
decompressVis cluster Nothing model vis =
    B.unfoldr simpleVisibility 0
  where
    simpleVisibility idx
        | idx >= Constants.maxMapLeafs `div` 8 = Nothing
        | idx < row = Just (0xFF, idx + 1)
        | otherwise = Just (0, idx + 1)
    row = ((vis^.dvNumClusters) + 7) `shiftR` 3
decompressVis cluster (Just modelVisibility) model vis =
    buildVis modelVisibility row offset 0 mempty
  where
    offset = ((vis^.dvBitOfs) V.! cluster)^._1
    row = ((vis^.dvNumClusters) + 7) `shiftR` 3

-- TODO: old implementation, refactor? or rewrite using Unboxed Storable Vector with ST?
buildVis :: B.ByteString -> Int -> Int -> Int -> BB.Builder -> B.ByteString
buildVis modelVisibility row inp outp builder
    | modelVisibility `B.index` inp /= 0 =
        let builder' = builder <> BB.word8 (modelVisibility `B.index` inp)
        in if outp + 1 < row
            then
                buildVis modelVisibility row (inp + 1) (outp + 1) builder'
            else
                let result = BL.toStrict (BB.toLazyByteString builder')
                    diff = Constants.maxMapLeafs `div` 8 - (B.length result)
                in if diff > 0
                    then result `B.append` (B.replicate diff 0)
                    else result
    | otherwise =
        let c = modelVisibility `B.index` (inp + 1)
            builder' = buildEmpty builder c
        in if outp + fromIntegral c < row
            then
                buildVis modelVisibility row (inp + 2) (outp + fromIntegral c) builder'
            else
                let result = BL.toStrict (BB.toLazyByteString builder')
                    diff = Constants.maxMapLeafs `div` 8 - (B.length result)
                in if diff > 0
                    then result `B.append` (B.replicate diff 0)
                    else result

buildEmpty :: BB.Builder -> Word8 -> BB.Builder
buildEmpty builder c
    | c <= 0 = builder
    | otherwise = buildEmpty (builder <> (BB.word8 0)) (c - 1)