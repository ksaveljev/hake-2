{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Mesh where

import Control.Lens ((^.), use)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), complement, shiftL)
import Data.Int (Int32)
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Maybe (fromJust)
import Linear (V3(..), _x, _y, _z, dot)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import qualified Constants
import qualified Client.VID as VID
import qualified Util.Math3D as Math3D

rDrawAliasModel :: IORef EntityT -> Quake ()
rDrawAliasModel e = do
    io (putStrLn "Mesh.rDrawAliasModel") >> undefined -- TODO

rCullAliasModel :: IORef EntityT -> Quake Bool
rCullAliasModel entRef = do
    e <- io $ readIORef entRef

    -- data ModelExtra = AliasModelExtra DMdlT | SpriteModelExtra DSpriteT
    Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
    currentModel <- io $ readIORef currentModelRef

    let Just (AliasModelExtra pAliasHdr) = currentModel^.mExtraData

    when ((e^.eFrame) >= (pAliasHdr^.dmNumFrames) || (e^.eFrame) < 0) $ do
      VID.printf Constants.printAll ("R_CullAliasModel " `B.append` (currentModel^.mName) `B.append` ": no such frame " `B.append` BC.pack (show (e^.eFrame)) `B.append` "\n") -- IMPROVE?
      io $ modifyIORef' entRef (\v -> v { _eFrame = 0 })

    when ((e^.eOldFrame) >= (pAliasHdr^.dmNumFrames) || (e^.eOldFrame) < 0) $ do
      VID.printf Constants.printAll ("R_CullAliasModel " `B.append` (currentModel^.mName) `B.append` ": no such oldframe " `B.append` BC.pack (show (e^.eOldFrame)) `B.append` "\n") -- IMPROVE?
      io $ modifyIORef' entRef (\v -> v { _eOldFrame = 0})

    e' <- io $ readIORef entRef

    let pFrame = (fromJust $ pAliasHdr^.dmAliasFrames) V.! (e'^.eFrame)
        pOldFrame = (fromJust $ pAliasHdr^.dmAliasFrames) V.! (e'^.eOldFrame)

    -- compute axially aligned mins and maxs
    let (mins, maxs) = if pFrame == pOldFrame
                         then (pFrame^.dafTranslate, (pFrame^.dafTranslate) + fmap (* 255) (pFrame^.dafScale))
                         else let thismaxs = (pFrame^.dafTranslate) + fmap (* 255) (pFrame^.dafScale)
                                  oldmaxs = (pOldFrame^.dafTranslate) + fmap (* 255) (pOldFrame^.dafScale)
                                  mina = if (pFrame^.dafTranslate._x) < (pOldFrame^.dafTranslate._x) then pFrame^.dafTranslate._x else pOldFrame^.dafTranslate._x
                                  minb = if (pFrame^.dafTranslate._y) < (pOldFrame^.dafTranslate._y) then pFrame^.dafTranslate._y else pOldFrame^.dafTranslate._y
                                  minc = if (pFrame^.dafTranslate._z) < (pOldFrame^.dafTranslate._z) then pFrame^.dafTranslate._z else pOldFrame^.dafTranslate._z
                                  maxa = if (thismaxs^._x) > (oldmaxs^._x) then thismaxs^._x else oldmaxs^._x
                                  maxb = if (thismaxs^._y) > (oldmaxs^._y) then thismaxs^._y else oldmaxs^._y
                                  maxc = if (thismaxs^._z) > (oldmaxs^._z) then thismaxs^._z else oldmaxs^._z
                              in (V3 mina minb minc, V3 maxa maxb maxc)

    -- compute a full bounding box
    let bbox = V.generate 8 (computeBoundingBox mins maxs)
    -- rotate the bounding box
    let tmp = let V3 a b c = mins
              in V3 a (negate b) c -- TODO: jake2 code uses YAW from Constants
        (Just v0, Just v1, Just v2) = Math3D.angleVectors tmp True True True
        bbox' = fmap (rotateBoundingBox (e'^.eOrigin) v0 v1 v2) bbox
        aggregateMask = complement 0 :: Int32

    frustum <- use $ fastRenderAPIGlobals.frFrustum
    frustum' <- io $ V.mapM readIORef frustum
    let mask = computeAggregateMask frustum' bbox' aggregateMask 0 8

    return $ if mask /= 0 then True else False

  where computeBoundingBox :: V3 Float -> V3 Float -> Int -> V3 Float
        computeBoundingBox mins maxs idx =
          let a = if idx .&. 1 /= 0 then mins^._x else maxs^._x
              b = if idx .&. 2 /= 0 then mins^._y else maxs^._y
              c = if idx .&. 4 /= 0 then mins^._z else maxs^._z
          in V3 a b c

        rotateBoundingBox :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
        rotateBoundingBox origin v0 v1 v2 tmp =
          let a = v0 `dot` tmp
              b = v1 `dot` tmp
              c = v2 `dot` tmp
          in (V3 a b c) + origin

        computeAggregateMask :: V.Vector CPlaneT -> V.Vector (V3 Float) -> Int32 -> Int -> Int -> Int32
        computeAggregateMask frustum bbox aggregateMask idx maxIdx
          | idx >= maxIdx = aggregateMask
          | otherwise =
              let mask = computeMask frustum (bbox V.! idx) 0 0 4
              in computeAggregateMask frustum bbox (aggregateMask .&. mask) (idx + 1) maxIdx

        computeMask :: V.Vector CPlaneT -> V3 Float -> Int32 -> Int -> Int -> Int32
        computeMask frustum bbox mask idx maxIdx
          | idx >= maxIdx = mask
          | otherwise =
              let dp = ((frustum V.! idx)^.cpNormal) `dot` bbox
              in if dp - ((frustum V.! idx)^.cpDist) < 0
                   then computeMask frustum bbox (mask .|. (1 `shiftL` idx)) (idx + 1) maxIdx
                   else computeMask frustum bbox mask (idx + 1) maxIdx
