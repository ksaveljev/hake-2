{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Mesh where

import Control.Lens ((^.), use)
import Control.Monad (when, liftM, unless)
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
import CVarVariables
import qualified Constants
import qualified Client.VID as VID
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import qualified Render.Fast.Light as Light
import qualified Util.Math3D as Math3D

rDrawAliasModel :: IORef EntityT -> Quake ()
rDrawAliasModel entRef = do
    e <- io $ readIORef entRef
    done <- checkIfDone e

    unless done $ do
      Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
      currentModel <- io $ readIORef currentModelRef

      let Just (AliasModelExtra pAliasHdr) = currentModel^.mExtraData

      -- get lighting information
      --
      -- PMM - rewrote, reordered to handle new shells & mixing
      -- PMM - 3.20 code .. replaced with original way of doing it to keep mod
      -- authors happy
      shadeLight <- buildShadeLight

      io $ print "SHADE LIGHT"
      io $ print shadeLight

      io (putStrLn "Mesh.rDrawAliasModel") >> undefined -- TODO

  where checkIfDone :: EntityT -> Quake Bool
        checkIfDone e = do
          if (e^.enFlags) .&. Constants.rfWeaponModel == 0
            then
              rCullAliasModel entRef
            else do
              handValue <- liftM (^.cvValue) handCVar
              return $ if handValue == 2 then True else False

        buildShadeLight :: Quake (V3 Float)
        buildShadeLight = do
          Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
          currentEntity <- io $ readIORef currentEntityRef

          shadeLight <- if | (currentEntity^.enFlags) .&. (Constants.rfShellHalfDam .|. Constants.rfShellGreen .|. Constants.rfShellRed .|. Constants.rfShellBlue .|. Constants.rfShellDouble) /= 0 -> do
                               let (a, b, c) = if (currentEntity^.enFlags) .&. Constants.rfShellHalfDam /= 0
                                                   then (0.56, 0.59, 0.45)
                                                   else (0, 0, 0)
                                   (a', b') = if (currentEntity^.enFlags) .&. Constants.rfShellDouble /= 0
                                                then (0.9, 0.7)
                                                else (a, b)
                                   a'' = if (currentEntity^.enFlags) .&. Constants.rfShellRed /= 0 then 1 else a'
                                   b'' = if (currentEntity^.enFlags) .&. Constants.rfShellGreen /= 0 then 1 else b'
                                   c' = if (currentEntity^.enFlags) .&. Constants.rfShellBlue /= 0 then 1 else c
                               return (V3 a'' b'' c')

                           | (currentEntity^.enFlags) .&. Constants.rfFullBright /= 0 ->
                               return (V3 1 1 1)

                           | otherwise -> do
                               shadeLight <- Light.rLightPoint (currentEntity^.eOrigin)

                               -- player lighting hack for communication back to server
                               -- big hack!
                               when ((currentEntity^.enFlags) .&. Constants.rfWeaponModel /= 0) $ do
                                 let v = if (shadeLight^._x) > (shadeLight^._y)
                                           then
                                             if (shadeLight^._x) > (shadeLight^._z)
                                               then shadeLight^._x
                                               else shadeLight^._z
                                           else
                                             if (shadeLight^._y) > (shadeLight^._z)
                                               then shadeLight^._y
                                               else shadeLight^._z

                                 clLightLevelCVar >>= \lightLevel -> CVar.update lightLevel { _cvValue = 150 * v }

                               monoLightmap <- liftM (^.cvString) glMonoLightMapCVar

                               return $ if monoLightmap `BC.index` 0 /= '0'
                                          then let s = shadeLight^._x
                                                   s' = if s < (shadeLight^._y) then shadeLight^._y else s
                                                   s'' = if s' < (shadeLight^._z) then shadeLight^._z else s'
                                               in V3 s'' s'' s''
                                          else shadeLight

          let shadeLight' = if (currentEntity^.enFlags) .&. Constants.rfMinLight /= 0
                              then let v = (shadeLight^._x) > 0.1 || (shadeLight^._y) > 0.1 || (shadeLight^._z) > 0.1
                                   in if v
                                        then shadeLight
                                        else V3 0.1 0.1 0.1
                              else
                                shadeLight

          newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef

          let shadeLight'' = if (currentEntity^.enFlags) .&. Constants.rfGlow /= 0 -- bonus items will pulse with time
                               then
                                 let scale = 0.1 * sin (7 * (newRefDef^.rdTime))
                                     smin = fmap (* 0.8) shadeLight'
                                     a = if (shadeLight'^._x) + scale < (smin^._x)
                                           then (smin^._x)
                                           else (shadeLight'^._x) + scale
                                     b = if (shadeLight'^._y) + scale < (smin^._y)
                                           then (smin^._y)
                                           else (shadeLight'^._y) + scale
                                     c = if (shadeLight'^._z) + scale < (smin^._z)
                                           then (smin^._z)
                                           else (shadeLight'^._z) + scale
                                 in V3 a b c
                               else
                                 shadeLight'

          let shadeLight''' = if ((newRefDef^.rdRdFlags) .&. Constants.rdfIrGoggles /= 0) && ((currentEntity^.enFlags) .&. Constants.rfIrVisible /= 0)
                                then V3 1 0 0
                                else shadeLight''

          return shadeLight'''

rCullAliasModel :: IORef EntityT -> Quake Bool
rCullAliasModel entRef = do
    e <- io $ readIORef entRef

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
