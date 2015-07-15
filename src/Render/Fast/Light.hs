module Render.Fast.Light where

import Control.Lens ((^.), preuse, use, (.=), ix)
import Control.Monad (when, liftM)
import Data.Bits (shiftL)
import Data.Maybe (fromJust)
import Linear (V3)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint _ = do
    io (putStrLn "Light.rLightPoint") >> undefined -- TODO

rPushDLights :: Quake ()
rPushDLights = do
    flashBlendValue <- liftM (^.cvValue) glFlashBlendCVar

    when (flashBlendValue == 0) $ do
      frameCount <- use $ fastRenderAPIGlobals.frFrameCount
      newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
      worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
      Just worldModel <- case fromJust worldModelRef of
                           ModKnownReference modelIdx -> preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx
                           ModInlineReference modelIdx -> preuse $ fastRenderAPIGlobals.frModInline.ix modelIdx

      fastRenderAPIGlobals.frDLightFrameCount .= frameCount + 1 -- because the count hasn't advanced yet for this frame

      markLights newRefDef worldModel 0 (newRefDef^.rdNumDLights)

  where markLights :: RefDefT -> ModelT -> Int -> Int -> Quake ()
        markLights newRefDef worldModel idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let light = (newRefDef^.rdDLights) V.! idx
              rMarkLights light (1 `shiftL` idx) ((worldModel^.mNodes) V.! 0)
              markLights newRefDef worldModel (idx + 1) maxIdx

rMarkLights :: DLightT -> Int -> MNodeT -> Quake ()
rMarkLights _ _ _ = do
    io (putStrLn "Light.rMarkLights") >> undefined -- TODO

rRenderDLights :: Quake ()
rRenderDLights = do
    io (putStrLn "Light.rRenderDLights") >> undefined -- TODO
