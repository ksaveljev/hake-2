module Client.CLFX
  ( runDLights
  , runLightStyles
  ) where

import           Client.CDLightT
import           Client.ClientStateT
import           Client.CLightStyleT
import qualified Constants
import           QuakeIOState
import           QuakeState
import           Types

import           Control.Lens (use, (^.), (%=), (.=), (&), (.~))
import           Control.Monad (unless)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import           Linear (V3(..))

runDLights :: Quake ()
runDLights =
  do time <- use (globals.gCl.csTime)
     dLights <- request (use cgDLights)
     request (io (runDLight dLights (fromIntegral time) 0 Constants.maxDLights))

runDLight :: MV.IOVector CDLightT -> Float -> Int -> Int -> IO ()
runDLight dLights time idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise = doRunDLight =<< MV.read dLights idx
  where doRunDLight dl
          | dl^.cdlRadius == 0 = runDLight dLights time (idx + 1) maxIdx
          | dl^.cdlDie < time = MV.write dLights idx (dl & cdlRadius .~ 0)
          | otherwise = runDLight dLights time (idx + 1) maxIdx
          -- TODO: original quake2 code does have something else
          -- here (jake2 is missing a part of this function)

runLightStyles :: Quake ()
runLightStyles =
  do time <- use (globals.gCl.csTime)
     lastOfs <- use $ clientGlobals.cgLastOfs
     doRunLightStyles time lastOfs

doRunLightStyles :: Int -> Int -> Quake ()
doRunLightStyles time lastOfs =
  unless (ofs == lastOfs) $
    do clientGlobals.cgLastOfs .= ofs
       clientGlobals.cgLightStyle %= V.map (runLightStyle ofs)
  where ofs = time `div` 100

runLightStyle :: Int -> CLightStyleT -> CLightStyleT
runLightStyle ofs ls
  | ls^.clsLength == 0 = ls & clsValue .~ V3 1 1 1
  | ls^.clsLength == 1 = let v = (ls^.clsMap) UV.! 0
                         in ls & clsValue .~ V3 v v v
  | otherwise = let v = (ls^.clsMap) UV.! (ofs `mod` (ls^.clsLength))
                in ls & clsValue .~ V3 v v v