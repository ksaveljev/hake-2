module Client.CLFX
  ( clearEffects
  , parseMuzzleFlash
  , parseMuzzleFlash2
  , runDLights
  , runLightStyles
  , setLightStyle
  ) where

import           Client.CDLightT
import           Client.ClientStateT
import           Client.CLightStyleT
import           Client.CParticleT
import qualified Constants
import qualified QCommon.Com as Com
import           QuakeIOState
import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Lens (preuse, use, ix, (^.), (%=), (.=), (&), (.~))
import           Control.Monad (unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (ord)
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

parseMuzzleFlash :: Quake ()
parseMuzzleFlash = error "CLFX.parseMuzzleFlash" -- TODO

parseMuzzleFlash2 :: Quake ()
parseMuzzleFlash2 = error "CLFX.parseMuzzleFlash2" -- TODO

clearEffects :: Quake ()
clearEffects =
  do clearParticles
     clearDLights
     clearLightStyles

clearParticles :: Quake ()
clearParticles =
  do clientGlobals.cgFreeParticles .= Just (Ref 0)
     clientGlobals.cgActiveParticles .= Nothing
     request doClearParticles

doClearParticles :: QuakeIO ()
doClearParticles =
  do particles <- use cgParticles
     io (mapM_ (clearParticle particles) [0..MV.length particles-1])
  where clearParticle particles idx
          | idx == MV.length particles - 1 =
              MV.modify particles (\v -> v & cpNext .~ Nothing) idx
          | otherwise =
              MV.modify particles (\v -> v & cpNext .~ Just (Ref (idx + 1))) idx 

clearDLights :: Quake ()
clearDLights = request doClearDLights

doClearDLights :: QuakeIO ()
doClearDLights =
  do dlights <- use cgDLights
     io (MV.set dlights newCDLightT)

clearLightStyles :: Quake ()
clearLightStyles =
  do clientGlobals.cgLightStyle .= V.replicate Constants.maxLightStyles newCLightStyleT
     clientGlobals.cgLastOfs .= -1

setLightStyle :: Int -> Quake ()
setLightStyle csIdx =
  do str <- preuse (globals.gCl.csConfigStrings.ix (csIdx + Constants.csLights))
     maybe configStringError doSetLightStyle str
  where configStringError = Com.fatalError "CLFX.setLightStyle str is Nothing"
        doSetLightStyle str =
          do when (B.length str >= Constants.maxQPath) $
               Com.comError Constants.errDrop ("svc_lightstyle length=" `B.append` encode (B.length str))
             clientGlobals.cgLightStyle.ix csIdx %= (\v -> v & clsLength .~ B.length str
                                                             & clsMap .~ UV.unfoldrN (B.length str) (buildLightStyle str (fromIntegral (ord 'm' - ord 'a'))) 0)
        buildLightStyle str d idx =
          let a = fromIntegral (ord (str `BC.index` idx) - ord 'a') :: Float
          in Just (a / d, idx + 1)
{-
setLightStyle csIdx = do
    Just str <- preuse $ globals.cl.csConfigStrings.ix (csIdx + Constants.csLights)
    let len = B.length str

    when (len >= Constants.maxQPath) $
      Com.comError Constants.errDrop $ 

    let d :: Float = fromIntegral (ord 'm' - ord 'a') -- so we do not recalculate it every time
        lsMap = UV.unfoldr buildLightStyle (str, d, 0)

    zoom (clientGlobals.cgLightStyle.ix csIdx) $ do
      clsLength .= len
      clsMap .=  lsMap -- TODO: make sure we never want to access something beyond length

  where buildLightStyle :: (B.ByteString, Float, Int) -> Maybe (Float, (B.ByteString, Float, Int))
        buildLightStyle (str, d, idx)
          | idx >= B.length str = Nothing
          | otherwise =
              let a :: Float = fromIntegral $ ord (str `BC.index` idx) - ord 'a'
              in Just (a / d, (str, d, idx + 1))
              -}