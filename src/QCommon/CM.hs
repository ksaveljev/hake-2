module QCommon.CM
  ( areasConnected
  , entityString
  , inlineModel
  , loadMap
  , numInlineModels
  , setAreaPortalState
  ) where

import qualified QCommon.Com as Com
import qualified Constants
import           QuakeState
import           Types
import qualified Util.Lib as Lib

import           Control.Lens (use)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

setAreaPortalState :: Int -> Bool -> Quake ()
setAreaPortalState = error "CM.setAreaPortalState" -- TODO

areasConnected :: Int -> Int -> Quake Bool
areasConnected = error "CM.areasConnected" -- TODO

loadMap :: B.ByteString -> Bool -> [Int] -> Quake (Int, [Int]) -- return model index (cmGlobals.cmMapCModels) and checksum
loadMap = error "CM.loadMap" -- TODO

inlineModel :: B.ByteString -> Quake (Ref CModelT)
inlineModel name =
  do checkName
     checkNumCModels =<< numInlineModels
     return (Ref num)
  where checkName
          | B.null name || BC.head name /= '*' =
              Com.comError Constants.errDrop "CM_InlineModel: bad name"
          | otherwise = return ()
        checkNumCModels numCModels
          | num < 1 || num >= numCModels =
              Com.comError Constants.errDrop "CM_InlineModel: bad number"
          | otherwise = return ()
        num = Lib.atoi (B.drop 1 name)

numInlineModels :: Quake Int
numInlineModels = use (cmGlobals.cmNumCModels)

entityString :: Quake B.ByteString
entityString = use (cmGlobals.cmMapEntityString)
