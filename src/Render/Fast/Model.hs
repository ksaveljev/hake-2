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
import           QuakeIOState
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Polygon as Polygon
import           Render.ModelT
import           Types

import           Control.Lens (use, preuse, ix, (^.), (.=), (+=), (%=))
import           Control.Monad (when, unless)
import qualified Data.ByteString as B
import qualified Data.Vector.Mutable as MV

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
modForName = error "Model.modForName" -- TODO

resetModelArrays :: Quake ()
resetModelArrays =
  do fastRenderAPIGlobals.frModelTextureCoordIdx .= 0
     fastRenderAPIGlobals.frModelVertexIndexIdx .= 0
