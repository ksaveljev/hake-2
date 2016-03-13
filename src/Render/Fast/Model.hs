module Render.Fast.Model
  ( freeAll
  , modelListF
  , modInit
  , rBeginRegistration
  , rRegisterModel
  , rEndRegistration
  ) where

import qualified Constants
import           QuakeIOState
import           QuakeState
import           Render.ModelT
import           Types

import           Control.Lens (use, (^.), (.=))
import qualified Data.ByteString as B
import           Data.IORef (IORef)
import qualified Data.Vector.Mutable as MV

modInit :: Quake ()
modInit =
  do request resetModels
     fastRenderAPIGlobals.frModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF
  where resetModels =
          do models <- use frModKnown
             MV.set models newModelT

rBeginRegistration :: B.ByteString -> Quake ()
rBeginRegistration = error "Model.rBeginRegistration" -- TODO

rRegisterModel :: B.ByteString -> Quake (Maybe (IORef ModelT))
rRegisterModel = error "Model.rRegisterModel" -- TODO

rEndRegistration :: Quake ()
rEndRegistration = error "Model.rEndRegistration" -- TODO

freeAll :: Quake ()
freeAll =
  do num <- use (fastRenderAPIGlobals.frModNumKnown)
     request $
       do models <- use frModKnown
          freeModels models 0 num

freeModels :: MV.IOVector ModelT -> Int -> Int -> QuakeIO ()
freeModels models idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise =
      do model <- io (MV.read models idx)
         freeModelWithExtraData (model^.mExtraData)
         freeModels models (idx + 1) maxIdx
  where freeModelWithExtraData Nothing = return ()
        freeModelWithExtraData (Just _) = io (MV.write models idx newModelT)

modelListF :: XCommandT
modelListF = error "Model.modelListF" -- TODO
