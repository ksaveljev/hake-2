{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Render.Fast.Model where

import Control.Lens ((.=), (+=), preuse, ix, (^.), Traversal')
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.XCommandT
import Render.OpenGL.GLDriver
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified Render.Fast.Polygon as Polygon

maxModKnown :: Int
maxModKnown = 512

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
      modFree (fastRenderAPIGlobals.frModKnown.ix 0)

    modelRef <- modForName fullName True
    fastRenderAPIGlobals.frWorldModel .= modelRef

    fastRenderAPIGlobals.frViewCluster .= (-1)

modFree :: Traversal' QuakeState ModelT -> Quake ()
modFree _ = do
    io (putStrLn "Model.modFree") >> undefined -- TODO

modForName :: B.ByteString -> Bool -> Quake (Maybe ModelReference)
modForName _ _ = do
    io (putStrLn "Model.modForName") >> undefined -- TODO

resetModelArrays :: Quake ()
resetModelArrays = do
    io (putStrLn "IMPLEMENT ME! DO WE NEED THIS? Model.resetModelArrays") >> return ()
    --io (putStrLn "Model.resetModelArrays") >> undefined -- TODO
