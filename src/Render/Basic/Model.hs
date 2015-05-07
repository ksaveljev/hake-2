module Render.Basic.Model where

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

maxModKnown :: Int
maxModKnown = 512

rBeginRegistration :: GLDriver -> B.ByteString -> Quake ()
rBeginRegistration _ _ = do
    io (putStrLn "Model.rBeginRegistration") >> undefined -- TODO

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO

modInit :: Quake ()
modInit = do
    -- init mod_known
    basicRenderAPIGlobals.brModKnown .= V.replicate maxModKnown newModelT
    basicRenderAPIGlobals.brModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF
