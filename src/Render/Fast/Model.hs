module Render.Fast.Model where

import Control.Lens ((.=))
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants

maxModKnown :: Int
maxModKnown = 512

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO

modInit :: Quake ()
modInit = do
    -- init mod_known
    fastRenderAPIGlobals.frModKnown .= V.replicate maxModKnown newModelT
    fastRenderAPIGlobals.frModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF
