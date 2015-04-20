module Render.Fast.Model where

import Quake
import QCommon.XCommandT

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO

modInit :: Quake ()
modInit = io (putStrLn "Model.modInit") >> undefined -- TODO
