module Render.Fast.Model where

import Quake
import QCommon.XCommandT

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO
