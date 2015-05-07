module Render.Basic.Model where

import qualified Data.ByteString as B

import Quake
import QCommon.XCommandT
import Render.OpenGL.GLDriver

rBeginRegistration :: GLDriver -> B.ByteString -> Quake ()
rBeginRegistration _ _ = do
    io (putStrLn "Model.rBeginRegistration") >> undefined -- TODO

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO

modInit :: Quake ()
modInit = do
    io (putStrLn "Model.modInit") >> undefined -- TODO
