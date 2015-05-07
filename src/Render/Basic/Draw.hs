module Render.Basic.Draw where

import qualified Data.ByteString as B

import Quake
import Render.OpenGL.GLDriver

initLocal :: Quake ()
initLocal = do
    io (putStrLn "Draw.initLocal") >> undefined -- TODO

stretchPic :: GLDriver -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchPic _ _ _ _ _ _ = do
    io (putStrLn "Draw.stretchPic") >> undefined -- TODO

drawPic :: GLDriver -> Int -> Int -> B.ByteString -> Quake ()
drawPic _ _ _ _ = do
    io (putStrLn "Draw.drawPic") >> undefined -- TODO

drawChar :: GLDriver -> Int -> Int -> Int -> Quake ()
drawChar _ _ _ _ = do
    io (putStrLn "Draw.drawChar") >> undefined -- TODO

fill :: GLDriver -> Int -> Int -> Int -> Int -> Int -> Quake ()
fill _ _ _ _ _ _ = do
    io (putStrLn "Draw.fill") >> undefined -- TODO
