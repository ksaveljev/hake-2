module Render.Fast.Draw
  ( drawChar
  , drawPic
  , fadeScreen
  , fill
  , findPic
  , getPicSize
  , initLocal
  , stretchPic
  , stretchRaw
  ) where

import           Types

import qualified Data.ByteString as B
import           Data.IORef

initLocal :: Quake ()
initLocal = error "Draw.initLocal" -- TODO

findPic :: B.ByteString -> Quake (Maybe (IORef ImageT))
findPic = error "Draw.findPic" -- TODO

getPicSize :: B.ByteString -> Quake (Maybe (Int, Int))
getPicSize = error "Draw.getPicSize" -- TODO

drawPic :: Int -> Int -> B.ByteString -> Quake ()
drawPic = error "Draw.drawPic" -- TODO

stretchPic :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchPic = error "Draw.stretchPic" -- TODO

drawChar :: Int -> Int -> Int -> Quake ()
drawChar = error "Draw.drawChar" -- TODO

fill :: Int -> Int -> Int -> Int -> Int -> Quake ()
fill = error "Draw.fill" -- TODO

fadeScreen :: Quake ()
fadeScreen = error "Draw.fadeScreen" -- TODO

stretchRaw :: Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchRaw = error "Draw.stretchRaw" -- TODO
