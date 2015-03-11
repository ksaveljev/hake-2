module Client.RefExportT where

import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Quake
import Client.RefDefT

data RefExportT =
  RefExportT { init                :: Int -> Int -> Quake Bool
             , shutDown            :: Quake ()
             , beginRegistration   :: B.ByteString -> Quake ()
             , registerModel       :: B.ByteString -> Quake ModelT
             , registerSkin        :: B.ByteString -> Quake ImageT
             , registerPic         :: B.ByteString -> Quake ImageT
             , setSky              :: B.ByteString -> Float -> V3 Float -> Quake ()
             , endRegistration     :: Quake ()
             , renderFrame         :: RefDefT -> Quake ()
             , drawGetPicSize      :: Int -> Int -> B.ByteString -> Quake ()
             , drawPic             :: Int -> Int -> B.ByteString -> Quake ()
             , drawStretchPic      :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , drawChar            :: Int -> Int -> Char -> Quake ()
             , drawTileClear       :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , drawFill            :: Int -> Int -> Int -> Int -> Int -> Quake ()
             , drawFadeScreen      :: Quake ()
             , drawStretchRaw      :: Int -> Int -> Int -> Int -> Int -> Int -> UV.Vector Word8 -> Quake ()
             , cinematicSetPalette :: UV.Vector Word8 -> Quake ()
             , beginFrame          :: Float -> Quake ()
             , endFrame            :: Quake ()
             , appActivate         :: Bool -> Quake ()
             , updateScreen        :: XCommandT -> Quake ()
             , apiVersion          :: Int
             , getModelList        :: UV.Vector Int -- TODO: ???
             , getKeyboardHandler  :: Int -- TODO: ???
             }
