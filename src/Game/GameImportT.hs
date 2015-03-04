{-# LANGUAGE TemplateHaskell #-}
module Game.GameImportT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import Game.EdictT
import Game.PMoveT
import Game.TraceT

-- TODO: function return types - is it really IO ?
data GameImportT =
  GameImportT { _bprintf            :: Int -> B.ByteString -> IO ()
              , _dprintf            :: B.ByteString -> IO ()
              , _cprintf            :: EdictT -> Int -> B.ByteString -> IO ()
              , _centerprintf       :: EdictT -> B.ByteString -> IO ()
              , _sound              :: EdictT -> Int -> Int -> Float -> Float -> Float -> IO ()
              , _positionedSound    :: V3 Float -> EdictT -> Int -> Int -> Float -> Float -> Float -> IO ()
              , _configstring       :: Int -> B.ByteString -> IO ()
              , _error              :: B.ByteString -> IO ()
              , _error2             :: Int -> B.ByteString -> IO ()
              , _modelindex         :: B.ByteString -> IO Int
              , _soundindex         :: B.ByteString -> IO Int
              , _imageindex         :: B.ByteString -> IO Int
              , _setmodel           :: EdictT -> B.ByteString -> IO ()
              , _trace              :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> IO TraceT
              --, pmove_t.PointContentsAdapter -- TODO: ???
              , _inPHS              :: V3 Float -> V3 Float -> IO Bool
              , _setAreaPortalState :: Int -> Bool -> IO ()
              , _areasConnected     :: Int -> Int -> IO Bool
              , _linkentity         :: EdictT -> IO ()
              , _unlinkentity       :: EdictT -> IO ()
              , _boxEdicts          :: V3 Float -> V3 Float -> UV.Vector EdictT -> Int -> Int -> IO Int
              , _pmove              :: PMoveT -> IO ()
              , _multicast          :: V3 Float -> Int -> IO ()
              , _unicast            :: EdictT -> Bool -> IO ()
              , _writeByte          :: Int -> IO ()
              , _writeShort         :: Int -> IO ()
              , _writeString        :: B.ByteString -> IO ()
              , _writePosition      :: V3 Float -> IO ()
              , _writeDir           :: V3 Float -> IO ()
              , _cvar               :: B.ByteString -> B.ByteString -> Int -> IO CVarT
              , _cvarSet            :: B.ByteString -> B.ByteString -> IO CVarT
              , _cvarForceset       :: B.ByteString -> B.ByteString -> IO CVarT
              , _argc               :: IO Int
              , _argv               :: Int -> B.ByteString
              , _args               :: IO B.ByteString
              , _addCommandString   :: B.ByteString -> IO ()
              }

makeLenses ''GameImportT
