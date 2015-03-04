module Game.GameImportT where

import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import Game.EdictT
import Game.PMoveT
import Game.TraceT

-- TODO: function return types - is it really IO ?
data GameImportT =
  GameImportT { bprintf            :: Int -> B.ByteString -> IO ()
              , dprintf            :: B.ByteString -> IO ()
              , cprintf            :: EdictT -> Int -> B.ByteString -> IO ()
              , centerprintf       :: EdictT -> B.ByteString -> IO ()
              , sound              :: EdictT -> Int -> Int -> Float -> Float -> Float -> IO ()
              , positionedSound    :: V3 Float -> EdictT -> Int -> Int -> Float -> Float -> Float -> IO ()
              , configstring       :: Int -> B.ByteString -> IO ()
              , error              :: B.ByteString -> IO ()
              , error2             :: Int -> B.ByteString -> IO ()
              , modelindex         :: B.ByteString -> IO Int
              , soundindex         :: B.ByteString -> IO Int
              , imageindex         :: B.ByteString -> IO Int
              , setmodel           :: EdictT -> B.ByteString -> IO ()
              , trace              :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> IO TraceT
              --, pmove_t.PointContentsAdapter -- TODO: ???
              , inPHS              :: V3 Float -> V3 Float -> IO Bool
              , setAreaPortalState :: Int -> Bool -> IO ()
              , areasConnected     :: Int -> Int -> IO Bool
              , linkentity         :: EdictT -> IO ()
              , unlinkentity       :: EdictT -> IO ()
              , boxEdicts          :: V3 Float -> V3 Float -> UV.Vector EdictT -> Int -> Int -> IO Int
              , pmove              :: PMoveT -> IO ()
              , multicast          :: V3 Float -> Int -> IO ()
              , unicast            :: EdictT -> Bool -> IO ()
              , writeByte          :: Int -> IO ()
              , writeShort         :: Int -> IO ()
              , writeString        :: B.ByteString -> IO ()
              , writePosition      :: V3 Float -> IO ()
              , writeDir           :: V3 Float -> IO ()
              , cvar               :: B.ByteString -> B.ByteString -> Int -> IO CVarT
              , cvarSet            :: B.ByteString -> B.ByteString -> IO CVarT
              , cvarForceset       :: B.ByteString -> B.ByteString -> IO CVarT
              , argc               :: IO Int
              , argv               :: Int -> B.ByteString
              , args               :: IO B.ByteString
              , addCommandString   :: B.ByteString -> IO ()
              }
