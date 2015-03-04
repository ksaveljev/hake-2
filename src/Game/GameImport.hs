module Game.GameImport where

import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.CVar
import Game.Edict
import Game.PMove
import Game.Trace

-- TODO: function return types - is it really IO ?
data GameImport =
  GameImport { bprintf            :: Int -> B.ByteString -> IO ()
             , dprintf            :: B.ByteString -> IO ()
             , cprintf            :: Edict -> Int -> B.ByteString -> IO ()
             , centerprintf       :: Edict -> B.ByteString -> IO ()
             , sound              :: Edict -> Int -> Int -> Float -> Float -> Float -> IO ()
             , positionedSound    :: V3 Float -> Edict -> Int -> Int -> Float -> Float -> Float -> IO ()
             , configstring       :: Int -> B.ByteString -> IO ()
             , error              :: B.ByteString -> IO ()
             , error2             :: Int -> B.ByteString -> IO ()
             , modelindex         :: B.ByteString -> IO Int
             , soundindex         :: B.ByteString -> IO Int
             , imageindex         :: B.ByteString -> IO Int
             , setmodel           :: Edict -> B.ByteString -> IO ()
             , trace              :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Edict -> Int -> IO Trace
             --, pmove_t.PointContentsAdapter -- TODO: ???
             , inPHS              :: V3 Float -> V3 Float -> IO Bool
             , setAreaPortalState :: Int -> Bool -> IO ()
             , areasConnected     :: Int -> Int -> IO Bool
             , linkentity         :: Edict -> IO ()
             , unlinkentity       :: Edict -> IO ()
             , boxEdicts          :: V3 Float -> V3 Float -> UV.Vector Edict -> Int -> Int -> IO Int
             , pmove              :: PMove -> IO ()
             , multicast          :: V3 Float -> Int -> IO ()
             , unicast            :: Edict -> Bool -> IO ()
             , writeByte          :: Int -> IO ()
             , writeShort         :: Int -> IO ()
             , writeString        :: B.ByteString -> IO ()
             , writePosition      :: V3 Float -> IO ()
             , writeDir           :: V3 Float -> IO ()
             , cvar               :: B.ByteString -> B.ByteString -> Int -> IO CVar
             , cvarSet            :: B.ByteString -> B.ByteString -> IO CVar
             , cvarForceset       :: B.ByteString -> B.ByteString -> IO CVar
             , argc               :: IO Int
             , argv               :: Int -> B.ByteString
             , args               :: IO B.ByteString
             , addCommandString   :: B.ByteString -> IO ()
             }
