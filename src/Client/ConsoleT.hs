{-# LANGUAGE TemplateHaskell #-}
module Client.ConsoleT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

data ConsoleT =
  ConsoleT { _cInitialized :: Bool
           , _cText        :: B.ByteString
           , _cCurrent     :: Int
           , _cX           :: Int
           , _cDisplay     :: Int
           , _cOrMask      :: Int
           , _cLineWidth   :: Int
           , _cTotalLines  :: Int
           , _cCursorSpeed :: Float
           , _cVisLines    :: Int
           , _cTimes       :: UV.Vector Float
           }

makeLenses ''ConsoleT
