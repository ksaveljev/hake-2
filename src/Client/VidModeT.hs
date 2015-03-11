{-# LANGUAGE TemplateHaskell #-}
module Client.VidModeT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data VidModeT =
  VidModeT { _vmDescription :: B.ByteString
           , _vmWidth       :: Int
           , _vmHeight      :: Int
           , _vmMode        :: Int
           }


makeLenses ''VidModeT
