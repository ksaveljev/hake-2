module Client.VID
  ( getModeInfo
  , initialize
  , menuDrawF
  , menuInit
  , menuKeyF
  , newWindow
  , printf
  , shutdown
  ) where

import           Types

import qualified Data.ByteString as B

getModeInfo :: Int -> Quake (Maybe (Int, Int))
initialize :: Quake ()
menuDrawF :: XCommandT
menuInit :: Quake ()
menuKeyF :: KeyFuncT
newWindow :: Int -> Int -> Quake ()
printf :: Int -> B.ByteString -> Quake ()
shutdown :: Quake ()