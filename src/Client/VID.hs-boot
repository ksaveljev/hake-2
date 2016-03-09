module Client.VID
  ( getModeInfo
  , initialize
  , menuDrawF
  , menuInit
  , menuKeyF
  , newWindow
  , printf
  ) where

import           Types

import qualified Data.ByteString as B

getModeInfo :: Int -> Quake (Maybe (Int, Int))
initialize :: Quake ()
menuInit :: Quake ()
newWindow :: Int -> Int -> Quake ()
printf :: Int -> B.ByteString -> Quake ()
menuDrawF :: XCommandT
menuKeyF :: KeyFuncT