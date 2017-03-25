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

import qualified Data.ByteString as B

import           Types

getModeInfo :: Int -> Quake (Maybe (Int, Int))
initialize :: Quake ()
menuDrawF :: XCommandT
menuInit :: Quake ()
menuKeyF :: KeyFuncT
newWindow :: Int -> Int -> Quake ()
printf :: Int -> B.ByteString -> Quake ()
shutdown :: Quake ()