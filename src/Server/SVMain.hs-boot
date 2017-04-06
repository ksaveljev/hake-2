module Server.SVMain
    ( dropClient
    , frame
    , initialize
    , shutdown
    ) where

import qualified Data.ByteString as B

import           Types

dropClient :: Ref ClientT -> Quake ()
frame :: Int -> Quake ()
initialize :: Quake ()
shutdown :: B.ByteString -> Bool -> Quake ()
