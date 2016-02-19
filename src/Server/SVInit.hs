module Server.SVInit
  ( svMap
  ) where

import           Types

import qualified Data.ByteString as B

svMap :: Bool -> B.ByteString -> Bool -> Quake ()
svMap = error "SVInit.svMap" -- TODO