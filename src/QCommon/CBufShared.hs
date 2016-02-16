module QCommon.CBufShared where

import           Types

import qualified Data.ByteString as B

insertText :: B.ByteString -> Quake ()
insertText = error "CBuf.insertText" -- TODO