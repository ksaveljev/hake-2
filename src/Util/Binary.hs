module Util.Binary
  ( encode
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

encode :: Show a => a -> B.ByteString
encode = BC.pack . show
