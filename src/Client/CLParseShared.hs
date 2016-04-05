{-# LANGUAGE Rank2Types #-}
module Client.CLParseShared
  ( loadClientInfo
  , parseClientInfo
  ) where

import Types

import Control.Lens (Traversal')
import qualified Data.ByteString as B

parseClientInfo :: Int -> Quake ()
parseClientInfo = error "CLParse.parseClientInfo" -- TODO

loadClientInfo :: Traversal' QuakeState ClientInfoT -> B.ByteString -> Quake ()
loadClientInfo = error "CLParse.loadClientInfo" -- TODO