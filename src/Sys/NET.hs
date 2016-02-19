module Sys.NET
  ( config
  , initialize
  ) where

import Types

initialize :: Quake ()
initialize = return () -- nothing to do

config :: Bool -> Quake ()
config = error "NET.config" -- TODO