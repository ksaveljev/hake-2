module Sound.S
  ( initialize
  ) where

import Types

initialize :: Quake ()
initialize = request (io (putStrLn "S.initialize IMPLEMENT ME!"))
