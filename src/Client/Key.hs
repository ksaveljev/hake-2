module Client.Key where

import Control.Lens ((.=))
import qualified Data.Vector as V
import qualified Data.ByteString as B

import Quake
import QuakeState
import qualified Game.Cmd as Cmd

init :: Quake ()
init = do
    setupKeyLines

    undefined -- TODO

  where setupKeyLines = do
          let kl = V.replicate 32 $ B.pack [93, 0] -- 93 is ']', 0 is NUL
          globals.keyLines .= kl
          globals.keyLinePos .= 1
