module Client.Key where

import Control.Lens ((.=), (%=))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

import Quake
import QuakeState
import Client.KeyConstants
import qualified Game.Cmd as Cmd

init :: Quake ()
init = do
    setupKeyLines

    keyGlobals.consoleKeys %= (UV.// (([32..127] `zip` repeat True) ++ 
            ([kEnter, kKpEnter, kTab, kLeftArrow, kKpLeftArrow,
             kRightArrow, kKpRightArrow, kUpArrow, kKpUpArrow,
             kDownArrow, kKpDownArrow, kBackspace, kHome, kKpHome,
             kEnd, kKpEnd, kPgUp, kKpPgUp, kPgDn, kKpPgDn, kShift,
             kIns, kKpIns, kKpDel, kKpSlash, kKpPlus, kKpMinus, 
             kKp5] `zip` repeat True) ++ [(96, False), (126, False)]))

    undefined -- TODO

  where setupKeyLines = do
          let kl = V.replicate 32 $ B.pack [93, 0] -- 93 is ']', 0 is NUL
          globals.keyLines .= kl
          globals.keyLinePos .= 1
