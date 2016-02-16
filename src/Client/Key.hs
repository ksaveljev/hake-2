module Client.Key
  (initialize)
  where

import           Client.KeyConstants
import qualified Game.Cmd as Cmd
import           QuakeState
import           Types

import           Control.Lens ((.=), (%=))
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

keyboardButtons :: [Int]
keyboardButtons = 
  [ kEnter, kKpEnter, kTab, kLeftArrow, kKpLeftArrow, kRightArrow, kKpRightArrow
  , kUpArrow, kKpUpArrow, kDownArrow, kKpDownArrow, kBackspace, kHome, kKpHome
  , kEnd, kKpEnd, kPgUp, kKpPgUp, kPgDn, kKpPgDn, kShift, kIns, kKpIns, kKpDel
  , kKpSlash, kKpPlus, kKpMinus, kKp5 ]

consoleKeys :: [Int]
consoleKeys = [32..127] ++ keyboardButtons

menuBoundKeys :: [Int]
menuBoundKeys = [kF1, kF2, kF3, kF4, kF5, kF6, kF7, kF8, kF9, kF10, kF11, kF12, kEscape]

initialCommands :: [(B.ByteString,XCommandT)]
initialCommands =
  [("bind",bindF), ("unbind",unbindF), ("unbindall",unbindAllF), ("bindlist",bindListF)] 

initialize :: Quake ()
initialize =
  do globals.gKeyLines .= V.replicate 32 (B.pack [93, 0]) -- 93 is ']'
     globals.gKeyLinePos .= 1
     keyGlobals.kgConsoleKeys %= (UV.// ((consoleKeys `zip` repeat True) ++ [(96, False), (126, False)])) -- 96 is '`', 126 is '~'
     keyGlobals.kgMenuBound %= (UV.// (menuBoundKeys `zip` repeat True))
     mapM_ (\(name,cmd) -> Cmd.addCommand name (Just cmd)) initialCommands

bindF :: XCommandT
bindF = error "Key.bindF" -- TODO

unbindF :: XCommandT
unbindF = error "Key.unbindF" -- TODO

unbindAllF :: XCommandT
unbindAllF = error "Key.unbindAllF" -- TODO

bindListF :: XCommandT
bindListF = error "Key.bindListF" -- TODO