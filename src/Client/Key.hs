module Client.Key
  ( initialize
  ) where

import           Client.KeyConstants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import           QuakeState
import           Types

import           Control.Lens (use, (.=), (%=))
import           Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (ord, toUpper)
import           Data.Maybe (fromMaybe)
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
bindF = XCommandT "Key.bindF" $
  do c <- Cmd.argc
     checkArgs c
  where checkArgs c | c < 2 = Com.printf "bind <key> [command] : attach a command to a key\n"
                    | otherwise =
                        do arg <- Cmd.argv 1
                           key <- stringToKeynum arg
                           bind arg key c

bind :: B.ByteString -> Int -> Int -> Quake ()
bind arg key c | key == -1 = Com.printf ("\"" `B.append` arg `B.append` "\" isn't a valid key\n")
               | c == 2 = bindInfo arg key
               | otherwise =
                   do cmd <- fmap (B.intercalate " ") (mapM Cmd.argv [2..c-1])
                      setBinding key (Just cmd)

bindInfo :: B.ByteString -> Int -> Quake ()
bindInfo arg key =
  do keyBindings <- use (globals.gKeyBindings)
     maybe notBound printBindInfo (keyBindings V.! key)
  where notBound = Com.printf ("\"" `B.append` arg `B.append` "\" is not bound\n")
        printBindInfo binding = Com.printf ("\"" `B.append` arg `B.append` "\" = \"" `B.append` binding `B.append` "\"\n")

setBinding :: Int -> Maybe B.ByteString -> Quake ()
setBinding keyNum binding =
    unless (keyNum == -1) $
      globals.gKeyBindings %= (V.// [(keyNum, binding)])

stringToKeynum :: B.ByteString -> Quake Int
stringToKeynum str
  | B.length str == 1 = return (ord (BC.index str 0))
  | otherwise =
      do keyNames <- use (keyGlobals.kgKeyNames)
         return (fromMaybe (-1) (V.findIndex (== Just upperStr) keyNames))
  where upperStr = BC.map toUpper str

unbindF :: XCommandT
unbindF = error "Key.unbindF" -- TODO

unbindAllF :: XCommandT
unbindAllF = error "Key.unbindAllF" -- TODO

bindListF :: XCommandT
bindListF = error "Key.bindListF" -- TODO