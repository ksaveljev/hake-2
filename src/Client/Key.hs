module Client.Key
  ( initialize
  ) where

import           Client.KeyConstants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (.=), (%=))
import           Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (ord, toUpper, chr)
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

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("bind", Just bindF), ("unbind", Just unbindF)
  , ("unbindall", Just unbindAllF), ("bindlist", Just bindListF)
  ] 

initialize :: Quake ()
initialize =
  do globals.gKeyLines .= V.replicate 32 (B.pack [93, 0]) -- 93 is ']'
     globals.gKeyLinePos .= 1
     keyGlobals.kgConsoleKeys %= (UV.// ((consoleKeys `zip` repeat True) ++ [(96, False), (126, False)])) -- 96 is '`', 126 is '~'
     keyGlobals.kgMenuBound %= (UV.// (menuBoundKeys `zip` repeat True))
     Cmd.addInitialCommands initialCommands

bindF :: XCommandT
bindF = XCommandT "Key.bindF" $
  checkArgs =<< Cmd.argc
  where checkArgs c
          | c < 2 = Com.printf "bind <key> [command] : attach a command to a key\n"
          | otherwise =
              do arg <- Cmd.argv 1
                 key <- stringToKeynum arg
                 bind arg key c

bind :: B.ByteString -> Int -> Int -> Quake ()
bind arg key c
  | key == -1 = Com.printf (B.concat ["\"", arg, "\" isn't a valid key\n"])
  | c == 2 = bindInfo arg key
  | otherwise =
      do cmd <- fmap (B.intercalate " ") (mapM Cmd.argv [2..c-1])
         setBinding key (Just cmd)

bindInfo :: B.ByteString -> Int -> Quake ()
bindInfo arg key =
  do keyBindings <- use (globals.gKeyBindings)
     maybe notBound printBindInfo (keyBindings V.! key)
  where notBound = Com.printf (B.concat ["\"", arg, "\" is not bound\n"])
        printBindInfo binding = Com.printf (B.concat ["\"", arg, "\" = \"", binding, "\"\n"])

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
unbindF = XCommandT "Key.unbindF" $
  checkArgs =<< Cmd.argc
  where checkArgs c
          | c /= 2 = Com.printf "unbind <key> : remove commands from a key\n"
          | otherwise = unbind

unbind :: Quake ()
unbind =
  do arg <- Cmd.argv 1
     keynum <- stringToKeynum arg
     clearBinding arg keynum
  where clearBinding arg keynum
          | keynum == -1 = Com.printf (B.concat ["\"", arg, "\" isn't a valid key\n"])
          | otherwise = setBinding keynum Nothing

unbindAllF :: XCommandT
unbindAllF = error "Key.unbindAllF" -- TODO

bindListF :: XCommandT
bindListF = XCommandT "Key.bindListF" $
  do bindings <- use (globals.gKeyBindings)
     V.imapM_ printBinding bindings

printBinding :: Int -> Maybe B.ByteString -> Quake ()
printBinding _ Nothing = return ()
printBinding idx (Just binding)
  | B.null binding = return ()
  | otherwise =
      do strKeynum <- keynumToString idx
         Com.printf (bindingInfo strKeynum)
  where bindingInfo strKeynum = B.concat [strKeynum, " \"", binding, "\"\n"]

keynumToString :: Int -> Quake B.ByteString
keynumToString keynum =
  do keyNames <- use (keyGlobals.kgKeyNames)
     return (convertKeyNum keyNames)
  where convertKeyNum keyNames
          | keynum < 0 || keynum > 255 = "<KEY NOT FOUND>"
          | keynum > 32 && keynum < 127 = encode (chr keynum)
          | otherwise = fromMaybe "<UNKNOWN KEYNUM>" (keyNames V.! keynum)
