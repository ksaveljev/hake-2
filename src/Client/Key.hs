{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Key where

import Data.Char (ord, toUpper)
import Control.Lens ((.=), (%=), (^.))
import Control.Monad.State (liftM, get)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import QCommon.XCommandT
import Client.KeyConstants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com

init :: Quake ()
init = do
    setupKeyLines

    keyGlobals.consoleKeys %= (UV.// (([32..127] `zip` repeat True) ++ 
            ([kEnter, kKpEnter, kTab, kLeftArrow, kKpLeftArrow,
             kRightArrow, kKpRightArrow, kUpArrow, kKpUpArrow,
             kDownArrow, kKpDownArrow, kBackspace, kHome, kKpHome,
             kEnd, kKpEnd, kPgUp, kKpPgUp, kPgDn, kKpPgDn, kShift,
             kIns, kKpIns, kKpDel, kKpSlash, kKpPlus, kKpMinus, 
             kKp5] `zip` repeat True) ++ [(96, False), (126, False)])) -- 96 is '`', 126 is '~'

    keyGlobals.menuBound %= (UV.// ([kF1, kF2, kF3, kF4, kF5,
                             kF6, kF7, kF8, kF9, kF10,
                             kF11, kF12, kEscape] `zip` repeat True))

    Cmd.addCommand "bind" bindF
    Cmd.addCommand "unbind" unbindF
    Cmd.addCommand "unbindall" unbindAllF
    Cmd.addCommand "bindlist" bindListF

  where setupKeyLines = do
          let kl = V.replicate 32 $ B.pack [93, 0] -- 93 is ']', 0 is NUL
          globals.keyLines .= kl
          globals.keyLinePos .= 1

bindF :: XCommandT
bindF = do
    c <- Cmd.argc

    if c < 2
      then Com.printf "bind <key> [command] : attach a command to a key\n"
      else do
        v <- Cmd.argv 1
        b <- stringToKeynum v

        if | b == -1 -> undefined -- TODO
           | c == 2 -> undefined -- TODO
           | otherwise -> undefined -- TODO

unbindF :: XCommandT
unbindF = undefined -- TODO

unbindAllF :: XCommandT
unbindAllF = undefined -- TODO

bindListF :: XCommandT
bindListF = undefined -- TODO

-- Returns a key number to be used to index keybindings[] by looking at
-- the given string. Single ascii characters return themselves, while
-- the K_* names are matched up.
stringToKeynum :: B.ByteString -> Quake Int
stringToKeynum str =
    if B.length str == 1
      then return $ ord $ BC.index str 0
      else do
        let upperStr = BC.map toUpper str
        keynames <- liftM (^.keyGlobals.keyNames) get

        case V.findIndex (== upperStr) keynames of
          Just i -> return i
          Nothing -> return (-1)
