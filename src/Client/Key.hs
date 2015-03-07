{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Key where

import Data.Maybe (isJust, fromJust)
import Data.Char (ord, toUpper, chr)
import Control.Lens ((.=), (%=), use)
import Control.Monad.State (liftM, unless, when, void)
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
    let kl = V.replicate 32 $ B.pack [93, 0] -- 93 is ']', 0 is NUL
    globals.keyLines .= kl
    globals.keyLinePos .= 1

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

bindF :: XCommandT
bindF = do
    c <- Cmd.argc

    if c < 2
      then Com.printf "bind <key> [command] : attach a command to a key\n"
      else do
        v <- Cmd.argv 1
        b <- stringToKeynum v

        if | b == -1 -> Com.printf $ "\"" `B.append` v `B.append` "\" isn't a valid key\n"
           | c == 2 -> do
               keybindings <- use $ globals.keyBindings
               case keybindings V.! b of
                 Just binding -> Com.printf $ "\"" `B.append` v `B.append` "\" = \"" `B.append` binding `B.append` "\"\n"
                 Nothing -> Com.printf $ "\"" `B.append` v `B.append` "\" is not bound\n"
           | otherwise -> do
               cmd <- liftM (B.intercalate " ") $ mapM Cmd.argv [2..c-1]
               setBinding b (Just cmd)

unbindF :: XCommandT
unbindF = do
    c <- Cmd.argc

    if c /= 2
      then Com.printf "unbind <key> : remove commands from a key\n"
      else do
        v <- Cmd.argv 1
        b <- stringToKeynum v

        if b == -1
          then Com.printf $ "\"" `B.append` v `B.append` "\" isn't a valid key\n"
          else setBinding b Nothing

unbindAllF :: XCommandT
unbindAllF = globals.keyBindings .= V.replicate 256 Nothing

bindListF :: XCommandT
bindListF = do
    bindings <- use $ globals.keyBindings
    void $ V.sequence $ V.imap printBinding bindings

  where printBinding _ Nothing = return ()
        printBinding i (Just b) =
          when (B.length b /= 0) $ do
                                    strKeynum <- keynumToString i
                                    Com.printf $ strKeynum
                                      `B.append` " \""
                                      `B.append` b
                                      `B.append` "\"\n"


-- Returns a key number to be used to index keybindings[] by looking at
-- the given string. Single ascii characters return themselves, while
-- the K_* names are matched up.
stringToKeynum :: B.ByteString -> Quake Int
stringToKeynum str =
    if B.length str == 1
      then return $ ord $ BC.index str 0
      else do
        let upperStr = BC.map toUpper str
        keynames <- use $ keyGlobals.keyNames

        case V.findIndex (== Just upperStr) keynames of
          Just i -> return i
          Nothing -> return (-1)

-- Returns a string (either a single ascii char, or a K_* name) for the 
-- given keynum.
keynumToString :: Int -> Quake B.ByteString
keynumToString keynum = do
    keynames <- use $ keyGlobals.keyNames

    if | keynum < 0 || keynum > 255 -> return "<KEY NOT FOUND>"
       | keynum > 32 && keynum < 127 -> return $ BC.pack [chr keynum]
       | isJust (keynames V.! keynum) -> return $ fromJust $ keynames V.! keynum
       | otherwise -> return "<UNKNOWN KEYNUM>"

setBinding :: Int -> Maybe B.ByteString -> Quake ()
setBinding keyNum binding =
    unless (keyNum == -1) $ globals.keyBindings %= (V.// [(keyNum, binding)])
