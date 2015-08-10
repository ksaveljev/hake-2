{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Key where

import Control.Lens ((.=), (%=), use, ix, (+=), (^.))
import Control.Monad.State (liftM, unless, when, void)
import Data.Char (ord, toUpper, chr)
import Data.Maybe (isJust, fromJust, isNothing)
import System.IO (Handle)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import Client.KeyConstants
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Client.Console as Console
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com

init :: Quake ()
init = do
    let kl = V.replicate 32 $ B.pack [93, 0] -- 93 is ']', 0 is NUL
    globals.keyLines .= kl
    globals.keyLinePos .= 1

    keyGlobals.kgConsoleKeys %= (UV.// (([32..127] `zip` repeat True) ++ 
            ([kEnter, kKpEnter, kTab, kLeftArrow, kKpLeftArrow,
             kRightArrow, kKpRightArrow, kUpArrow, kKpUpArrow,
             kDownArrow, kKpDownArrow, kBackspace, kHome, kKpHome,
             kEnd, kKpEnd, kPgUp, kKpPgUp, kPgDn, kKpPgDn, kShift,
             kIns, kKpIns, kKpDel, kKpSlash, kKpPlus, kKpMinus, 
             kKp5] `zip` repeat True) ++ [(96, False), (126, False)])) -- 96 is '`', 126 is '~'

    keyGlobals.kgMenuBound %= (UV.// ([kF1, kF2, kF3, kF4, kF5,
                             kF6, kF7, kF8, kF9, kF10,
                             kF11, kF12, kEscape] `zip` repeat True))

    Cmd.addCommand "bind" (Just bindF)
    Cmd.addCommand "unbind" (Just unbindF)
    Cmd.addCommand "unbindall" (Just unbindAllF)
    Cmd.addCommand "bindlist" (Just bindListF)

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
        keynames <- use $ keyGlobals.kgKeyNames

        case V.findIndex (== Just upperStr) keynames of
          Just i -> return i
          Nothing -> return (-1)

-- Returns a string (either a single ascii char, or a K_* name) for the 
-- given keynum.
keynumToString :: Int -> Quake B.ByteString
keynumToString keynum = do
    keynames <- use $ keyGlobals.kgKeyNames

    if | keynum < 0 || keynum > 255 -> return "<KEY NOT FOUND>"
       | keynum > 32 && keynum < 127 -> return $ BC.pack [chr keynum]
       | isJust (keynames V.! keynum) -> return $ fromJust $ keynames V.! keynum
       | otherwise -> return "<UNKNOWN KEYNUM>"

setBinding :: Int -> Maybe B.ByteString -> Quake ()
setBinding keyNum binding =
    unless (keyNum == -1) $ globals.keyBindings %= (V.// [(keyNum, binding)])

writeBindings :: Handle -> Quake ()
writeBindings h = do
    kb <- use $ globals.keyBindings
    void $ V.sequence $ V.imap (writeKeyBinding h) kb

  where writeKeyBinding :: Handle -> Int -> Maybe B.ByteString -> Quake ()
        writeKeyBinding _ _ Nothing = return ()
        writeKeyBinding handle i (Just b) = do
          keyStr <- keynumToString i
          io $ B.hPut handle $ "bind " `B.append` keyStr `B.append` " \"" `B.append` b `B.append` "\"\n"

-- Called by the system between frames for both key up and key down events.
event :: Int -> Bool -> Int -> Quake ()
event key down time = do
    -- TODO: do we need this?
    -- // hack for modal presses
    -- if (key_waiting == -1) {
    --     if (down)
    --         key_waiting = key;
    --     return;
    -- }
    
    cls' <- use $ globals.cls

    -- update auto-repeat status
    done <- if down
              then do
                keyGlobals.kgKeyRepeats.ix key += 1

                keyRepeats <- use $ keyGlobals.kgKeyRepeats
                keyBindings' <- use $ globals.keyBindings

                if | keyRepeats UV.! key > 1 && (cls'^.csKeyDest) == Constants.keyGame && not ((cls'^.csState) == Constants.caDisconnected) -> -- ignore most autorepeats
                       return True
                   | key >= 200 && isNothing (keyBindings' V.! key) -> do
                       v <- keynumToString key
                       Com.printf (v `B.append` " is unbound, hit F4 to set.\n")
                       return False
                   | otherwise ->
                       return False

              else do
                keyGlobals.kgKeyRepeats.ix key .= 0
                return False

    unless done $ do
      when (key == kShift) $
        keyGlobals.kgShiftDown .= down

      -- TODO: '~' is not working for console so far? need to set it up?
      -- console key is hardcoded, so the user can never unbind it
      done' <- if key == ord '`' || key == ord '~'
                 then do
                   if not down
                     then return True
                     else do
                       Console.toggleConsoleF
                       return True
                 else
                   return False

      unless done' $ do
        -- any key during the attract mode will bring up the menu
        cl' <- use $ globals.cl

        let key' = if (cl'^.csAttractLoop) && (cls'^.csKeyDest) /= Constants.keyMenu && not (key >= kF1 && key <= kF12)
                     then kEscape
                     else key

        -- menu key is hardcoded, so the user can never unbind it
        if key' == kEscape
          then do
            undefined -- TODO
          else do
            io (print "Key.event") >> undefined -- TODO
