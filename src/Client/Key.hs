{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Key where

import Control.Lens ((.=), (%=), use, ix, (+=), (^.), (-=), preuse)
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
import {-# SOURCE #-} qualified Client.Menu as Menu
import {-# SOURCE #-} qualified Client.Console as Console
import qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
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
bindF =
  XCommandT "Key.bindF" (do
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
  )

unbindF :: XCommandT
unbindF =
  XCommandT "Key.unbindF" (do
    c <- Cmd.argc

    if c /= 2
      then Com.printf "unbind <key> : remove commands from a key\n"
      else do
        v <- Cmd.argv 1
        b <- stringToKeynum v

        if b == -1
          then Com.printf $ "\"" `B.append` v `B.append` "\" isn't a valid key\n"
          else setBinding b Nothing
  )

unbindAllF :: XCommandT
unbindAllF = XCommandT "Key.unbindAllF" (globals.keyBindings .= V.replicate 256 Nothing)

bindListF :: XCommandT
bindListF =
  XCommandT "Key.bindListF" (do
    bindings <- use $ globals.keyBindings
    void $ V.sequence $ V.imap printBinding bindings
  )

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
                       (Console.toggleConsoleF)^.xcCmd
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
            when down $ do
              if (cl'^.csFrame.fPlayerState.psStats) UV.! Constants.statLayouts /= 0 && (cls'^.csKeyDest) == Constants.keyGame
                then
                  CBuf.addText "cmd putaway\n"
                else
                  if | (cls'^.csKeyDest) == Constants.keyMessage ->
                         message key'

                     | (cls'^.csKeyDest) == Constants.keyMenu ->
                         Menu.menuKeyDown key'

                     | (cls'^.csKeyDest) == Constants.keyGame || (cls'^.csKeyDest) == Constants.keyConsole ->
                         (Menu.menuMainF)^.xcCmd

                     | otherwise ->
                         Com.comError Constants.errFatal "Bad cls.key_dest"
          else do
            -- track if any key is down for BUTTON_ANY
            globals.keyDown.ix key' .= down

            if down
              then do
                keyRepeats <- use $ keyGlobals.kgKeyRepeats
                when (keyRepeats UV.! key' == 1) $
                  keyGlobals.kgAnyKeyDown += 1
              else do
                anyKeyDown <- use $ keyGlobals.kgAnyKeyDown
                when (anyKeyDown > 0) $
                  keyGlobals.kgAnyKeyDown -= 1

            -- key up events only generate commands if the game key binding is
            -- a button command (leading + sign).  These will occur even in console mode,
            -- to keep the character from continuing an action started before a console
            -- switch.  Button commands include the kenum as a parameter, so multiple
            -- downs can be matched with ups
            if not down
              then do
                Just kb <- preuse $ globals.keyBindings.ix key'
                case kb of
                  Nothing -> return ()
                  Just binding ->
                    when (B.length binding > 0 && binding `BC.index` 0 == '+') $
                      CBuf.addText ("-" `B.append` (B.drop 1 binding) `B.append` " " `B.append` BC.pack (show key') `B.append` " " `B.append` BC.pack (show time) `B.append` "\n") -- IMPROVE?
              else do
                -- if not a consolekey, send to the interpreter no matter what mode is
                Just menuBound <- preuse $ keyGlobals.kgMenuBound.ix key'
                Just consoleKey <- preuse $ keyGlobals.kgConsoleKeys.ix key'

                if ((cls'^.csKeyDest) == Constants.keyMenu && menuBound) ||
                   ((cls'^.csKeyDest) == Constants.keyConsole && not consoleKey) ||
                   ((cls'^.csKeyDest) == Constants.keyGame && ((cls'^.csState) == Constants.caActive || not consoleKey))
                   then do
                     Just kb <- preuse $ globals.keyBindings.ix key'
                     case kb of
                       Nothing -> return ()
                       Just binding ->
                         if B.length binding > 0 && binding `BC.index` 0 == '+'
                           then CBuf.addText (binding `B.append` " " `B.append` BC.pack (show key') `B.append` " " `B.append` BC.pack (show time) `B.append` "\n") -- IMPROVE?
                           else CBuf.addText (binding `B.append` "\n")
                   else
                     -- other systems only care about key down events
                     when down $ do
                       if | (cls'^.csKeyDest) == Constants.keyMessage ->
                              message key'

                          | (cls'^.csKeyDest) == Constants.keyMenu ->
                              Menu.menuKeyDown key'

                          | (cls'^.csKeyDest) == Constants.keyGame || (cls'^.csKeyDest) == Constants.keyConsole ->
                              console key'

                          | otherwise ->
                              Com.comError Constants.errFatal "Bad cls.key_dest"

message :: Int -> Quake ()
message key = do
    io (putStrLn "Key.message") >> undefined -- TODO

console :: Int -> Quake ()
console key = do
    io (putStrLn "Key.console") >> undefined -- TODO

clearStates :: Quake ()
clearStates = do
    keyGlobals.kgAnyKeyDown .= 0
    keyDownEvents 0 256

  where keyDownEvents :: Int -> Int -> Quake ()
        keyDownEvents idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              keyDown' <- use $ globals.keyDown
              keyRepeats <- use $ keyGlobals.kgKeyRepeats

              when ((keyDown' UV.! idx) || (keyRepeats UV.! idx) /= 0) $
                event idx False 0

              globals.keyDown.ix idx .= False
              keyGlobals.kgKeyRepeats.ix idx .= 0

              keyDownEvents (idx + 1) maxIdx
