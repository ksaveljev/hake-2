{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.KeyGlobals ( KeyGlobals
                         , initialKeyGlobals
                         , kgAnyKeyDown
                         , kgConsoleKeys
                         , kgHistoryLine
                         , kgKeyNames
                         , kgKeyRepeats
                         , kgKeyWaiting
                         , kgMenuBound
                         , kgShiftDown
                         ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Client.KeyConstants

makeLenses ''KeyGlobals

setupKeyNames :: [(Int, Maybe B.ByteString)]
setupKeyNames = [ (kTab,          Just "TAB")
                , (kEnter,        Just "ENTER")
                , (kEscape,       Just "ESCAPE")
                , (kSpace,        Just "SPACE")
                , (kBackspace,    Just "BACKSPACE")
                , (kUpArrow,      Just "UPARROW")
                , (kDownArrow,    Just "DOWNARROW")
                , (kLeftArrow,    Just "LEFTARROW")
                , (kRightArrow,   Just "RIGHTARROW")
                , (kAlt,          Just "ALT")
                , (kCtrl,         Just "CTRL")
                , (kShift,        Just "SHIFT")
                , (kF1,           Just "F1")
                , (kF2,           Just "F2")
                , (kF3,           Just "F3")
                , (kF4,           Just "F4")
                , (kF5,           Just "F5")
                , (kF6,           Just "F6")
                , (kF7,           Just "F7")
                , (kF8,           Just "F8")
                , (kF9,           Just "F9")
                , (kF10,          Just "F10")
                , (kF11,          Just "F11")
                , (kF12,          Just "F12")
                , (kIns,          Just "INS")
                , (kDel,          Just "DEL")
                , (kPgDn,         Just "PGDN")
                , (kPgUp,         Just "PGUP")
                , (kHome,         Just "HOME")
                , (kEnd,          Just "END")
                , (kMouse1,       Just "MOUSE1")
                , (kMouse2,       Just "MOUSE2")
                , (kMouse3,       Just "MOUSE3")
                , (kKpHome,       Just "KP_HOME")
                , (kKpUpArrow,    Just "KP_UPARROW")
                , (kKpPgUp,       Just "KP_PGUP")
                , (kKpLeftArrow,  Just "KP_LEFTARROW")
                , (kKp5,          Just "KP_5")
                , (kKpRightArrow, Just "KP_RIGHTARROW")
                , (kKpEnd,        Just "KP_END")
                , (kKpDownArrow,  Just "KP_DOWNARROW")
                , (kKpPgDn,       Just "KP_PGDN")
                , (kKpEnter,      Just "KP_ENTER")
                , (kKpIns,        Just "KP_INS")
                , (kKpDel,        Just "KP_DEL")
                , (kKpSlash,      Just "KP_SLASH")
                , (kKpPlus,       Just "KP_PLUS")
                , (kKpMinus,      Just "KP_MINUS")
                , (kMWheelUp,     Just "MWHEELUP")
                , (kMWheelDown,   Just "MWHEELDOWN")
                , (kPause,        Just "PAUSE")
                , (59,            Just "SEMICOLON") -- because a raw semicolon seperates commands
                , (0,             Just "NULL")
                ]

initialKeyGlobals :: KeyGlobals
initialKeyGlobals =
  KeyGlobals { _kgAnyKeyDown  = 0
             , _kgKeyWaiting  = 0
             , _kgHistoryLine = 0
             , _kgShiftDown   = False
             , _kgKeyRepeats  = UV.replicate 256 0
             , _kgMenuBound   = UV.replicate 256 False
             , _kgConsoleKeys = UV.replicate 256 False
             , _kgKeyNames    = V.replicate 256 Nothing V.// setupKeyNames
             }
