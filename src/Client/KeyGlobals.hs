{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.KeyGlobals ( KeyGlobals
                         , initialKeyGlobals
                         , anyKeyDown
                         , consoleKeys
                         , historyLine
                         , keyNames
                         , keyRepeats
                         , keyWaiting
                         , menuBound
                         , shiftDown
                         ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Client.KeyConstants

makeLenses ''KeyGlobals

setupKeyNames :: [(Int, B.ByteString)]
setupKeyNames = [ (kTab,        "TAB")
                , (kEnter,      "ENTER")
                , (kEscape,     "ESCAPE")
                , (kSpace,      "SPACE")
                , (kBackspace,  "BACKSPACE")
                , (kUpArrow,    "UPARROW")
                , (kDownArrow,  "DOWNARROW")
                , (kLeftArrow,  "LEFTARROW")
                , (kRightArrow, "RIGHTARROW")
                , (kAlt,        "ALT")
                , (kCtrl,       "CTRL")
                , (kShift,      "SHIFT")
                , (kF1,         "F1")
                , (kF2,         "F2")
                , (kF3,         "F3")
                , (kF4,         "F4")
                , (kF5,         "F5")
                , (kF6,         "F6")
                , (kF7,         "F7")
                , (kF8,         "F8")
                , (kF9,         "F9")
                , (kF10,        "F10")
                , (kF11,        "F11")
                , (kF12,        "F12")
                , (kIns,        "INS")
                , (kDel,        "DEL")
                , (kPgDn,       "PGDN")
                , (kPgUp,       "PGUP")
                , (kHome,       "HOME")
                , (kEnd,        "END")
                , (kMouse1,     "MOUSE1")
                , (kMouse2,     "MOUSE2")
                , (kMouse3,     "MOUSE3")
                , (kKpHome,     "KP_HOME")
                , (kKpUpArrow,  "KP_UPARROW")
                , (kKpPgUp,     "KP_PGUP")
                , (kKpLeftArrow, "KP_LEFTARROW")
                , (kKp5,         "KP_5")
                , (kKpRightArrow, "KP_RIGHTARROW")
                , (kKpEnd,        "KP_END")
                , (kKpDownArrow,  "KP_DOWNARROW")
                , (kKpPgDn,       "KP_PGDN")
                , (kKpEnter,      "KP_ENTER")
                , (kKpIns,        "KP_INS")
                , (kKpDel,        "KP_DEL")
                , (kKpSlash,      "KP_SLASH")
                , (kKpPlus,       "KP_PLUS")
                , (kKpMinus,      "KP_MINUS")
                , (kMWheelUp,     "MWHEELUP")
                , (kMWheelDown,   "MWHEELDOWN")
                , (kPause,        "PAUSE")
                , (59,            "SEMICOLON") -- because a raw semicolon seperates commands
                , (0,             "NULL")
                ]

initialKeyGlobals :: KeyGlobals
initialKeyGlobals =
  KeyGlobals { _anyKeyDown  = 0
             , _keyWaiting  = 0
             , _historyLine = 0
             , _shiftDown   = False
             , _keyRepeats  = UV.replicate 256 0
             , _menuBound   = UV.replicate 256 False
             , _consoleKeys = UV.replicate 256 False
             , _keyNames    = V.replicate 256 "" V.// setupKeyNames
             }
