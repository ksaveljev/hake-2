{-# LANGUAGE TemplateHaskell #-}
module Client.MenuGlobals ( module Client.MenuGlobals
                          , module Client.MenuFrameworkS
                          ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.MenuFrameworkS

makeLenses ''MenuGlobals

initialMenuGlobals :: MenuGlobals
initialMenuGlobals =
  MenuGlobals { _mgMenuFrameworks = V.replicate 13 newMenuFrameworkS
              , _mgMenuItems = V.fromList [ newMenuListS    -- modeList
                                          , newMenuListS    -- refList
                                          , newMenuSliderS  -- tqSlider
                                          , newMenuSliderS  -- screenSizeSlider
                                          , newMenuSliderS  -- brightnessSlider
                                          , newMenuListS    -- fsBox
                                          , newMenuListS    -- stippleBox
                                          , newMenuListS    -- palettedTextureBox
                                          , newMenuListS    -- vSyncBox
                                          , newMenuListS    -- windowedMouse
                                          , newMenuActionS  -- applyAction
                                          , newMenuActionS  -- defaultsAction
                                          ]
              }

{-
  _mgMenuFrameworks:
  0 - OpenGLMenu
  1 - MultiPlayerMenu
  2 - KeysMenu
  3 - OptionsMenu
  4 - GameMenu
  5 - SaveGameMenu
  6 - LoadGameMenu
  7 - JoinServerMenu
  8 - StartServerMenu
  9 - DMOptionsMenu
  10 - DownloadOptionsMenu
  11 - AddressBookMenu
  12 - PlayerConfigMenu
-}
