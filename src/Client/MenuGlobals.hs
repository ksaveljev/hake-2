{-# LANGUAGE TemplateHaskell #-}
module Client.MenuGlobals ( module Client.MenuGlobals
                          , module Client.MenuFrameworkS
                          , module Client.MenuLayerT
                          ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.MenuFrameworkS
import Client.MenuLayerT

makeLenses ''MenuGlobals

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

openGLMenuRef :: MenuFrameworkSReference
openGLMenuRef = MenuFrameworkSReference 0

modeListRef :: MenuListSReference
modeListRef = MenuListSReference 0

refListRef :: MenuListSReference
refListRef = MenuListSReference 1

tqSliderRef :: MenuSliderSReference
tqSliderRef = MenuSliderSReference 0

screenSizeSliderRef :: MenuSliderSReference
screenSizeSliderRef = MenuSliderSReference 1

brightnessSliderRef :: MenuSliderSReference
brightnessSliderRef = MenuSliderSReference 2

fsBoxRef :: MenuListSReference
fsBoxRef = MenuListSReference 2

stippleBoxRef :: MenuListSReference
stippleBoxRef = MenuListSReference 3

palettedTextureBoxRef :: MenuListSReference
palettedTextureBoxRef = MenuListSReference 4

vSyncBoxRef :: MenuListSReference
vSyncBoxRef = MenuListSReference 5

windowedMouseRef :: MenuListSReference
windowedMouseRef = MenuListSReference 6

applyActionRef :: MenuActionSReference
applyActionRef = MenuActionSReference 0

defaultsActionRef :: MenuActionSReference
defaultsActionRef = MenuActionSReference 1

initialMenuGlobals :: MenuGlobals
initialMenuGlobals =
  MenuGlobals { _mgMenuFrameworks   = V.replicate 13 newMenuFrameworkS
              , _mgMenuListSItems   = V.fromList [ newMenuListS -- modeList
                                                 , newMenuListS -- refList
                                                 , newMenuListS -- fsBox
                                                 , newMenuListS -- stippleBox
                                                 , newMenuListS -- palettedTextureBox
                                                 , newMenuListS -- vSyncBox
                                                 , newMenuListS -- windowedMouse
                                                 ]
              , _mgMenuSliderSItems = V.fromList [ newMenuSliderS -- tqSlider
                                                 , newMenuSliderS -- screenSizeSlider
                                                 , newMenuSliderS -- brightnessSlider
                                                 ]
              , _mgMenuActionSItems = V.fromList [ newMenuActionS -- applyAction
                                                 , newMenuActionS -- defaultsAction
                                                 ]
              , _mgLayers           = V.empty
              , _mgDrawFunc         = Nothing
              , _mgEnterSound       = False
              , _mgMenuDepth        = 0
              }
