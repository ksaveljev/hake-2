{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.MenuGlobals ( module Client.MenuGlobals
                          , module Client.MenuFrameworkS
                          , module Client.MenuLayerT
                          ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.MenuFrameworkS
import Client.MenuLayerT
import qualified Constants

makeLenses ''MenuGlobals

maxSaveGames :: Int
maxSaveGames = 15

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

maxMenuDepth :: Int
maxMenuDepth = 8

openGLMenuRef :: MenuFrameworkSReference
openGLMenuRef = MenuFrameworkSReference 0

gameMenuRef :: MenuFrameworkSReference
gameMenuRef = MenuFrameworkSReference 4

saveGameMenuRef :: MenuFrameworkSReference
saveGameMenuRef = MenuFrameworkSReference 5

loadGameMenuRef :: MenuFrameworkSReference
loadGameMenuRef = MenuFrameworkSReference 6

joinServerMenuRef :: MenuFrameworkSReference
joinServerMenuRef = MenuFrameworkSReference 7

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

easyGameActionRef :: MenuActionSReference
easyGameActionRef = MenuActionSReference 2

mediumGameActionRef :: MenuActionSReference
mediumGameActionRef = MenuActionSReference 3

hardGameActionRef :: MenuActionSReference
hardGameActionRef = MenuActionSReference 4

loadGameActionRef :: MenuActionSReference
loadGameActionRef = MenuActionSReference 5

saveGameActionRef :: MenuActionSReference
saveGameActionRef = MenuActionSReference 6

creditsActionRef :: MenuActionSReference
creditsActionRef = MenuActionSReference 7

joinNetworkServerActionRef :: MenuActionSReference
joinNetworkServerActionRef = MenuActionSReference 8

startNetworkServerActionRef :: MenuActionSReference
startNetworkServerActionRef = MenuActionSReference 9

playerSetupActionRef :: MenuActionSReference
playerSetupActionRef = MenuActionSReference 10

keysAttackActionRef :: MenuActionSReference
keysAttackActionRef = MenuActionSReference 11

keysChangeWeaponActionRef :: MenuActionSReference
keysChangeWeaponActionRef = MenuActionSReference 12

keysWalkForwardActionRef :: MenuActionSReference
keysWalkForwardActionRef = MenuActionSReference 13

keysBackpedalActionRef :: MenuActionSReference
keysBackpedalActionRef = MenuActionSReference 14

keysTurnLeftActionRef :: MenuActionSReference
keysTurnLeftActionRef = MenuActionSReference 15

keysTurnRightActionRef :: MenuActionSReference
keysTurnRightActionRef = MenuActionSReference 16

keysRunActionRef :: MenuActionSReference
keysRunActionRef = MenuActionSReference 17

keysStepLeftActionRef :: MenuActionSReference
keysStepLeftActionRef = MenuActionSReference 18

keysStepRightActionRef :: MenuActionSReference
keysStepRightActionRef = MenuActionSReference 19

keysSidestepActionRef :: MenuActionSReference
keysSidestepActionRef = MenuActionSReference 20

keysLookUpActionRef :: MenuActionSReference
keysLookUpActionRef = MenuActionSReference 21

keysLookDownActionRef :: MenuActionSReference
keysLookDownActionRef = MenuActionSReference 22

keysCenterViewActionRef :: MenuActionSReference
keysCenterViewActionRef = MenuActionSReference 23

keysMouseLookActionRef :: MenuActionSReference
keysMouseLookActionRef = MenuActionSReference 24

keysKeyboardLookActionRef :: MenuActionSReference
keysKeyboardLookActionRef = MenuActionSReference 25

keysMoveUpActionRef :: MenuActionSReference
keysMoveUpActionRef = MenuActionSReference 26

keysMoveDownActionRef :: MenuActionSReference
keysMoveDownActionRef = MenuActionSReference 27

keysInventoryActionRef :: MenuActionSReference
keysInventoryActionRef = MenuActionSReference 28

keysInvUseActionRef :: MenuActionSReference
keysInvUseActionRef = MenuActionSReference 29

keysInvDropActionRef :: MenuActionSReference
keysInvDropActionRef = MenuActionSReference 30

keysInvPrevActionRef :: MenuActionSReference
keysInvPrevActionRef = MenuActionSReference 31

keysInvNextActionRef :: MenuActionSReference
keysInvNextActionRef = MenuActionSReference 32

keysHelpComputerActionRef :: MenuActionSReference
keysHelpComputerActionRef = MenuActionSReference 33

optionsDefaultsActionRef :: MenuActionSReference
optionsDefaultsActionRef = MenuActionSReference 34

optionsCustomizeOptionsActionRef :: MenuActionSReference
optionsCustomizeOptionsActionRef = MenuActionSReference 35

optionsConsoleActionRef :: MenuActionSReference
optionsConsoleActionRef = MenuActionSReference 36

joinServerSearchActionRef :: MenuActionSReference
joinServerSearchActionRef = MenuActionSReference 37

joinServerAddressBookActionRef :: MenuActionSReference
joinServerAddressBookActionRef = MenuActionSReference 38

startServerStartActionRef :: MenuActionSReference
startServerStartActionRef = MenuActionSReference 39

startServerDMOptionsActionRef :: MenuActionSReference
startServerDMOptionsActionRef = MenuActionSReference 40

playerDownloadActionRef :: MenuActionSReference
playerDownloadActionRef = MenuActionSReference 41

blankLineRef :: MenuSeparatorSReference
blankLineRef = MenuSeparatorSReference 0

joinServerServerTitleRef :: MenuSeparatorSReference
joinServerServerTitleRef = MenuSeparatorSReference 1

menuActions :: [MenuActionS]
menuActions = [ newMenuActionS --  0 applyAction
              , newMenuActionS --  1 defaultsAction
              , newMenuActionS --  2 easyGameAction
              , newMenuActionS --  3 mediumGameAction
              , newMenuActionS --  4 hardGameAction
              , newMenuActionS --  5 loadGameAction
              , newMenuActionS --  6 saveGameAction
              , newMenuActionS --  7 creditsAction
              , newMenuActionS --  8 joinNetworkServerAction
              , newMenuActionS --  9 startNetworkServerAction
              , newMenuActionS -- 10 playerSetupAction
              , newMenuActionS -- 11 keysAttackAction
              , newMenuActionS -- 12 keysChangeWeaponAction
              , newMenuActionS -- 13 keysWalkForwardAction
              , newMenuActionS -- 14 keysBackpedalAction
              , newMenuActionS -- 15 keysTurnLeftAction
              , newMenuActionS -- 16 keysTurnRightAction
              , newMenuActionS -- 17 keysRunAction
              , newMenuActionS -- 18 keysStepLeftAction
              , newMenuActionS -- 19 keysStepRightAction
              , newMenuActionS -- 20 keysSidestepAction
              , newMenuActionS -- 21 keysLookUpAction
              , newMenuActionS -- 22 keysLookDownAction
              , newMenuActionS -- 23 keysCenterViewAction
              , newMenuActionS -- 24 keysMouseLookAction
              , newMenuActionS -- 25 keysKeyboardLookAction
              , newMenuActionS -- 26 keysMoveUpAction
              , newMenuActionS -- 27 keysMoveDownAction
              , newMenuActionS -- 28 keysInventoryAction
              , newMenuActionS -- 29 keysInvUseAction
              , newMenuActionS -- 30 keysInvDropAction
              , newMenuActionS -- 31 keysInvPrevAction
              , newMenuActionS -- 32 keysInvNextAction
              , newMenuActionS -- 33 keysHelpComputerAction
              , newMenuActionS -- 34 optionsDefaultsAction
              , newMenuActionS -- 35 optionsCustomizeOptionsAction
              , newMenuActionS -- 36 optionsConsoleAction
              , newMenuActionS -- 37 joinServerSearchAction
              , newMenuActionS -- 38 joinServerAddressBookAction
              , newMenuActionS -- 39 startServerStartAction
              , newMenuActionS -- 40 startServerDMOptionsAction
              , newMenuActionS -- 41 playerDownloadAction
              ]

loadGameActions :: V.Vector MenuActionSReference
loadGameActions = V.generate maxSaveGames (\idx -> MenuActionSReference (idx + loadGameActionsOffset))

saveGameActions :: V.Vector MenuActionSReference
saveGameActions = V.generate maxSaveGames (\idx -> MenuActionSReference (idx + saveGameActionsOffset))

joinServerActions :: V.Vector MenuActionSReference
joinServerActions = V.generate Constants.maxLocalServers (\idx -> MenuActionSReference (idx + joinServerActionsOffset))

loadGameActionsOffset :: Int
loadGameActionsOffset = length menuActions

saveGameActionsOffset :: Int
saveGameActionsOffset = loadGameActionsOffset + maxSaveGames

joinServerActionsOffset :: Int
joinServerActionsOffset = saveGameActionsOffset + maxSaveGames

initialMenuGlobals :: MenuGlobals
initialMenuGlobals =
  MenuGlobals { _mgMenuFrameworks      = V.replicate 13 newMenuFrameworkS
              , _mgMenuListSItems      = V.fromList [ newMenuListS -- modeList
                                                    , newMenuListS -- refList
                                                    , newMenuListS -- fsBox
                                                    , newMenuListS -- stippleBox
                                                    , newMenuListS -- palettedTextureBox
                                                    , newMenuListS -- vSyncBox
                                                    , newMenuListS -- windowedMouse
                                                    ]
              , _mgMenuSliderSItems    = V.fromList [ newMenuSliderS -- tqSlider
                                                    , newMenuSliderS -- screenSizeSlider
                                                    , newMenuSliderS -- brightnessSlider
                                                    ]
              , _mgMenuActionSItems    = V.fromList ( menuActions
                                                      ++ replicate maxSaveGames newMenuActionS -- loadGameActions
                                                      ++ replicate maxSaveGames newMenuActionS -- saveGameActions
                                                      ++ replicate Constants.maxLocalServers newMenuActionS -- joinServerActions
                                                    )
              , _mgMenuSeparatorSItems = V.fromList [ newMenuSeparatorS -- blankLine
                                                    , newMenuSeparatorS -- joinServerServerTitle
                                                    ]
              , _mgLayers              = V.replicate maxMenuDepth newMenuLayerT
              , _mgDrawFunc            = Nothing
              , _mgKeyFunc             = Nothing
              , _mgEnterSound          = False
              , _mgMenuDepth           = 0
              , _mgMainCursor          = 0
              , _mgCached              = False
              , _mgGameCursor          = 0
              , _mgLocalServerNames    = V.replicate Constants.maxLocalServers ""
              }
