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

multiplayerMenuRef :: MenuFrameworkSReference
multiplayerMenuRef = MenuFrameworkSReference 1

keysMenuRef :: MenuFrameworkSReference
keysMenuRef = MenuFrameworkSReference 2

optionsMenuRef :: MenuFrameworkSReference
optionsMenuRef = MenuFrameworkSReference 3

gameMenuRef :: MenuFrameworkSReference
gameMenuRef = MenuFrameworkSReference 4

saveGameMenuRef :: MenuFrameworkSReference
saveGameMenuRef = MenuFrameworkSReference 5

loadGameMenuRef :: MenuFrameworkSReference
loadGameMenuRef = MenuFrameworkSReference 6

joinServerMenuRef :: MenuFrameworkSReference
joinServerMenuRef = MenuFrameworkSReference 7

startServerMenuRef :: MenuFrameworkSReference
startServerMenuRef = MenuFrameworkSReference 8

dmOptionsMenuRef :: MenuFrameworkSReference
dmOptionsMenuRef = MenuFrameworkSReference 9

downloadOptionsMenuRef :: MenuFrameworkSReference
downloadOptionsMenuRef = MenuFrameworkSReference 10

addressBookMenuRef :: MenuFrameworkSReference
addressBookMenuRef = MenuFrameworkSReference 11

playerConfigMenuRef :: MenuFrameworkSReference
playerConfigMenuRef = MenuFrameworkSReference 12

tqSliderRef :: MenuSliderSReference
tqSliderRef = MenuSliderSReference 0

screenSizeSliderRef :: MenuSliderSReference
screenSizeSliderRef = MenuSliderSReference 1

brightnessSliderRef :: MenuSliderSReference
brightnessSliderRef = MenuSliderSReference 2

optionsSensitivitySliderRef :: MenuSliderSReference
optionsSensitivitySliderRef = MenuSliderSReference 3

optionsSfxVolumeSliderRef :: MenuSliderSReference
optionsSfxVolumeSliderRef = MenuSliderSReference 4

modeListRef :: MenuListSReference
modeListRef = MenuListSReference 0

refListRef :: MenuListSReference
refListRef = MenuListSReference 1

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

optionsFreeLookBoxRef :: MenuListSReference
optionsFreeLookBoxRef = MenuListSReference 7

optionsNoAltTabBoxRef :: MenuListSReference
optionsNoAltTabBoxRef = MenuListSReference 8

optionsAlwaysRunBoxRef :: MenuListSReference
optionsAlwaysRunBoxRef = MenuListSReference 9

optionsInvertMouseBoxRef :: MenuListSReference
optionsInvertMouseBoxRef = MenuListSReference 10

optionsLookSpringBoxRef :: MenuListSReference
optionsLookSpringBoxRef = MenuListSReference 11

optionsLookStrafeBoxRef :: MenuListSReference
optionsLookStrafeBoxRef = MenuListSReference 12

optionsCrosshairBoxRef :: MenuListSReference
optionsCrosshairBoxRef = MenuListSReference 13

optionsJoystickBoxRef :: MenuListSReference
optionsJoystickBoxRef = MenuListSReference 14

optionsCdVolumeBoxRef :: MenuListSReference
optionsCdVolumeBoxRef = MenuListSReference 15

optionsQualityListRef :: MenuListSReference
optionsQualityListRef = MenuListSReference 16

startMapListRef :: MenuListSReference
startMapListRef = MenuListSReference 17

rulesBoxRef :: MenuListSReference
rulesBoxRef = MenuListSReference 18

friendlyFireBoxRef :: MenuListSReference
friendlyFireBoxRef = MenuListSReference 19

fallsBoxRef :: MenuListSReference
fallsBoxRef = MenuListSReference 20

weaponsStayBoxRef :: MenuListSReference
weaponsStayBoxRef = MenuListSReference 21

instantPowerUpsBoxRef :: MenuListSReference
instantPowerUpsBoxRef = MenuListSReference 22

powerUpsBoxRef :: MenuListSReference
powerUpsBoxRef = MenuListSReference 23

healthBoxRef :: MenuListSReference
healthBoxRef = MenuListSReference 24

spawnFarthestBoxRef :: MenuListSReference
spawnFarthestBoxRef = MenuListSReference 25

teamPlayBoxRef :: MenuListSReference
teamPlayBoxRef = MenuListSReference 26

sameLevelBoxRef :: MenuListSReference
sameLevelBoxRef = MenuListSReference 27

forceRespawnBoxRef :: MenuListSReference
forceRespawnBoxRef = MenuListSReference 28

armorBoxRef :: MenuListSReference
armorBoxRef = MenuListSReference 29

allowExitBoxRef :: MenuListSReference
allowExitBoxRef = MenuListSReference 30

infiniteAmmoBoxRef :: MenuListSReference
infiniteAmmoBoxRef = MenuListSReference 31

fixedFovBoxRef :: MenuListSReference
fixedFovBoxRef = MenuListSReference 32

quadDropBoxRef :: MenuListSReference
quadDropBoxRef = MenuListSReference 33

noMinesBoxRef :: MenuListSReference
noMinesBoxRef = MenuListSReference 34

noNukesBoxRef :: MenuListSReference
noNukesBoxRef = MenuListSReference 35

stackDoubleBoxRef :: MenuListSReference
stackDoubleBoxRef = MenuListSReference 36

noSpheresBoxRef :: MenuListSReference
noSpheresBoxRef = MenuListSReference 37

allowDownloadBoxRef :: MenuListSReference
allowDownloadBoxRef = MenuListSReference 38

allowDownloadMapsBoxRef :: MenuListSReference
allowDownloadMapsBoxRef = MenuListSReference 39

allowDownloadModelsBoxRef :: MenuListSReference
allowDownloadModelsBoxRef = MenuListSReference 40

allowDownloadPlayersBoxRef :: MenuListSReference
allowDownloadPlayersBoxRef = MenuListSReference 41

allowDownloadSoundsBoxRef :: MenuListSReference
allowDownloadSoundsBoxRef = MenuListSReference 42

playerModelBoxRef :: MenuListSReference
playerModelBoxRef = MenuListSReference 43

playerSkinBoxRef :: MenuListSReference
playerSkinBoxRef = MenuListSReference 44

playerHandednessBoxRef :: MenuListSReference
playerHandednessBoxRef = MenuListSReference 45

playerRateBoxRef :: MenuListSReference
playerRateBoxRef = MenuListSReference 46

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

downloadTitleRef :: MenuSeparatorSReference
downloadTitleRef = MenuSeparatorSReference 2

playerSkinTitleRef :: MenuSeparatorSReference
playerSkinTitleRef = MenuSeparatorSReference 3

playerModelTitleRef :: MenuSeparatorSReference
playerModelTitleRef = MenuSeparatorSReference 4

playerHandTitleRef :: MenuSeparatorSReference
playerHandTitleRef = MenuSeparatorSReference 5

playerRateTitleRef :: MenuSeparatorSReference
playerRateTitleRef = MenuSeparatorSReference 6

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
              , _mgMenuListSItems      = V.fromList [ newMenuListS --  0 modeList
                                                    , newMenuListS --  1 refList
                                                    , newMenuListS --  2 fsBox
                                                    , newMenuListS --  3 stippleBox
                                                    , newMenuListS --  4 palettedTextureBox
                                                    , newMenuListS --  5 vSyncBox
                                                    , newMenuListS --  6 windowedMouse
                                                    , newMenuListS --  7 optionsFreeLookBox
                                                    , newMenuListS --  8 optionsNoAltTabBox
                                                    , newMenuListS --  9 optionsAlwaysRunBox
                                                    , newMenuListS -- 10 optionsInvertMouseBox
                                                    , newMenuListS -- 11 optionsLookSpringBox
                                                    , newMenuListS -- 12 optionsLookStrafeBox
                                                    , newMenuListS -- 13 optionsCrosshairBox
                                                    , newMenuListS -- 14 optionsJoystickBox
                                                    , newMenuListS -- 15 optionsCdVolumeBox
                                                    , newMenuListS -- 16 optionsQualityList
                                                    , newMenuListS -- 17 startMapList
                                                    , newMenuListS -- 18 rulesBox
                                                    , newMenuListS -- 19 friendlyFireBox
                                                    , newMenuListS -- 20 fallsBox
                                                    , newMenuListS -- 21 weaponsStayBox
                                                    , newMenuListS -- 22 instantPowerUpsBox
                                                    , newMenuListS -- 23 powerUpsBox
                                                    , newMenuListS -- 24 healthBox
                                                    , newMenuListS -- 25 spawnFarthestBox
                                                    , newMenuListS -- 26 teamPlayBox
                                                    , newMenuListS -- 27 sameLevelBox
                                                    , newMenuListS -- 28 forceRespawnBox
                                                    , newMenuListS -- 29 armorBox
                                                    , newMenuListS -- 30 allowExitBox
                                                    , newMenuListS -- 31 infiniteAmmoBox
                                                    , newMenuListS -- 32 fixedFovBox
                                                    , newMenuListS -- 33 quadDropBox
                                                    , newMenuListS -- 34 noMinesBox
                                                    , newMenuListS -- 35 noNukesBox
                                                    , newMenuListS -- 36 stackDoubleBox
                                                    , newMenuListS -- 37 noSpheresBox
                                                    , newMenuListS -- 38 allowDownloadBox
                                                    , newMenuListS -- 39 allowDownloadMapsBox
                                                    , newMenuListS -- 40 allowDownloadModelsBox
                                                    , newMenuListS -- 41 allowDownloadPlayersBox
                                                    , newMenuListS -- 42 allowDownloadSoundsBox
                                                    , newMenuListS -- 43 playerModelBox
                                                    , newMenuListS -- 44 playerSkinBox
                                                    , newMenuListS -- 45 playerHandednessBox
                                                    , newMenuListS -- 46 playerRateBox
                                                    ]
              , _mgMenuSliderSItems    = V.fromList [ newMenuSliderS -- tqSlider
                                                    , newMenuSliderS -- screenSizeSlider
                                                    , newMenuSliderS -- brightnessSlider
                                                    , newMenuSliderS -- optionsSensitivitySlider
                                                    , newMenuSliderS -- optionsSfxVolumeSlider
                                                    ]
              , _mgMenuActionSItems    = V.fromList ( menuActions
                                                      ++ replicate maxSaveGames newMenuActionS -- loadGameActions
                                                      ++ replicate maxSaveGames newMenuActionS -- saveGameActions
                                                      ++ replicate Constants.maxLocalServers newMenuActionS -- joinServerActions
                                                    )
              , _mgMenuSeparatorSItems = V.fromList [ newMenuSeparatorS -- blankLine
                                                    , newMenuSeparatorS -- joinServerServerTitle
                                                    , newMenuSeparatorS -- downloadTitle
                                                    , newMenuSeparatorS -- playerSkinTitle
                                                    , newMenuSeparatorS -- playerModelTitle
                                                    , newMenuSeparatorS -- playerHandTitle
                                                    , newMenuSeparatorS -- playerRateTitle
                                                    ]
              , _mgMenuFieldSItems     = V.empty
              , _mgLayers              = V.replicate maxMenuDepth newMenuLayerT
              , _mgDrawFunc            = Nothing
              , _mgKeyFunc             = Nothing
              , _mgEnterSound          = False
              , _mgMenuDepth           = 0
              , _mgMainCursor          = 0
              , _mgCached              = False
              , _mgGameCursor          = 0
              , _mgSaveStrings         = V.replicate maxSaveGames ""
              , _mgLocalServerNames    = V.replicate Constants.maxLocalServers ""
              }
