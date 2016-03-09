{-# LANGUAGE TemplateHaskell #-}
module Client.MenuGlobals
  ( module Client.MenuGlobals
  ) where

import           Client.MenuActionS (newMenuActionS)
import           Client.MenuFieldS (newMenuFieldS)
import           Client.MenuFrameworkS (newMenuFrameworkS)
import           Client.MenuLayerT (newMenuLayerT)
import           Client.MenuListS (newMenuListS)
import           Client.MenuSeparatorS (newMenuSeparatorS)
import           Client.MenuSliderS (newMenuSliderS)
import           QCommon.NetAdrT (newNetAdrT)
import           Types
import qualified Constants

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V

makeLenses ''MenuGlobals

idCredits :: V.Vector B.ByteString
idCredits = V.fromList [ "+QUAKE II BY ID SOFTWARE", ""
                       , "+PROGRAMMING", "John Carmack", "John Cash", "Brian Hook", ""
                       , "+JAVA PORT BY BYTONIC", "Carsten Weisse", "Holger Zickner", "Rene Stoeckel", "", "+ART"
                       , "Adrian Carmack", "Kevin Cloud", "Paul Steed", "", "+LEVEL DESIGN"
                       , "Tim Willits", "American McGee", "Christian Antkow"
                       , "Paul Jaquays", "Brandon James", "", "+BIZ", "Todd Hollenshead"
                       , "Barrett (Bear) Alexander", "Donna Jackson", "", ""
                       , "+SPECIAL THANKS", "Ben Donges for beta testing", "", "", "", ""
                       , "", "", "+ADDITIONAL SUPPORT", "", "+LINUX PORT AND CTF"
                       , "Dave \"Zoid\" Kirsch", "", "+CINEMATIC SEQUENCES"
                       , "Ending Cinematic by Blur Studio - ", "Venice, CA", ""
                       , "Environment models for Introduction"
                       , "Cinematic by Karl Dolgener", ""
                       , "Assistance with environment design", "by Cliff Iwai", ""
                       , "+SOUND EFFECTS AND MUSIC"
                       , "Sound Design by Soundelux Media Labs."
                       , "Music Composed and Produced by"
                       , "Soundelux Media Labs.  Special thanks"
                       , "to Bill Brown, Tom Ozanich, Brian"
                       , "Celano, Jeff Eisner, and The Soundelux", "Players.", ""
                       , "\"Level Music\" by Sonic Mayhem", "www.sonicmayhem.com", ""
                       , "\"Quake II Theme Song\"", "(C) 1997 Rob Zombie. All Rights"
                       , "Reserved.", "", "Track 10 (\"Climb\") by Jer Sypult", ""
                       , "Voice of computers by", "Carly Staehlin-Taylor", ""
                       , "+THANKS TO ACTIVISION", "+IN PARTICULAR:", "", "John Tam"
                       , "Steve Rosenthal", "Marty Stratton", "Henk Hartong", ""
                       , "Quake II(tm) (C)1997 Id Software, Inc."
                       , "All Rights Reserved.  Distributed by"
                       , "Activision, Inc. under license."
                       , "Quake II(tm), the Id Software name,"
                       , "the \"Q II\"(tm) logo and id(tm)"
                       , "logo are trademarks of Id Software,"
                       , "Inc. Activision(R) is a registered"
                       , "trademark of Activision, Inc. All"
                       , "other trademarks and trade names are"
                       , "properties of their respective owners."
                       ]

xatCredits :: V.Vector B.ByteString
xatCredits = V.fromList [ "+QUAKE II MISSION PACK: THE RECKONING"
                        , "+BY", "+XATRIX ENTERTAINMENT, INC.", "", "+DESIGN AND DIRECTION"
                        , "Drew Markham", "", "+PRODUCED BY", "Greg Goodrich", ""
                        , "+PROGRAMMING", "Rafael Paiz", ""
                        , "+LEVEL DESIGN / ADDITIONAL GAME DESIGN", "Alex Mayberry", ""
                        , "+LEVEL DESIGN", "Mal Blackwell", "Dan Koppel", ""
                        , "+ART DIRECTION", "Michael \"Maxx\" Kaufman", ""
                        , "+COMPUTER GRAPHICS SUPERVISOR AND"
                        , "+CHARACTER ANIMATION DIRECTION", "Barry Dempsey", ""
                        , "+SENIOR ANIMATOR AND MODELER", "Jason Hoover", ""
                        , "+CHARACTER ANIMATION AND", "+MOTION CAPTURE SPECIALIST"
                        , "Amit Doron", "", "+ART", "Claire Praderie-Markham"
                        , "Viktor Antonov", "Corky Lehmkuhl", "", "+INTRODUCTION ANIMATION"
                        , "Dominique Drozdz", "", "+ADDITIONAL LEVEL DESIGN", "Aaron Barber"
                        , "Rhett Baldwin", "", "+3D CHARACTER ANIMATION TOOLS"
                        , "Gerry Tyra, SA Technology", ""
                        , "+ADDITIONAL EDITOR TOOL PROGRAMMING", "Robert Duffy", ""
                        , "+ADDITIONAL PROGRAMMING", "Ryan Feltrin", ""
                        , "+PRODUCTION COORDINATOR", "Victoria Sylvester", ""
                        , "+SOUND DESIGN", "Gary Bradfield", "", "+MUSIC BY", "Sonic Mayhem"
                        , "", "", "", "+SPECIAL THANKS", "+TO"
                        , "+OUR FRIENDS AT ID SOFTWARE", "", "John Carmack", "John Cash"
                        , "Brian Hook", "Adrian Carmack", "Kevin Cloud", "Paul Steed"
                        , "Tim Willits", "Christian Antkow", "Paul Jaquays", "Brandon James"
                        , "Todd Hollenshead", "Barrett (Bear) Alexander"
                        , "Dave \"Zoid\" Kirsch", "Donna Jackson", "", "", ""
                        , "+THANKS TO ACTIVISION", "+IN PARTICULAR:", "", "Marty Stratton"
                        , "Henk \"The Original Ripper\" Hartong", "Kevin Kraff"
                        , "Jamey Gottlieb", "Chris Hepburn", "", "+AND THE GAME TESTERS", ""
                        , "Tim Vanlaw", "Doug Jacobs", "Steven Rosenthal", "David Baker"
                        , "Chris Campbell", "Aaron Casillas", "Steve Elwell"
                        , "Derek Johnstone", "Igor Krinitskiy", "Samantha Lee"
                        , "Michael Spann", "Chris Toft", "Juan Valdes", ""
                        , "+THANKS TO INTERGRAPH COMPUTER SYTEMS", "+IN PARTICULAR:", ""
                        , "Michael T. Nicolaou", "", ""
                        , "Quake II Mission Pack: The Reckoning"
                        , "(tm) (C)1998 Id Software, Inc. All"
                        , "Rights Reserved. Developed by Xatrix"
                        , "Entertainment, Inc. for Id Software,"
                        , "Inc. Distributed by Activision Inc."
                        , "under license. Quake(R) is a"
                        , "registered trademark of Id Software,"
                        , "Inc. Quake II Mission Pack: The"
                        , "Reckoning(tm), Quake II(tm), the Id"
                        , "Software name, the \"Q II\"(tm) logo"
                        , "and id(tm) logo are trademarks of Id"
                        , "Software, Inc. Activision(R) is a"
                        , "registered trademark of Activision,"
                        , "Inc. Xatrix(R) is a registered"
                        , "trademark of Xatrix Entertainment,"
                        , "Inc. All other trademarks and trade"
                        , "names are properties of their", "respective owners."
                        ]

rogueCredits :: V.Vector B.ByteString
rogueCredits = V.fromList [ "+QUAKE II MISSION PACK 2: GROUND ZERO"
                          , "+BY", "+ROGUE ENTERTAINMENT, INC.", "", "+PRODUCED BY"
                          , "Jim Molinets", "", "+PROGRAMMING", "Peter Mack"
                          , "Patrick Magruder", "", "+LEVEL DESIGN", "Jim Molinets"
                          , "Cameron Lamprecht", "Berenger Fish", "Robert Selitto"
                          , "Steve Tietze", "Steve Thoms", "", "+ART DIRECTION"
                          , "Rich Fleider", "", "+ART", "Rich Fleider", "Steve Maines"
                          , "Won Choi", "", "+ANIMATION SEQUENCES", "Creat Studios"
                          , "Steve Maines", "", "+ADDITIONAL LEVEL DESIGN", "Rich Fleider"
                          , "Steve Maines", "Peter Mack", "", "+SOUND", "James Grunke", ""
                          , "+GROUND ZERO THEME", "+AND", "+MUSIC BY", "Sonic Mayhem", ""
                          , "+VWEP MODELS", "Brent \"Hentai\" Dill", "", "", ""
                          , "+SPECIAL THANKS", "+TO", "+OUR FRIENDS AT ID SOFTWARE", ""
                          , "John Carmack", "John Cash", "Brian Hook", "Adrian Carmack"
                          , "Kevin Cloud", "Paul Steed", "Tim Willits", "Christian Antkow"
                          , "Paul Jaquays", "Brandon James", "Todd Hollenshead"
                          , "Barrett (Bear) Alexander", "Katherine Anna Kang", "Donna Jackson"
                          , "Dave \"Zoid\" Kirsch", "", "", "", "+THANKS TO ACTIVISION"
                          , "+IN PARTICULAR:", "", "Marty Stratton", "Henk Hartong"
                          , "Mitch Lasky", "Steve Rosenthal", "Steve Elwell", ""
                          , "+AND THE GAME TESTERS", "", "The Ranger Clan"
                          , "Dave \"Zoid\" Kirsch", "Nihilistic Software", "Robert Duffy", ""
                          , "And Countless Others", "", "", ""
                          , "Quake II Mission Pack 2: Ground Zero"
                          , "(tm) (C)1998 Id Software, Inc. All"
                          , "Rights Reserved. Developed by Rogue"
                          , "Entertainment, Inc. for Id Software,"
                          , "Inc. Distributed by Activision Inc."
                          , "under license. Quake(R) is a"
                          , "registered trademark of Id Software,"
                          , "Inc. Quake II Mission Pack 2: Ground"
                          , "Zero(tm), Quake II(tm), the Id"
                          , "Software name, the \"Q II\"(tm) logo"
                          , "and id(tm) logo are trademarks of Id"
                          , "Software, Inc. Activision(R) is a"
                          , "registered trademark of Activision,"
                          , "Inc. Rogue(R) is a registered"
                          , "trademark of Rogue Entertainment,"
                          , "Inc. All other trademarks and trade"
                          , "names are properties of their", "respective owners."
                          ]

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

openGLMenuRef :: Ref MenuFrameworkS
openGLMenuRef = Ref 0

multiplayerMenuRef :: Ref MenuFrameworkS
multiplayerMenuRef = Ref 1

keysMenuRef :: Ref MenuFrameworkS
keysMenuRef = Ref 2

optionsMenuRef :: Ref MenuFrameworkS
optionsMenuRef = Ref 3

gameMenuRef :: Ref MenuFrameworkS
gameMenuRef = Ref 4

saveGameMenuRef :: Ref MenuFrameworkS
saveGameMenuRef = Ref 5

loadGameMenuRef :: Ref MenuFrameworkS
loadGameMenuRef = Ref 6

joinServerMenuRef :: Ref MenuFrameworkS
joinServerMenuRef = Ref 7

startServerMenuRef :: Ref MenuFrameworkS
startServerMenuRef = Ref 8

dmOptionsMenuRef :: Ref MenuFrameworkS
dmOptionsMenuRef = Ref 9

downloadOptionsMenuRef :: Ref MenuFrameworkS
downloadOptionsMenuRef = Ref 10

addressBookMenuRef :: Ref MenuFrameworkS
addressBookMenuRef = Ref 11

playerConfigMenuRef :: Ref MenuFrameworkS
playerConfigMenuRef = Ref 12

tqSliderRef :: Ref MenuSliderS
tqSliderRef = Ref 0

screenSizeSliderRef :: Ref MenuSliderS
screenSizeSliderRef = Ref 1

brightnessSliderRef :: Ref MenuSliderS
brightnessSliderRef = Ref 2

optionsSensitivitySliderRef :: Ref MenuSliderS
optionsSensitivitySliderRef = Ref 3

optionsSfxVolumeSliderRef :: Ref MenuSliderS
optionsSfxVolumeSliderRef = Ref 4

modeListRef :: Ref MenuListS
modeListRef = Ref 0

refListRef :: Ref MenuListS
refListRef = Ref 1

fsBoxRef :: Ref MenuListS
fsBoxRef = Ref 2

stippleBoxRef :: Ref MenuListS
stippleBoxRef = Ref 3

palettedTextureBoxRef :: Ref MenuListS
palettedTextureBoxRef = Ref 4

vSyncBoxRef :: Ref MenuListS
vSyncBoxRef = Ref 5

windowedMouseRef :: Ref MenuListS
windowedMouseRef = Ref 6

optionsFreeLookBoxRef :: Ref MenuListS
optionsFreeLookBoxRef = Ref 7

optionsNoAltTabBoxRef :: Ref MenuListS
optionsNoAltTabBoxRef = Ref 8

optionsAlwaysRunBoxRef :: Ref MenuListS
optionsAlwaysRunBoxRef = Ref 9

optionsInvertMouseBoxRef :: Ref MenuListS
optionsInvertMouseBoxRef = Ref 10

optionsLookSpringBoxRef :: Ref MenuListS
optionsLookSpringBoxRef = Ref 11

optionsLookStrafeBoxRef :: Ref MenuListS
optionsLookStrafeBoxRef = Ref 12

optionsCrosshairBoxRef :: Ref MenuListS
optionsCrosshairBoxRef = Ref 13

optionsJoystickBoxRef :: Ref MenuListS
optionsJoystickBoxRef = Ref 14

optionsCdVolumeBoxRef :: Ref MenuListS
optionsCdVolumeBoxRef = Ref 15

optionsQualityListRef :: Ref MenuListS
optionsQualityListRef = Ref 16

startMapListRef :: Ref MenuListS
startMapListRef = Ref 17

rulesBoxRef :: Ref MenuListS
rulesBoxRef = Ref 18

friendlyFireBoxRef :: Ref MenuListS
friendlyFireBoxRef = Ref 19

fallsBoxRef :: Ref MenuListS
fallsBoxRef = Ref 20

weaponsStayBoxRef :: Ref MenuListS
weaponsStayBoxRef = Ref 21

instantPowerUpsBoxRef :: Ref MenuListS
instantPowerUpsBoxRef = Ref 22

powerUpsBoxRef :: Ref MenuListS
powerUpsBoxRef = Ref 23

healthBoxRef :: Ref MenuListS
healthBoxRef = Ref 24

spawnFarthestBoxRef :: Ref MenuListS
spawnFarthestBoxRef = Ref 25

teamPlayBoxRef :: Ref MenuListS
teamPlayBoxRef = Ref 26

sameLevelBoxRef :: Ref MenuListS
sameLevelBoxRef = Ref 27

forceRespawnBoxRef :: Ref MenuListS
forceRespawnBoxRef = Ref 28

armorBoxRef :: Ref MenuListS
armorBoxRef = Ref 29

allowExitBoxRef :: Ref MenuListS
allowExitBoxRef = Ref 30

infiniteAmmoBoxRef :: Ref MenuListS
infiniteAmmoBoxRef = Ref 31

fixedFovBoxRef :: Ref MenuListS
fixedFovBoxRef = Ref 32

quadDropBoxRef :: Ref MenuListS
quadDropBoxRef = Ref 33

noMinesBoxRef :: Ref MenuListS
noMinesBoxRef = Ref 34

noNukesBoxRef :: Ref MenuListS
noNukesBoxRef = Ref 35

stackDoubleBoxRef :: Ref MenuListS
stackDoubleBoxRef = Ref 36

noSpheresBoxRef :: Ref MenuListS
noSpheresBoxRef = Ref 37

allowDownloadBoxRef :: Ref MenuListS
allowDownloadBoxRef = Ref 38

allowDownloadMapsBoxRef :: Ref MenuListS
allowDownloadMapsBoxRef = Ref 39

allowDownloadModelsBoxRef :: Ref MenuListS
allowDownloadModelsBoxRef = Ref 40

allowDownloadPlayersBoxRef :: Ref MenuListS
allowDownloadPlayersBoxRef = Ref 41

allowDownloadSoundsBoxRef :: Ref MenuListS
allowDownloadSoundsBoxRef = Ref 42

playerModelBoxRef :: Ref MenuListS
playerModelBoxRef = Ref 43

playerSkinBoxRef :: Ref MenuListS
playerSkinBoxRef = Ref 44

playerHandednessBoxRef :: Ref MenuListS
playerHandednessBoxRef = Ref 45

playerRateBoxRef :: Ref MenuListS
playerRateBoxRef = Ref 46

applyActionRef :: Ref MenuActionS
applyActionRef = Ref 0

defaultsActionRef :: Ref MenuActionS
defaultsActionRef = Ref 1

easyGameActionRef :: Ref MenuActionS
easyGameActionRef = Ref 2

mediumGameActionRef :: Ref MenuActionS
mediumGameActionRef = Ref 3

hardGameActionRef :: Ref MenuActionS
hardGameActionRef = Ref 4

loadGameActionRef :: Ref MenuActionS
loadGameActionRef = Ref 5

saveGameActionRef :: Ref MenuActionS
saveGameActionRef = Ref 6

creditsActionRef :: Ref MenuActionS
creditsActionRef = Ref 7

joinNetworkServerActionRef :: Ref MenuActionS
joinNetworkServerActionRef = Ref 8

startNetworkServerActionRef :: Ref MenuActionS
startNetworkServerActionRef = Ref 9

playerSetupActionRef :: Ref MenuActionS
playerSetupActionRef = Ref 10

keysAttackActionRef :: Ref MenuActionS
keysAttackActionRef = Ref 11

keysChangeWeaponActionRef :: Ref MenuActionS
keysChangeWeaponActionRef = Ref 12

keysWalkForwardActionRef :: Ref MenuActionS
keysWalkForwardActionRef = Ref 13

keysBackpedalActionRef :: Ref MenuActionS
keysBackpedalActionRef = Ref 14

keysTurnLeftActionRef :: Ref MenuActionS
keysTurnLeftActionRef = Ref 15

keysTurnRightActionRef :: Ref MenuActionS
keysTurnRightActionRef = Ref 16

keysRunActionRef :: Ref MenuActionS
keysRunActionRef = Ref 17

keysStepLeftActionRef :: Ref MenuActionS
keysStepLeftActionRef = Ref 18

keysStepRightActionRef :: Ref MenuActionS
keysStepRightActionRef = Ref 19

keysSidestepActionRef :: Ref MenuActionS
keysSidestepActionRef = Ref 20

keysLookUpActionRef :: Ref MenuActionS
keysLookUpActionRef = Ref 21

keysLookDownActionRef :: Ref MenuActionS
keysLookDownActionRef = Ref 22

keysCenterViewActionRef :: Ref MenuActionS
keysCenterViewActionRef = Ref 23

keysMouseLookActionRef :: Ref MenuActionS
keysMouseLookActionRef = Ref 24

keysKeyboardLookActionRef :: Ref MenuActionS
keysKeyboardLookActionRef = Ref 25

keysMoveUpActionRef :: Ref MenuActionS
keysMoveUpActionRef = Ref 26

keysMoveDownActionRef :: Ref MenuActionS
keysMoveDownActionRef = Ref 27

keysInventoryActionRef :: Ref MenuActionS
keysInventoryActionRef = Ref 28

keysInvUseActionRef :: Ref MenuActionS
keysInvUseActionRef = Ref 29

keysInvDropActionRef :: Ref MenuActionS
keysInvDropActionRef = Ref 30

keysInvPrevActionRef :: Ref MenuActionS
keysInvPrevActionRef = Ref 31

keysInvNextActionRef :: Ref MenuActionS
keysInvNextActionRef = Ref 32

keysHelpComputerActionRef :: Ref MenuActionS
keysHelpComputerActionRef = Ref 33

optionsDefaultsActionRef :: Ref MenuActionS
optionsDefaultsActionRef = Ref 34

optionsCustomizeOptionsActionRef :: Ref MenuActionS
optionsCustomizeOptionsActionRef = Ref 35

optionsConsoleActionRef :: Ref MenuActionS
optionsConsoleActionRef = Ref 36

joinServerSearchActionRef :: Ref MenuActionS
joinServerSearchActionRef = Ref 37

joinServerAddressBookActionRef :: Ref MenuActionS
joinServerAddressBookActionRef = Ref 38

startServerStartActionRef :: Ref MenuActionS
startServerStartActionRef = Ref 39

startServerDMOptionsActionRef :: Ref MenuActionS
startServerDMOptionsActionRef = Ref 40

playerDownloadActionRef :: Ref MenuActionS
playerDownloadActionRef = Ref 41

blankLineRef :: Ref MenuSeparatorS
blankLineRef = Ref 0

joinServerServerTitleRef :: Ref MenuSeparatorS
joinServerServerTitleRef = Ref 1

downloadTitleRef :: Ref MenuSeparatorS
downloadTitleRef = Ref 2

playerSkinTitleRef :: Ref MenuSeparatorS
playerSkinTitleRef = Ref 3

playerModelTitleRef :: Ref MenuSeparatorS
playerModelTitleRef = Ref 4

playerHandTitleRef :: Ref MenuSeparatorS
playerHandTitleRef = Ref 5

playerRateTitleRef :: Ref MenuSeparatorS
playerRateTitleRef = Ref 6

timeLimitFieldRef :: Ref MenuFieldS
timeLimitFieldRef = Ref 0

fragLimitFieldRef :: Ref MenuFieldS
fragLimitFieldRef = Ref 1

maxClientsFieldRef :: Ref MenuFieldS
maxClientsFieldRef = Ref 2

hostnameFieldRef :: Ref MenuFieldS
hostnameFieldRef = Ref 3

playerNameFieldRef :: Ref MenuFieldS
playerNameFieldRef = Ref 4

menuFields :: [MenuFieldS]
menuFields = [ newMenuFieldS -- 0 timeLimitField
             , newMenuFieldS -- 1 fragLimitField
             , newMenuFieldS -- 2 maxClientsField
             , newMenuFieldS -- 3 hostnameField
             , newMenuFieldS -- 4 playerNameField
             ]
             
menuFieldsOffset :: Int
menuFieldsOffset = length menuFields
             
addressBookFields :: V.Vector (Ref MenuFieldS)
addressBookFields = V.generate Constants.numAddressBookEntries (\idx -> Ref (idx + menuFieldsOffset))

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

loadGameActions :: V.Vector (Ref MenuActionS)
loadGameActions = V.generate maxSaveGames (\idx -> Ref (idx + loadGameActionsOffset))

saveGameActions :: V.Vector (Ref MenuActionS)
saveGameActions = V.generate maxSaveGames (\idx -> Ref (idx + saveGameActionsOffset))

joinServerActions :: V.Vector (Ref MenuActionS)
joinServerActions = V.generate Constants.maxLocalServers (\idx -> Ref (idx + joinServerActionsOffset))

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
              , _mgMenuFieldSItems     = V.fromList ( menuFields
                                                      ++ replicate Constants.numAddressBookEntries newMenuFieldS -- addressBookFields
                                                    )
              , _mgLayers              = V.replicate maxMenuDepth newMenuLayerT
              , _mgDrawFunc            = Nothing
              , _mgKeyFunc             = Nothing
              , _mgEnterSound          = False
              , _mgMenuDepth           = 0
              , _mgMainCursor          = 0
              , _mgCached              = False
              , _mgGameCursor          = 0
              , _mgSaveStrings         = V.replicate maxSaveGames ""
              , _mgSaveValid           = V.replicate maxSaveGames False
              , _mgLocalServerNames    = V.replicate Constants.maxLocalServers ""
              , _mgLocalServerNetAdr   = V.replicate Constants.maxLocalServers newNetAdrT
              , _mgCreditsStartTime    = 0
              , _mgCredits             = idCredits
              , _mgNumServers          = 0
              , _mgBindGrab            = False
              , _mgDmOptionsStatusBar  = Nothing
              , _mgMapNames            = Nothing
              }