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
idCredits = V.fromList [ "+QUAKE II BY ID SOFTWARE", B.empty
                       , "+PROGRAMMING", "John Carmack", "John Cash", "Brian Hook", B.empty
                       , "+JAVA PORT BY BYTONIC", "Carsten Weisse", "Holger Zickner", "Rene Stoeckel", B.empty, "+ART"
                       , "Adrian Carmack", "Kevin Cloud", "Paul Steed", B.empty, "+LEVEL DESIGN"
                       , "Tim Willits", "American McGee", "Christian Antkow"
                       , "Paul Jaquays", "Brandon James", B.empty, "+BIZ", "Todd Hollenshead"
                       , "Barrett (Bear) Alexander", "Donna Jackson", B.empty, B.empty
                       , "+SPECIAL THANKS", "Ben Donges for beta testing", B.empty, B.empty, B.empty, B.empty
                       , B.empty, B.empty, "+ADDITIONAL SUPPORT", B.empty, "+LINUX PORT AND CTF"
                       , "Dave \"Zoid\" Kirsch", B.empty, "+CINEMATIC SEQUENCES"
                       , "Ending Cinematic by Blur Studio - ", "Venice, CA", B.empty
                       , "Environment models for Introduction"
                       , "Cinematic by Karl Dolgener", B.empty
                       , "Assistance with environment design", "by Cliff Iwai", B.empty
                       , "+SOUND EFFECTS AND MUSIC"
                       , "Sound Design by Soundelux Media Labs."
                       , "Music Composed and Produced by"
                       , "Soundelux Media Labs.  Special thanks"
                       , "to Bill Brown, Tom Ozanich, Brian"
                       , "Celano, Jeff Eisner, and The Soundelux", "Players.", B.empty
                       , "\"Level Music\" by Sonic Mayhem", "www.sonicmayhem.com", B.empty
                       , "\"Quake II Theme Song\"", "(C) 1997 Rob Zombie. All Rights"
                       , "Reserved.", B.empty, "Track 10 (\"Climb\") by Jer Sypult", B.empty
                       , "Voice of computers by", "Carly Staehlin-Taylor", B.empty
                       , "+THANKS TO ACTIVISION", "+IN PARTICULAR:", B.empty, "John Tam"
                       , "Steve Rosenthal", "Marty Stratton", "Henk Hartong", B.empty
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
                        , "+BY", "+XATRIX ENTERTAINMENT, INC.", B.empty, "+DESIGN AND DIRECTION"
                        , "Drew Markham", B.empty, "+PRODUCED BY", "Greg Goodrich", B.empty
                        , "+PROGRAMMING", "Rafael Paiz", B.empty
                        , "+LEVEL DESIGN / ADDITIONAL GAME DESIGN", "Alex Mayberry", B.empty
                        , "+LEVEL DESIGN", "Mal Blackwell", "Dan Koppel", B.empty
                        , "+ART DIRECTION", "Michael \"Maxx\" Kaufman", B.empty
                        , "+COMPUTER GRAPHICS SUPERVISOR AND"
                        , "+CHARACTER ANIMATION DIRECTION", "Barry Dempsey", B.empty
                        , "+SENIOR ANIMATOR AND MODELER", "Jason Hoover", B.empty
                        , "+CHARACTER ANIMATION AND", "+MOTION CAPTURE SPECIALIST"
                        , "Amit Doron", B.empty, "+ART", "Claire Praderie-Markham"
                        , "Viktor Antonov", "Corky Lehmkuhl", B.empty, "+INTRODUCTION ANIMATION"
                        , "Dominique Drozdz", B.empty, "+ADDITIONAL LEVEL DESIGN", "Aaron Barber"
                        , "Rhett Baldwin", B.empty, "+3D CHARACTER ANIMATION TOOLS"
                        , "Gerry Tyra, SA Technology", B.empty
                        , "+ADDITIONAL EDITOR TOOL PROGRAMMING", "Robert Duffy", B.empty
                        , "+ADDITIONAL PROGRAMMING", "Ryan Feltrin", B.empty
                        , "+PRODUCTION COORDINATOR", "Victoria Sylvester", B.empty
                        , "+SOUND DESIGN", "Gary Bradfield", B.empty, "+MUSIC BY", "Sonic Mayhem"
                        , B.empty, B.empty, B.empty, "+SPECIAL THANKS", "+TO"
                        , "+OUR FRIENDS AT ID SOFTWARE", B.empty, "John Carmack", "John Cash"
                        , "Brian Hook", "Adrian Carmack", "Kevin Cloud", "Paul Steed"
                        , "Tim Willits", "Christian Antkow", "Paul Jaquays", "Brandon James"
                        , "Todd Hollenshead", "Barrett (Bear) Alexander"
                        , "Dave \"Zoid\" Kirsch", "Donna Jackson", B.empty, B.empty, B.empty
                        , "+THANKS TO ACTIVISION", "+IN PARTICULAR:", B.empty, "Marty Stratton"
                        , "Henk \"The Original Ripper\" Hartong", "Kevin Kraff"
                        , "Jamey Gottlieb", "Chris Hepburn", B.empty, "+AND THE GAME TESTERS", B.empty
                        , "Tim Vanlaw", "Doug Jacobs", "Steven Rosenthal", "David Baker"
                        , "Chris Campbell", "Aaron Casillas", "Steve Elwell"
                        , "Derek Johnstone", "Igor Krinitskiy", "Samantha Lee"
                        , "Michael Spann", "Chris Toft", "Juan Valdes", B.empty
                        , "+THANKS TO INTERGRAPH COMPUTER SYTEMS", "+IN PARTICULAR:", B.empty
                        , "Michael T. Nicolaou", B.empty, B.empty
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
                          , "+BY", "+ROGUE ENTERTAINMENT, INC.", B.empty, "+PRODUCED BY"
                          , "Jim Molinets", B.empty, "+PROGRAMMING", "Peter Mack"
                          , "Patrick Magruder", B.empty, "+LEVEL DESIGN", "Jim Molinets"
                          , "Cameron Lamprecht", "Berenger Fish", "Robert Selitto"
                          , "Steve Tietze", "Steve Thoms", B.empty, "+ART DIRECTION"
                          , "Rich Fleider", B.empty, "+ART", "Rich Fleider", "Steve Maines"
                          , "Won Choi", B.empty, "+ANIMATION SEQUENCES", "Creat Studios"
                          , "Steve Maines", B.empty, "+ADDITIONAL LEVEL DESIGN", "Rich Fleider"
                          , "Steve Maines", "Peter Mack", B.empty, "+SOUND", "James Grunke", B.empty
                          , "+GROUND ZERO THEME", "+AND", "+MUSIC BY", "Sonic Mayhem", B.empty
                          , "+VWEP MODELS", "Brent \"Hentai\" Dill", B.empty, B.empty, B.empty
                          , "+SPECIAL THANKS", "+TO", "+OUR FRIENDS AT ID SOFTWARE", B.empty
                          , "John Carmack", "John Cash", "Brian Hook", "Adrian Carmack"
                          , "Kevin Cloud", "Paul Steed", "Tim Willits", "Christian Antkow"
                          , "Paul Jaquays", "Brandon James", "Todd Hollenshead"
                          , "Barrett (Bear) Alexander", "Katherine Anna Kang", "Donna Jackson"
                          , "Dave \"Zoid\" Kirsch", B.empty, B.empty, B.empty, "+THANKS TO ACTIVISION"
                          , "+IN PARTICULAR:", B.empty, "Marty Stratton", "Henk Hartong"
                          , "Mitch Lasky", "Steve Rosenthal", "Steve Elwell", B.empty
                          , "+AND THE GAME TESTERS", B.empty, "The Ranger Clan"
                          , "Dave \"Zoid\" Kirsch", "Nihilistic Software", "Robert Duffy", B.empty
                          , "And Countless Others", B.empty, B.empty, B.empty
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

openGLMenuRef :: Ref' MenuFrameworkS
openGLMenuRef = Ref Constants.noParent 0

multiplayerMenuRef :: Ref' MenuFrameworkS
multiplayerMenuRef = Ref Constants.noParent 1

keysMenuRef :: Ref' MenuFrameworkS
keysMenuRef = Ref Constants.noParent 2

optionsMenuRef :: Ref' MenuFrameworkS
optionsMenuRef = Ref Constants.noParent 3

gameMenuRef :: Ref' MenuFrameworkS
gameMenuRef = Ref Constants.noParent 4

saveGameMenuRef :: Ref' MenuFrameworkS
saveGameMenuRef = Ref Constants.noParent 5

loadGameMenuRef :: Ref' MenuFrameworkS
loadGameMenuRef = Ref Constants.noParent 6

joinServerMenuRef :: Ref' MenuFrameworkS
joinServerMenuRef = Ref Constants.noParent 7

startServerMenuRef :: Ref' MenuFrameworkS
startServerMenuRef = Ref Constants.noParent 8

dmOptionsMenuRef :: Ref' MenuFrameworkS
dmOptionsMenuRef = Ref Constants.noParent 9

downloadOptionsMenuRef :: Ref' MenuFrameworkS
downloadOptionsMenuRef = Ref Constants.noParent 10

addressBookMenuRef :: Ref' MenuFrameworkS
addressBookMenuRef = Ref Constants.noParent 11

playerConfigMenuRef :: Ref' MenuFrameworkS
playerConfigMenuRef = Ref Constants.noParent 12

tqSliderRef :: Ref' MenuSliderS
tqSliderRef = Ref Constants.noParent 0

screenSizeSliderRef :: Ref' MenuSliderS
screenSizeSliderRef = Ref Constants.noParent 1

brightnessSliderRef :: Ref' MenuSliderS
brightnessSliderRef = Ref Constants.noParent 2

optionsSensitivitySliderRef :: Ref' MenuSliderS
optionsSensitivitySliderRef = Ref Constants.noParent 3

optionsSfxVolumeSliderRef :: Ref' MenuSliderS
optionsSfxVolumeSliderRef = Ref Constants.noParent 4

modeListRef :: Ref' MenuListS
modeListRef = Ref Constants.noParent 0

refListRef :: Ref' MenuListS
refListRef = Ref Constants.noParent 1

fsBoxRef :: Ref' MenuListS
fsBoxRef = Ref Constants.noParent 2

stippleBoxRef :: Ref' MenuListS
stippleBoxRef = Ref Constants.noParent 3

palettedTextureBoxRef :: Ref' MenuListS
palettedTextureBoxRef = Ref Constants.noParent 4

vSyncBoxRef :: Ref' MenuListS
vSyncBoxRef = Ref Constants.noParent 5

windowedMouseRef :: Ref' MenuListS
windowedMouseRef = Ref Constants.noParent 6

optionsFreeLookBoxRef :: Ref' MenuListS
optionsFreeLookBoxRef = Ref Constants.noParent 7

optionsNoAltTabBoxRef :: Ref' MenuListS
optionsNoAltTabBoxRef = Ref Constants.noParent 8

optionsAlwaysRunBoxRef :: Ref' MenuListS
optionsAlwaysRunBoxRef = Ref Constants.noParent 9

optionsInvertMouseBoxRef :: Ref' MenuListS
optionsInvertMouseBoxRef = Ref Constants.noParent 10

optionsLookSpringBoxRef :: Ref' MenuListS
optionsLookSpringBoxRef = Ref Constants.noParent 11

optionsLookStrafeBoxRef :: Ref' MenuListS
optionsLookStrafeBoxRef = Ref Constants.noParent 12

optionsCrosshairBoxRef :: Ref' MenuListS
optionsCrosshairBoxRef = Ref Constants.noParent 13

optionsJoystickBoxRef :: Ref' MenuListS
optionsJoystickBoxRef = Ref Constants.noParent 14

optionsCdVolumeBoxRef :: Ref' MenuListS
optionsCdVolumeBoxRef = Ref Constants.noParent 15

optionsQualityListRef :: Ref' MenuListS
optionsQualityListRef = Ref Constants.noParent 16

startMapListRef :: Ref' MenuListS
startMapListRef = Ref Constants.noParent 17

rulesBoxRef :: Ref' MenuListS
rulesBoxRef = Ref Constants.noParent 18

friendlyFireBoxRef :: Ref' MenuListS
friendlyFireBoxRef = Ref Constants.noParent 19

fallsBoxRef :: Ref' MenuListS
fallsBoxRef = Ref Constants.noParent 20

weaponsStayBoxRef :: Ref' MenuListS
weaponsStayBoxRef = Ref Constants.noParent 21

instantPowerUpsBoxRef :: Ref' MenuListS
instantPowerUpsBoxRef = Ref Constants.noParent 22

powerUpsBoxRef :: Ref' MenuListS
powerUpsBoxRef = Ref Constants.noParent 23

healthBoxRef :: Ref' MenuListS
healthBoxRef = Ref Constants.noParent 24

spawnFarthestBoxRef :: Ref' MenuListS
spawnFarthestBoxRef = Ref Constants.noParent 25

teamPlayBoxRef :: Ref' MenuListS
teamPlayBoxRef = Ref Constants.noParent 26

sameLevelBoxRef :: Ref' MenuListS
sameLevelBoxRef = Ref Constants.noParent 27

forceRespawnBoxRef :: Ref' MenuListS
forceRespawnBoxRef = Ref Constants.noParent 28

armorBoxRef :: Ref' MenuListS
armorBoxRef = Ref Constants.noParent 29

allowExitBoxRef :: Ref' MenuListS
allowExitBoxRef = Ref Constants.noParent 30

infiniteAmmoBoxRef :: Ref' MenuListS
infiniteAmmoBoxRef = Ref Constants.noParent 31

fixedFovBoxRef :: Ref' MenuListS
fixedFovBoxRef = Ref Constants.noParent 32

quadDropBoxRef :: Ref' MenuListS
quadDropBoxRef = Ref Constants.noParent 33

noMinesBoxRef :: Ref' MenuListS
noMinesBoxRef = Ref Constants.noParent 34

noNukesBoxRef :: Ref' MenuListS
noNukesBoxRef = Ref Constants.noParent 35

stackDoubleBoxRef :: Ref' MenuListS
stackDoubleBoxRef = Ref Constants.noParent 36

noSpheresBoxRef :: Ref' MenuListS
noSpheresBoxRef = Ref Constants.noParent 37

allowDownloadBoxRef :: Ref' MenuListS
allowDownloadBoxRef = Ref Constants.noParent 38

allowDownloadMapsBoxRef :: Ref' MenuListS
allowDownloadMapsBoxRef = Ref Constants.noParent 39

allowDownloadModelsBoxRef :: Ref' MenuListS
allowDownloadModelsBoxRef = Ref Constants.noParent 40

allowDownloadPlayersBoxRef :: Ref' MenuListS
allowDownloadPlayersBoxRef = Ref Constants.noParent 41

allowDownloadSoundsBoxRef :: Ref' MenuListS
allowDownloadSoundsBoxRef = Ref Constants.noParent 42

playerModelBoxRef :: Ref' MenuListS
playerModelBoxRef = Ref Constants.noParent 43

playerSkinBoxRef :: Ref' MenuListS
playerSkinBoxRef = Ref Constants.noParent 44

playerHandednessBoxRef :: Ref' MenuListS
playerHandednessBoxRef = Ref Constants.noParent 45

playerRateBoxRef :: Ref' MenuListS
playerRateBoxRef = Ref Constants.noParent 46

applyActionRef :: Ref' MenuActionS
applyActionRef = Ref Constants.noParent 0

defaultsActionRef :: Ref' MenuActionS
defaultsActionRef = Ref Constants.noParent 1

easyGameActionRef :: Ref' MenuActionS
easyGameActionRef = Ref Constants.noParent 2

mediumGameActionRef :: Ref' MenuActionS
mediumGameActionRef = Ref Constants.noParent 3

hardGameActionRef :: Ref' MenuActionS
hardGameActionRef = Ref Constants.noParent 4

loadGameActionRef :: Ref' MenuActionS
loadGameActionRef = Ref Constants.noParent 5

saveGameActionRef :: Ref' MenuActionS
saveGameActionRef = Ref Constants.noParent 6

creditsActionRef :: Ref' MenuActionS
creditsActionRef = Ref Constants.noParent 7

joinNetworkServerActionRef :: Ref' MenuActionS
joinNetworkServerActionRef = Ref Constants.noParent 8

startNetworkServerActionRef :: Ref' MenuActionS
startNetworkServerActionRef = Ref Constants.noParent 9

playerSetupActionRef :: Ref' MenuActionS
playerSetupActionRef = Ref Constants.noParent 10

keysAttackActionRef :: Ref' MenuActionS
keysAttackActionRef = Ref Constants.noParent 11

keysChangeWeaponActionRef :: Ref' MenuActionS
keysChangeWeaponActionRef = Ref Constants.noParent 12

keysWalkForwardActionRef :: Ref' MenuActionS
keysWalkForwardActionRef = Ref Constants.noParent 13

keysBackpedalActionRef :: Ref' MenuActionS
keysBackpedalActionRef = Ref Constants.noParent 14

keysTurnLeftActionRef :: Ref' MenuActionS
keysTurnLeftActionRef = Ref Constants.noParent 15

keysTurnRightActionRef :: Ref' MenuActionS
keysTurnRightActionRef = Ref Constants.noParent 16

keysRunActionRef :: Ref' MenuActionS
keysRunActionRef = Ref Constants.noParent 17

keysStepLeftActionRef :: Ref' MenuActionS
keysStepLeftActionRef = Ref Constants.noParent 18

keysStepRightActionRef :: Ref' MenuActionS
keysStepRightActionRef = Ref Constants.noParent 19

keysSidestepActionRef :: Ref' MenuActionS
keysSidestepActionRef = Ref Constants.noParent 20

keysLookUpActionRef :: Ref' MenuActionS
keysLookUpActionRef = Ref Constants.noParent 21

keysLookDownActionRef :: Ref' MenuActionS
keysLookDownActionRef = Ref Constants.noParent 22

keysCenterViewActionRef :: Ref' MenuActionS
keysCenterViewActionRef = Ref Constants.noParent 23

keysMouseLookActionRef :: Ref' MenuActionS
keysMouseLookActionRef = Ref Constants.noParent 24

keysKeyboardLookActionRef :: Ref' MenuActionS
keysKeyboardLookActionRef = Ref Constants.noParent 25

keysMoveUpActionRef :: Ref' MenuActionS
keysMoveUpActionRef = Ref Constants.noParent 26

keysMoveDownActionRef :: Ref' MenuActionS
keysMoveDownActionRef = Ref Constants.noParent 27

keysInventoryActionRef :: Ref' MenuActionS
keysInventoryActionRef = Ref Constants.noParent 28

keysInvUseActionRef :: Ref' MenuActionS
keysInvUseActionRef = Ref Constants.noParent 29

keysInvDropActionRef :: Ref' MenuActionS
keysInvDropActionRef = Ref Constants.noParent 30

keysInvPrevActionRef :: Ref' MenuActionS
keysInvPrevActionRef = Ref Constants.noParent 31

keysInvNextActionRef :: Ref' MenuActionS
keysInvNextActionRef = Ref Constants.noParent 32

keysHelpComputerActionRef :: Ref' MenuActionS
keysHelpComputerActionRef = Ref Constants.noParent 33

optionsDefaultsActionRef :: Ref' MenuActionS
optionsDefaultsActionRef = Ref Constants.noParent 34

optionsCustomizeOptionsActionRef :: Ref' MenuActionS
optionsCustomizeOptionsActionRef = Ref Constants.noParent 35

optionsConsoleActionRef :: Ref' MenuActionS
optionsConsoleActionRef = Ref Constants.noParent 36

joinServerSearchActionRef :: Ref' MenuActionS
joinServerSearchActionRef = Ref Constants.noParent 37

joinServerAddressBookActionRef :: Ref' MenuActionS
joinServerAddressBookActionRef = Ref Constants.noParent 38

startServerStartActionRef :: Ref' MenuActionS
startServerStartActionRef = Ref Constants.noParent 39

startServerDMOptionsActionRef :: Ref' MenuActionS
startServerDMOptionsActionRef = Ref Constants.noParent 40

playerDownloadActionRef :: Ref' MenuActionS
playerDownloadActionRef = Ref Constants.noParent 41

blankLineRef :: Ref' MenuSeparatorS
blankLineRef = Ref Constants.noParent 0

joinServerServerTitleRef :: Ref' MenuSeparatorS
joinServerServerTitleRef = Ref Constants.noParent 1

downloadTitleRef :: Ref' MenuSeparatorS
downloadTitleRef = Ref Constants.noParent 2

playerSkinTitleRef :: Ref' MenuSeparatorS
playerSkinTitleRef = Ref Constants.noParent 3

playerModelTitleRef :: Ref' MenuSeparatorS
playerModelTitleRef = Ref Constants.noParent 4

playerHandTitleRef :: Ref' MenuSeparatorS
playerHandTitleRef = Ref Constants.noParent 5

playerRateTitleRef :: Ref' MenuSeparatorS
playerRateTitleRef = Ref Constants.noParent 6

timeLimitFieldRef :: Ref' MenuFieldS
timeLimitFieldRef = Ref Constants.noParent 0

fragLimitFieldRef :: Ref' MenuFieldS
fragLimitFieldRef = Ref Constants.noParent 1

maxClientsFieldRef :: Ref' MenuFieldS
maxClientsFieldRef = Ref Constants.noParent 2

hostnameFieldRef :: Ref' MenuFieldS
hostnameFieldRef = Ref Constants.noParent 3

playerNameFieldRef :: Ref' MenuFieldS
playerNameFieldRef = Ref Constants.noParent 4

menuFields :: [MenuFieldS]
menuFields = [ newMenuFieldS -- 0 timeLimitField
             , newMenuFieldS -- 1 fragLimitField
             , newMenuFieldS -- 2 maxClientsField
             , newMenuFieldS -- 3 hostnameField
             , newMenuFieldS -- 4 playerNameField
             ]
             
menuFieldsOffset :: Int
menuFieldsOffset = length menuFields
             
addressBookFields :: V.Vector (Ref' MenuFieldS)
addressBookFields = V.generate Constants.numAddressBookEntries (\idx -> Ref Constants.noParent (idx + menuFieldsOffset))

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

loadGameActions :: V.Vector (Ref' MenuActionS)
loadGameActions = V.generate maxSaveGames (\idx -> Ref Constants.noParent (idx + loadGameActionsOffset))

saveGameActions :: V.Vector (Ref' MenuActionS)
saveGameActions = V.generate maxSaveGames (\idx -> Ref Constants.noParent (idx + saveGameActionsOffset))

joinServerActions :: V.Vector (Ref' MenuActionS)
joinServerActions = V.generate Constants.maxLocalServers (\idx -> Ref Constants.noParent (idx + joinServerActionsOffset))

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
              , _mgSaveStrings         = V.replicate maxSaveGames B.empty
              , _mgSaveValid           = V.replicate maxSaveGames False
              , _mgLocalServerNames    = V.replicate Constants.maxLocalServers B.empty
              , _mgLocalServerNetAdr   = V.replicate Constants.maxLocalServers newNetAdrT
              , _mgCreditsStartTime    = 0
              , _mgCredits             = idCredits
              , _mgNumServers          = 0
              , _mgBindGrab            = False
              , _mgDmOptionsStatusBar  = Nothing
              , _mgMapNames            = Nothing
              }