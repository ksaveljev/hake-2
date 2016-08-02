{-# LANGUAGE FlexibleContexts #-}
module Client.Menu
  ( addToServerList
  , draw
  , initialize
  , menuAddItem
  , menuCenter
  ) where

import           Client.ClientStateT
import           Client.ClientStaticT
import {-# SOURCE #-} qualified Client.CL as CL
import qualified Client.Console as Console
import qualified Client.Key as Key
import qualified Client.KeyConstants as KeyConstants
import           Client.MenuActionS
import           Client.MenuCommonS
import           Client.MenuFieldS
import           Client.MenuFrameworkS
import           Client.MenuLayerT
import           Client.MenuListS
import           Client.MenuSeparatorS
import           Client.MenuSliderS
import           Client.RefExportT
import qualified Client.SCRShared as SCR
import {-# SOURCE #-} qualified Client.VID as VID
import           Client.VidDefT
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS as FS
import           QCommon.XCommandT
import           QuakeRef
import           QuakeState
import           Render.Renderer
import qualified Sound.S as S
import qualified Sys.NET as NET
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib
import qualified Util.QuakeFile as QuakeFile

import           Control.Applicative (liftA2)
import           Control.Lens (use, ix, (^.), (.=), (%=), (-=), (&), (.~), (%~), (+~), _2)
import           Control.Monad (void, join, when, unless)
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (ord, toLower)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Linear (V4(..), _x)
import           System.IO (Handle, IOMode(ReadMode))

mainItems :: Int
mainItems = 5

numCursorFrames :: Int
numCursorFrames = 15

yesNoNames :: V.Vector B.ByteString
yesNoNames = V.fromList ["no", "yes"]

cdMusicItems :: V.Vector B.ByteString
cdMusicItems = V.fromList ["disabled", "enabled"]

teamPlayNames :: V.Vector B.ByteString
teamPlayNames = V.fromList ["disabled", "by skin", "by model"]

menuInSound :: B.ByteString
menuInSound = "misc/menu1.wav"

menuMoveSound :: B.ByteString
menuMoveSound = "misc/menu2.wav"

menuOutSound :: B.ByteString
menuOutSound = "misc/menu3.wav"

dmCoopNames :: V.Vector B.ByteString
dmCoopNames = V.fromList ["deathmatch", "cooperative"]

dmCoopNamesRogue :: V.Vector B.ByteString
dmCoopNamesRogue = V.fromList ["deathmatch", "cooperative", "tag"]

bindNames :: V.Vector (B.ByteString, B.ByteString)
bindNames = V.fromList [ ("+attack", "attack")
                       , ("weapnext", "next weapon")
                       , ("+forward", "walk forward")
                       , ("+back", "backpedal")
                       , ("+left", "turn left")
                       , ("+right", "turn right")
                       , ("+speed", "run")
                       , ("+moveleft", "step left")
                       , ("+moveright", "step right")
                       , ("+strafe", "sidestep")
                       , ("+lookup", "look up")
                       , ("+lookdown", "look down")
                       , ("centerview", "center view")
                       , ("+mlook", "mouse look")
                       , ("+klook", "keyboard look")
                       , ("+moveup", "up / jump")
                       , ("+movedown", "down / crouch")
                       , ("inven", "inventory")
                       , ("invuse", "use item")
                       , ("invdrop", "drop item")
                       , ("invprev", "prev item")
                       , ("invnext", "next item")
                       , ("cmd help", "help computer")
                       ]

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("menu_main", Just menuMainF), ("menu_game", Just menuGameF)
  , ("menu_video", Just menuVideoF), ("menu_options", Just menuOptionsF)
  , ("menu_loadgame", Just menuLoadGameF), ("menu_savegame", Just menuSaveGameF)
  , ("menu_joinserver", Just menuJoinServerF), ("menu_addressbook", Just menuAddressBookF)
  , ("menu_startserver", Just menuStartServerF), ("menu_dmoptions", Just menuDMOptionsF)
  , ("menu_credits", Just menuCreditsF), ("menu_multiplayer", Just menuMultiplayerF)
  , ("menu_playerconfig", Just menuPlayerConfigF), ("menu_keys", Just menuKeysF)
  , ("menu_downloadoptions", Just menuDownloadOptionsF), ("menu_quit", Just menuQuitF)
  ]

initialize :: Quake ()
initialize =
  do Cmd.addInitialCommands initialCommands
     menuGlobals.mgLayers .= V.replicate maxMenuDepth newMenuLayerT

menuAddItem :: Ref MenuFrameworkS -> MenuItemRef -> Quake ()
menuAddItem menuFrameworkRef menuItemRef =
  do menu <- readRef menuFrameworkRef
     checkEmptyMenu menu
     addItem menu
     n <- menuTallySlots menuFrameworkRef
     modifyRef menuFrameworkRef (\v -> v & mfNSlots .~ n)
  where checkEmptyMenu menu
          | (menu^.mfNItems) == 0 = modifyRef menuFrameworkRef (\v -> v & mfNSlots .~ 0)
          | otherwise = return ()
        addItem menu
          | (menu^.mfNItems) < Constants.maxMenuItems =
              do modifyRef menuFrameworkRef (\v -> v & mfItems %~ (`V.snoc` menuItemRef)
                                                     & mfNItems +~ 1)
                 setParent menuFrameworkRef menuItemRef
          | otherwise = return ()

setParent :: Ref MenuFrameworkS -> MenuItemRef -> Quake ()
setParent parentRef (MenuActionRef ref) = modifyRef ref (\v -> v & maGeneric.mcParent .~ Just parentRef)
setParent parentRef (MenuFieldRef ref) = modifyRef ref (\v -> v & mflGeneric.mcParent .~ Just parentRef)
setParent parentRef (MenuListRef ref) = modifyRef ref (\v -> v & mlGeneric.mcParent .~ Just parentRef)
setParent parentRef (MenuSeparatorRef ref) = modifyRef ref (\v -> v & mspGeneric.mcParent .~ Just parentRef)
setParent parentRef (MenuSliderRef ref) = modifyRef ref (\v -> v & msGeneric.mcParent .~ Just parentRef)

menuCenter :: Ref MenuFrameworkS -> Quake ()
menuCenter menuFrameworkRef =
  do menu <- readRef menuFrameworkRef
     height <- getMenuItemHeight (V.last (menu^.mfItems))
     h <- use (globals.gVidDef.vdHeight)
     modifyRef menuFrameworkRef (\v -> v & mfY .~ (h - (height + 10)) `div` 2)
  where getMenuItemHeight (MenuListRef ref) = fmap (^.mlGeneric.mcY) (readRef ref)
        getMenuItemHeight (MenuActionRef ref) = fmap (^.maGeneric.mcY) (readRef ref)
        getMenuItemHeight (MenuSliderRef ref) = fmap (^.msGeneric.mcY) (readRef ref)
        getMenuItemHeight (MenuSeparatorRef ref) = fmap (^.mspGeneric.mcY) (readRef ref)
        getMenuItemHeight (MenuFieldRef ref) = fmap (^.mflGeneric.mcY) (readRef ref)

menuTallySlots :: Ref MenuFrameworkS -> Quake Int
menuTallySlots menuFrameworkRef =
  do menu <- readRef menuFrameworkRef
     itemsNum <- V.mapM numberOfItems (menu^.mfItems)
     return (V.foldl' (+) 0 itemsNum)
  where numberOfItems (MenuListRef ref) =
          do menuItem <- readRef ref
             return (V.length (menuItem^.mlItemNames))
        numberOfItems _ = return 1

menuMainF :: XCommandT
menuMainF = XCommandT "Menu.menuMainF" (pushMenu mainDrawF mainKeyF)

menuGameF :: XCommandT
menuGameF = XCommandT "Menu.menuGameF" $
  do gameMenuInit
     pushMenu gameMenuDrawF gameMenuKeyF
     menuGlobals.mgGameCursor .= 1

menuVideoF :: XCommandT
menuVideoF = XCommandT "Menu.menuVideoF" $
  do VID.menuInit
     pushMenu VID.menuDrawF VID.menuKeyF

menuOptionsF :: XCommandT
menuOptionsF = XCommandT "Menu.menuOptionsF" $
  do optionsMenuInit
     pushMenu optionsMenuDrawF optionsMenuKey

menuLoadGameF :: XCommandT
menuLoadGameF = XCommandT "Menu.loadGameF" $
  do loadGameMenuInit
     pushMenu loadGameMenuDrawF loadGameMenuKeyF

menuSaveGameF :: XCommandT
menuSaveGameF = XCommandT "Menu.saveGameF" $
  do serverState <- use (globals.gServerState)
     initOnlyWhenPlaying serverState
  where initOnlyWhenPlaying 0 =
          do saveGameMenuInit
             pushMenu saveGameMenuDrawF saveGameMenuKeyF
        initOnlyWhenPlaying _ = return ()

menuJoinServerF :: XCommandT
menuJoinServerF = XCommandT "Menu.menuJoinServerF" $
  do joinServerMenuInit
     pushMenu joinServerMenuDrawF joinServerMenuKeyF

menuAddressBookF :: XCommandT
menuAddressBookF = XCommandT "Menu.menuAddressBookF" $
  do addressBookMenuInit
     pushMenu addressBookMenuDrawF addressBookMenuKeyF

menuStartServerF :: XCommandT
menuStartServerF = XCommandT "Menu.menuStartServerF" $
  do startServerMenuInit
     pushMenu startServerMenuDrawF startServerMenuKeyF

menuDMOptionsF :: XCommandT
menuDMOptionsF = XCommandT "Menu.menuDMOptionsF" $
  do dmOptionsMenuInit
     pushMenu dmOptionsMenuDrawF dmOptionsMenuKeyF

menuCreditsF :: XCommandT
menuCreditsF = error "Menu.menuCreditsF" $
  do realTime <- use (globals.gCls.csRealTime)
     credits <- loadCredits =<< FS.loadFile "credits"
     menuGlobals.mgCreditsStartTime .= realTime
     menuGlobals.mgCredits .= credits
     pushMenu creditsMenuDrawF creditsKeyF

loadCredits :: Maybe B.ByteString -> Quake (V.Vector B.ByteString)
loadCredits Nothing = fmap pickCredits (FS.developerSearchPath 1)
  where pickCredits 1 = xatCredits
        pickCredits 2 = rogueCredits
        pickCredits _ = idCredits
loadCredits (Just contents) = return (V.fromList (Lib.tokenise "\r\n" contents))

menuMultiplayerF :: XCommandT
menuMultiplayerF = XCommandT "Menu.menuMultiplayerF" $
  do multiplayerMenuInit
     pushMenu multiplayerMenuDrawF multiplayerMenuKey

menuPlayerConfigF :: XCommandT
menuPlayerConfigF = XCommandT "Menu.menuPlayerConfigF" $
  playerConfigMenuInit >>= validate
  where validate False =
          menuSetStatusBar multiplayerMenuRef (Just "No valid player models found")
        validate True =
          do menuSetStatusBar multiplayerMenuRef Nothing
             pushMenu playerConfigMenuDrawF playerConfigMenuKeyF

menuKeysF :: XCommandT
menuKeysF = XCommandT "Menu.menuKeysF" $
  do keysMenuInit
     pushMenu keysMenuDrawF keysMenuKeyF

menuDownloadOptionsF :: XCommandT
menuDownloadOptionsF = XCommandT "Menu.menuDownloadOptionsF" $
  do downloadOptionsMenuInit
     pushMenu downloadOptionsMenuDrawF downloadOptionsMenuKeyF

menuQuitF :: XCommandT
menuQuitF = XCommandT "Menu.menuQuitF" (pushMenu quitDrawF quitKeyF)

pushMenu :: XCommandT -> KeyFuncT -> Quake ()
pushMenu drawF keyF =
  do pauseIfNeeded
     menuDepth <- use (menuGlobals.mgMenuDepth)
     menuIdx <- getMenuIdx menuDepth =<< use (menuGlobals.mgLayers)
     when (menuIdx == menuDepth) (addLayer menuDepth menuIdx)
     menuGlobals %= (\v -> v & mgMenuDepth +~ 1
                             & mgDrawFunc .~ Just drawF
                             & mgKeyFunc .~ Just keyF
                             & mgEnterSound .~ True)
     globals.gCls.csKeyDest .= Constants.keyMenu
  where pauseIfNeeded =
          do maxClients <- CVar.variableValue "maxclients"
             serverState <- use (globals.gServerState)
             when (maxClients == 1 && serverState /= 0) $
               void (CVar.set "paused" "1")
        getMenuIdx menuDepth layers =
          case V.findIndex (\layer -> (layer^.mlDraw) == Just drawF && (layer^.mlKey) == Just keyF) layers of
            Nothing -> return menuDepth
            Just idx -> updateMenuDepthAndReturn menuDepth idx
        updateMenuDepthAndReturn menuDepth idx
          | idx >= menuDepth = return menuDepth
          | otherwise =
              do menuGlobals.mgMenuDepth .= idx
                 return idx
        addLayer menuDepth menuIdx
          | menuDepth == maxMenuDepth = Com.fatalError "PushMenu: MAX_MENU_DEPTH"
          | otherwise =
              menuGlobals.mgLayers.ix menuIdx %= (\v -> v & mlDraw .~ Just drawF
                                                          & mlKey .~ Just keyF)
menuNames :: V.Vector B.ByteString
menuNames = V.fromList [ "m_main_game", "m_main_multiplayer", "m_main_options"
                       , "m_main_video" , "m_main_quit"
                       ]

mainDrawF :: XCommandT
mainDrawF = XCommandT "Menu.mainDrawF" $
  do vidDef <- use (globals.gVidDef)
     renderer <- use (globals.gRenderer)
     maybe rendererError (mainDraw vidDef) renderer
  where rendererError = Com.fatalError "Menu.mainDrawF renderer is Nothing"

mainDraw :: VidDefT -> Renderer -> Quake ()
mainDraw vidDef renderer =
  do widest <- V.foldM findWidest (-1) menuNames
     mainCursor <- use (menuGlobals.mgMainCursor)
     realTime <- use (globals.gCls.csRealTime)
     V.imapM_ (drawMenuPic renderer mainCursor yStart (xOffset widest)) menuNames
     (renderer^.rRefExport.reDrawPic) (xOffset widest) (yStart + mainCursor * 40 + 13) (litName mainCursor)
     drawCursor (xOffset widest - 25) (yStart + mainCursor * 40 + 11) ((realTime `div` 100) `mod` numCursorFrames)
     drawPlaque renderer (xOffset widest) yStart
  where findWidest widest menuName =
          do dim <- (renderer^.rRefExport.reDrawGetPicSize) menuName
             maybe (return widest) (\(w, _) -> return (max w widest)) dim
        yStart = (vidDef^.vdHeight) `div` 2 - 110
        xOffset widest = ((vidDef^.vdWidth) - widest + 70) `div` 2
        litName mainCursor = (menuNames V.! mainCursor) `B.append` "_sel"

drawMenuPic :: Renderer -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
drawMenuPic renderer mainCursor yStart xOffset idx name
  | idx /= mainCursor = (renderer^.rRefExport.reDrawPic) xOffset (yStart + idx * 40 + 13) name
  | otherwise = return ()

drawPlaque :: Renderer -> Int -> Int -> Quake ()
drawPlaque renderer xOffset yStart =
  do dim <- (renderer^.rRefExport.reDrawGetPicSize) "m_main_plaque"
     maybe dimError doDrawPlaque dim
  where dimError = Com.fatalError "Menu.drawPlaque dim is Nothing"
        doDrawPlaque (w, h) =
          do (renderer^.rRefExport.reDrawPic) (xOffset - 30 - w) yStart "m_main_plaque"
             (renderer^.rRefExport.reDrawPic) (xOffset - 30 - w) (yStart + h + 5) "m_main_logo"

mainKeyF :: KeyFuncT
mainKeyF = KeyFuncT "Menu.mainKeyF" mainKey

mainKey :: Int -> Quake (Maybe B.ByteString)
mainKey key
  | key == KeyConstants.kEscape =
      do popMenu
         return Nothing
  | key `elem` [KeyConstants.kKpDownArrow, KeyConstants.kDownArrow] =
      do menuGlobals.mgMainCursor %= (\v -> if v + 1 >= mainItems then 0 else v + 1)
         return (Just menuMoveSound)
  | key `elem` [KeyConstants.kKpUpArrow, KeyConstants.kUpArrow] =
      do menuGlobals.mgMainCursor %= (\v -> if v - 1 < 0 then mainItems - 1 else v - 1)
         return (Just menuMoveSound)
  | key `elem` [KeyConstants.kKpEnter, KeyConstants.kEnter] =
      do menuGlobals.mgEnterSound .= True
         menuToOpen <- pickMenuToOpen <$> use (menuGlobals.mgMainCursor)
         maybe (return ()) runXCommandT menuToOpen
         return Nothing
  | otherwise = return Nothing
  where pickMenuToOpen idx
          | idx < 0 || idx > 4 = Nothing
          | otherwise = Just (menus V.! idx)
        menus = V.fromList [menuGameF, menuMultiplayerF, menuOptionsF, menuVideoF, menuQuitF]

gameMenuInit :: Quake ()
gameMenuInit =
  do vidDef <- use (globals.gVidDef)
     modifyRef gameMenuRef (\v -> v & mfX .~ (vidDef^.vdWidth) `div` 2
                                    & mfNItems .~ 0)
     modifyRef easyGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                          & maGeneric.mcX .~ 0
                                          & maGeneric.mcY .~ 0
                                          & maGeneric.mcName .~ Just "easy"
                                          & maGeneric.mcCallback .~ Just easyGameFunc)
     modifyRef mediumGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                            & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                            & maGeneric.mcX .~ 0
                                            & maGeneric.mcY .~ 10
                                            & maGeneric.mcName .~ Just "medium"
                                            & maGeneric.mcCallback .~ Just mediumGameFunc)
     modifyRef hardGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                          & maGeneric.mcX .~ 0
                                          & maGeneric.mcY .~ 20
                                          & maGeneric.mcName .~ Just "hard"
                                          & maGeneric.mcCallback .~ Just hardGameFunc)
     modifyRef blankLineRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator)
     modifyRef loadGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                          & maGeneric.mcX .~ 0
                                          & maGeneric.mcY .~ 40
                                          & maGeneric.mcName .~ Just "load game"
                                          & maGeneric.mcCallback .~ Just (menuLoadGameF^.xcCmd))
     modifyRef saveGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                          & maGeneric.mcX .~ 0
                                          & maGeneric.mcY .~ 50
                                          & maGeneric.mcName .~ Just "save game"
                                          & maGeneric.mcCallback .~ Just (menuSaveGameF^.xcCmd))
     modifyRef creditsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                         & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                         & maGeneric.mcX .~ 0
                                         & maGeneric.mcY .~ 60
                                         & maGeneric.mcName .~ Just "credits"
                                         & maGeneric.mcCallback .~ Just (menuCreditsF^.xcCmd))
     menuAddItem gameMenuRef (MenuActionRef easyGameActionRef)
     menuAddItem gameMenuRef (MenuActionRef mediumGameActionRef)
     menuAddItem gameMenuRef (MenuActionRef hardGameActionRef)
     menuAddItem gameMenuRef (MenuSeparatorRef blankLineRef)
     menuAddItem gameMenuRef (MenuActionRef loadGameActionRef)
     menuAddItem gameMenuRef (MenuActionRef saveGameActionRef)
     menuAddItem gameMenuRef (MenuSeparatorRef blankLineRef)
     menuAddItem gameMenuRef (MenuActionRef creditsActionRef)
     menuCenter gameMenuRef

gameMenuDrawF :: XCommandT
gameMenuDrawF = XCommandT "Menu.gameMenuDrawF" $
  do banner "m_banner_game"
     menuAdjustCursor gameMenuRef 1
     menuDraw gameMenuRef

gameMenuKeyF :: KeyFuncT
gameMenuKeyF = KeyFuncT "Menu.gameMenuKeyF" (defaultMenuKey gameMenuRef)

optionsMenuInit :: Quake ()
optionsMenuInit =
  do drivers <- S.getDriverNames
     vidDef <- use (globals.gVidDef)
     volume <- fmap (* 10) (CVar.variableValue "s_volume")
     cdnocd <- CVar.variableValue "cd_nocd"
     void (CVar.get "win_noalttab" "0" Constants.cvarArchive)
     modifyRef optionsMenuRef (\v -> v & mfX .~ (vidDef^.vdWidth) `div` 2
                                       & mfY .~ ((vidDef^.vdHeight) `div` 2) - 58
                                       & mfNItems .~ 0)
     modifyRef optionsSfxVolumeSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                & msGeneric.mcX .~ 0
                                                & msGeneric.mcY .~ 0
                                                & msGeneric.mcName .~ Just "effects volume"
                                                & msGeneric.mcCallback .~ Just updateVolumeFunc
                                                & msMinValue .~ 0
                                                & msMaxValue .~ 10
                                                & msCurValue .~ volume)
     modifyRef optionsCdVolumeBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                              & mlGeneric.mcX .~ 0
                                              & mlGeneric.mcY .~ 10
                                              & mlGeneric.mcName .~ Just "CD music"
                                              & mlGeneric.mcCallback .~ Just updateCdVolumeFunc
                                              & mlItemNames .~ cdMusicItems
                                              & mlCurValue .~ 1 - truncate cdnocd)
     modifyRef optionsQualityListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                              & mlGeneric.mcX .~ 0
                                              & mlGeneric.mcY .~ 20
                                              & mlGeneric.mcName .~ Just "sound"
                                              & mlGeneric.mcCallback .~ Just updateSoundQualityFunc
                                              & mlItemNames .~ fmap (\name -> if name == "dummy" then "off" else name) drivers)
     modifyRef optionsSensitivitySliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                    & msGeneric.mcX .~ 0
                                                    & msGeneric.mcY .~ 50
                                                    & msGeneric.mcName .~ Just "mouse speed"
                                                    & msGeneric.mcCallback .~ Just mouseSpeedFunc
                                                    & msMinValue .~ 2
                                                    & msMaxValue .~ 22)
     modifyRef optionsAlwaysRunBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                               & mlGeneric.mcX .~ 0
                                               & mlGeneric.mcY .~ 60
                                               & mlGeneric.mcName .~ Just "always run"
                                               & mlGeneric.mcCallback .~ Just alwaysRunFunc
                                               & mlItemNames .~ yesNoNames)
     modifyRef optionsInvertMouseBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                 & mlGeneric.mcX .~ 0
                                                 & mlGeneric.mcY .~ 70
                                                 & mlGeneric.mcName .~ Just "invert mouse"
                                                 & mlGeneric.mcCallback .~ Just invertMouseFunc
                                                 & mlItemNames .~ yesNoNames)
     modifyRef optionsLookSpringBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                & mlGeneric.mcX .~ 0
                                                & mlGeneric.mcY .~ 80
                                                & mlGeneric.mcName .~ Just "lookspring"
                                                & mlGeneric.mcCallback .~ Just lookSpringFunc
                                                & mlItemNames .~ yesNoNames)
     modifyRef optionsLookStrafeBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                              & mlGeneric.mcX .~ 0
                                                              & mlGeneric.mcY .~ 90
                                                              & mlGeneric.mcName .~ Just "lookstrafe"
                                                              & mlGeneric.mcCallback .~ Just lookStrafeFunc
                                                              & mlItemNames .~ yesNoNames)
     modifyRef optionsFreeLookBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                              & mlGeneric.mcX .~ 0
                                              & mlGeneric.mcY .~ 100
                                              & mlGeneric.mcName .~ Just "free look"
                                              & mlGeneric.mcCallback .~ Just freeLookFunc
                                              & mlItemNames .~ yesNoNames)
     modifyRef optionsCrosshairBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                               & mlGeneric.mcX .~ 0
                                               & mlGeneric.mcY .~ 110
                                               & mlGeneric.mcName .~ Just "crosshair"
                                               & mlGeneric.mcCallback .~ Just crosshairFunc
                                               & mlItemNames .~ yesNoNames)
     modifyRef optionsJoystickBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                              & mlGeneric.mcX .~ 0
                                              & mlGeneric.mcY .~ 120
                                              & mlGeneric.mcName .~ Just "use joystick"
                                              & mlGeneric.mcCallback .~ Just joystickFunc
                                              & mlItemNames .~ yesNoNames)
     modifyRef optionsCustomizeOptionsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                         & maGeneric.mcX .~ 0
                                                         & maGeneric.mcY .~ 140
                                                         & maGeneric.mcName .~ Just "customize controls"
                                                         & maGeneric.mcCallback .~ Just customizeControlsFunc)
     modifyRef optionsDefaultsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                 & maGeneric.mcX .~ 0
                                                 & maGeneric.mcY .~ 150
                                                 & maGeneric.mcName .~ Just "reset defaults"
                                                 & maGeneric.mcCallback .~ Just controlsResetDefaultsFunc)
     modifyRef optionsConsoleActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                & maGeneric.mcX .~ 0
                                                & maGeneric.mcY .~ 160
                                                & maGeneric.mcName .~ Just "go to console"
                                                & maGeneric.mcCallback .~ Just consoleFunc)
     controlsSetMenuItemValues
     menuAddItem optionsMenuRef (MenuSliderRef optionsSfxVolumeSliderRef)
     menuAddItem optionsMenuRef (MenuListRef optionsCdVolumeBoxRef)
     menuAddItem optionsMenuRef (MenuListRef optionsQualityListRef)
     menuAddItem optionsMenuRef (MenuSliderRef optionsSensitivitySliderRef)
     menuAddItem optionsMenuRef (MenuListRef optionsAlwaysRunBoxRef)
     menuAddItem optionsMenuRef (MenuListRef optionsInvertMouseBoxRef)
     menuAddItem optionsMenuRef (MenuListRef optionsLookSpringBoxRef)
     menuAddItem optionsMenuRef (MenuListRef optionsLookStrafeBoxRef)
     menuAddItem optionsMenuRef (MenuListRef optionsFreeLookBoxRef)
     menuAddItem optionsMenuRef (MenuListRef optionsCrosshairBoxRef)
     -- menuAddItem optionsMenuRef (MenuListRef optionsJoystickBoxRef)
     menuAddItem optionsMenuRef (MenuActionRef optionsCustomizeOptionsActionRef)
     menuAddItem optionsMenuRef (MenuActionRef optionsDefaultsActionRef)
     menuAddItem optionsMenuRef (MenuActionRef optionsConsoleActionRef)

optionsMenuDrawF :: XCommandT
optionsMenuDrawF = XCommandT "Menu.optionsMenuDrawF" $
  do banner "m_banner_options"
     menuAdjustCursor optionsMenuRef 1
     menuDraw optionsMenuRef

optionsMenuKey :: KeyFuncT
optionsMenuKey = KeyFuncT "Menu.optionsMenuKey" (defaultMenuKey optionsMenuRef)

loadGameMenuInit :: Quake ()
loadGameMenuInit =
  do vidDef <- use (globals.gVidDef)
     modifyRef loadGameMenuRef (\v -> v & mfX .~ ((vidDef^.vdWidth) `div` 2) - 120
                                        & mfY .~ ((vidDef^.vdHeight) `div` 2) - 58
                                        & mfNItems .~ 0)
     createSaveStrings
     saveStrings <- use (menuGlobals.mgSaveStrings)
     V.imapM_ (setupLoadGameMenuAction saveStrings) loadGameActions
  where setupLoadGameMenuAction saveStrings idx actionRef =
          do modifyRef actionRef (\v -> v & maGeneric.mcName .~ Just (saveStrings V.! idx)
                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                          & maGeneric.mcLocalData._x .~ idx
                                          & maGeneric.mcCallback .~ Just (loadGameCallback actionRef)
                                          & maGeneric.mcX .~ 0
                                          & maGeneric.mcY .~ (if idx > 0 then idx * 10 + 10 else idx * 10)
                                          & maGeneric.mcType .~ Constants.mtypeAction)
             menuAddItem loadGameMenuRef (MenuActionRef actionRef)

loadGameMenuDrawF :: XCommandT
loadGameMenuDrawF = XCommandT "Menu.loadGameMenuDrawF" $
  do banner "m_banner_load_game"
     menuDraw loadGameMenuRef

loadGameMenuKeyF :: KeyFuncT
loadGameMenuKeyF = KeyFuncT "Menu.loadGameMenuKeyF" loadGameMenuKey

loadGameMenuKey :: Int -> Quake (Maybe B.ByteString)
loadGameMenuKey key
  | key == KeyConstants.kEscape || key == KeyConstants.kEnter =
      do loadGameMenu <- readRef loadGameMenuRef
         modifyRef saveGameMenuRef (\v -> v & mfCursor .~ max ((loadGameMenu^.mfCursor) - 1) 0)
         defaultMenuKey loadGameMenuRef key
  | otherwise = defaultMenuKey loadGameMenuRef key

saveGameMenuInit :: Quake ()
saveGameMenuInit =
  do vidDef <- use (globals.gVidDef)
     modifyRef saveGameMenuRef (\v -> v & mfX .~ ((vidDef^.vdWidth) `div` 2) - 120
                                        & mfY .~ ((vidDef^.vdHeight) `div` 2) - 58
                                        & mfNItems .~ 0)
     createSaveStrings
     saveStrings <- use (menuGlobals.mgSaveStrings)
     -- don't include the autosave slot
     V.imapM_ (setupSaveGameMenuAction saveStrings) (V.init saveGameActions)
  where setupSaveGameMenuAction saveStrings idx actionRef =
          do modifyRef actionRef (\v -> v & maGeneric.mcName .~ Just (saveStrings V.! (idx + 1))
                                          & maGeneric.mcLocalData .~ V4 (idx + 1) 0 0 0
                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                          & maGeneric.mcCallback .~ Just (saveGameCallback actionRef)
                                          & maGeneric.mcX .~ 0
                                          & maGeneric.mcY .~ idx * 10
                                          & maGeneric.mcType .~ Constants.mtypeAction)
             menuAddItem saveGameMenuRef (MenuActionRef actionRef)

saveGameMenuDrawF :: XCommandT
saveGameMenuDrawF = XCommandT "Menu.saveGameMenuDrawF" $
  do banner "m_banner_save_game"
     menuAdjustCursor saveGameMenuRef 1
     menuDraw saveGameMenuRef

saveGameMenuKeyF :: KeyFuncT
saveGameMenuKeyF = KeyFuncT "Menu.saveGameMenuKeyF" saveGameMenuKey

saveGameMenuKey :: Int -> Quake (Maybe B.ByteString)
saveGameMenuKey key
  | key == KeyConstants.kEnter || key == KeyConstants.kEscape =
      do saveGameMenu <- readRef saveGameMenuRef
         modifyRef loadGameMenuRef (\v -> v & mfCursor .~ max ((saveGameMenu^.mfCursor) - 1) 0)
         defaultMenuKey saveGameMenuRef key
  | otherwise = defaultMenuKey saveGameMenuRef key

joinServerMenuInit :: Quake ()
joinServerMenuInit =
  do vidDef <- use (globals.gVidDef)
     modifyRef joinServerMenuRef (\v -> v & mfX .~ ((vidDef^.vdWidth) `div` 2) - 120
                                          & mfNItems .~ 0)
     modifyRef joinServerAddressBookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                       & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                       & maGeneric.mcX .~ 0
                                                       & maGeneric.mcY .~ 0
                                                       & maGeneric.mcName .~ Just "address book"
                                                       & maGeneric.mcCallback .~ Just (menuAddressBookF^.xcCmd))
     modifyRef joinServerSearchActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                  & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                  & maGeneric.mcX .~ 0
                                                  & maGeneric.mcY .~ 10
                                                  & maGeneric.mcName .~ Just "refresh server list"
                                                  & maGeneric.mcCallback .~ Just searchLocalGames
                                                  & maGeneric.mcStatusBar .~ Just "search for servers")
     modifyRef joinServerServerTitleRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator
                                                 & mspGeneric.mcName .~ Just "connect to..."
                                                 & mspGeneric.mcX .~ 80
                                                 & mspGeneric.mcY .~ 30)
     V.imapM_ setupJoinServerAction joinServerActions
     menuAddItem joinServerMenuRef (MenuActionRef joinServerAddressBookActionRef)
     menuAddItem joinServerMenuRef (MenuSeparatorRef joinServerServerTitleRef)
     menuAddItem joinServerMenuRef (MenuActionRef joinServerSearchActionRef)
     V.mapM_ addJoinServerAction joinServerActions
     menuCenter joinServerMenuRef
     searchLocalGames
  where setupJoinServerAction idx actionRef =
          do modifyRef actionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                          & maGeneric.mcX .~ 0
                                          & maGeneric.mcY .~ 40 + idx * 10
                                          & maGeneric.mcName .~ Just Constants.noServerString
                                          & maGeneric.mcCallback .~ Just (joinServerFunc actionRef)
                                          & maGeneric.mcStatusBar .~ Just "press ENTER to connect")
             menuGlobals.mgLocalServerNames.ix idx .= Constants.noServerString
        addJoinServerAction actionRef =
          menuAddItem joinServerMenuRef (MenuActionRef actionRef)

joinServerMenuDrawF :: XCommandT
joinServerMenuDrawF = XCommandT "Menu.joinServerMenuDrawF" $
  do banner "m_banner_join_server"
     menuDraw joinServerMenuRef

joinServerMenuKeyF :: KeyFuncT
joinServerMenuKeyF = KeyFuncT "Menu.joinServerMenuKeyF" (defaultMenuKey joinServerMenuRef)

addressBookMenuInit :: Quake ()
addressBookMenuInit =
  do vidDef <- use (globals.gVidDef)
     modifyRef addressBookMenuRef (\v -> v & mfX .~ ((vidDef^.vdWidth) `div` 2) - 142
                                           & mfY .~ ((vidDef^.vdHeight) `div` 2) - 58
                                           & mfNItems .~ 0)
     V.imapM_ setupAddressBookMenuAction addressBookFields
  where setupAddressBookMenuAction idx fieldRef =
          do adr <- getAddress idx
             modifyRef fieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                         & mflGeneric.mcName .~ Nothing
                                         & mflGeneric.mcCallback .~ Nothing
                                         & mflGeneric.mcX .~ 0
                                         & mflGeneric.mcY .~ idx * 18
                                         & mflGeneric.mcLocalData._x .~ idx
                                         -- put the cursor to the end of text for editing
                                         & mflCursor .~ B.length (adr^.cvString)
                                         & mflLength .~ 60
                                         & mflVisibleLength .~ 30
                                         & mflBuffer .~ (adr^.cvString))
             menuAddItem addressBookMenuRef (MenuFieldRef fieldRef)
        getAddress idx =
          do adr <- CVar.get ("adr" `B.append` encode idx) "" Constants.cvarArchive
             maybe adrError return adr
        adrError =
          do Com.fatalError "Menu.addressBookMenuInit adr is Nothing"
             return (CVarT B.empty B.empty Nothing 0 False 0)

addressBookMenuDrawF :: XCommandT
addressBookMenuDrawF = XCommandT "Menu.addressBookMenuDrawF" $
  do banner "m_banner_addressbook"
     menuDraw addressBookMenuRef

addressBookMenuKeyF :: KeyFuncT
addressBookMenuKeyF = KeyFuncT "Menu.addressBookMenuKeyF" addressBookMenuKey

addressBookMenuKey :: Int -> Quake (Maybe B.ByteString)
addressBookMenuKey key
  | key == KeyConstants.kEscape =
      do setAddressBookCVars 0 Constants.numAddressBookEntries
         defaultMenuKey addressBookMenuRef key
  | otherwise = defaultMenuKey addressBookMenuRef key

setAddressBookCVars :: Int -> Int -> Quake ()
setAddressBookCVars idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise = do
      field <- readRef fieldRef
      void (CVar.set ("adr" `B.append` encode idx) (field^.mflBuffer))
      setAddressBookCVars (idx + 1) maxIdx
  where fieldRef = addressBookFields V.! idx

startServerMenuInit :: Quake ()
startServerMenuInit =
  do loadMapNames
     vidDef <- use (globals.gVidDef)
     mapNames <- fmap (fromMaybe V.empty) (use (menuGlobals.mgMapNames))
     dev <- FS.developerSearchPath 2
     coop <- fmap (^.cvValue) coopCVar
     timeLimit <- CVar.variableString "timelimit"
     fragLimit <- CVar.variableString "fraglimit"
     -- maxclients determines the maximum number of players that can join
     -- the game. If maxclients is only "1" then we should default the menu
     -- option to 8 players, otherwise use whatever its current value is.
     -- Clamping will be done when the server is actually started.
     maxClientsValue <- CVar.variableValue "maxclients"
     maxClientsStr <- CVar.variableString "maxclients"
     hostname <- CVar.variableString "hostname"
     modifyRef startServerMenuRef (\v -> v & mfX .~ (vidDef^.vdWidth) `div` 2
                                           & mfNItems .~ 0)
     modifyRef startMapListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                        & mlGeneric.mcX .~ 0
                                        & mlGeneric.mcY .~ 0
                                        & mlGeneric.mcName .~ Just "initial map"
                                        & mlItemNames .~ mapNames)
     modifyRef rulesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                    & mlGeneric.mcX .~ 0
                                    & mlGeneric.mcY .~ 20
                                    & mlGeneric.mcName .~ Just "rules"
                                    & mlItemNames .~ (if dev == 2 then dmCoopNamesRogue else dmCoopNames)
                                    & mlCurValue .~ (if coop /= 0 then 1 else 0)
                                    & mlGeneric.mcCallback .~ Just rulesChangeFunc)
     modifyRef timeLimitFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                          & mflGeneric.mcName .~ Just "time limit"
                                          & mflGeneric.mcFlags .~ Constants.qmfNumbersOnly
                                          & mflGeneric.mcX .~ 0
                                          & mflGeneric.mcY .~ 36
                                          & mflGeneric.mcStatusBar .~ Just "0 = no limit"
                                          & mflLength .~ 3
                                          & mflVisibleLength .~ 3
                                          & mflBuffer .~ timeLimit)
     modifyRef fragLimitFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                          & mflGeneric.mcName .~ Just "frag limit"
                                          & mflGeneric.mcFlags .~ Constants.qmfNumbersOnly
                                          & mflGeneric.mcX .~ 0
                                          & mflGeneric.mcY .~ 54
                                          & mflGeneric.mcStatusBar .~ Just "0 = no limit"
                                          & mflLength .~ 3
                                          & mflVisibleLength .~ 3
                                          & mflBuffer .~ fragLimit)
     modifyRef maxClientsFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                           & mflGeneric.mcName .~ Just "max players"
                                           & mflGeneric.mcFlags .~ Constants.qmfNumbersOnly
                                           & mflGeneric.mcX .~ 0
                                           & mflGeneric.mcY .~ 72
                                           & mflGeneric.mcStatusBar .~ Nothing
                                           & mflLength .~ 3
                                           & mflVisibleLength .~ 3
                                           & mflBuffer .~ (if maxClientsValue == 1 then "8" else maxClientsStr))
     modifyRef hostnameFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                         & mflGeneric.mcName .~ Just "hostname"
                                         & mflGeneric.mcFlags .~ 0
                                         & mflGeneric.mcX .~ 0
                                         & mflGeneric.mcY .~ 90
                                         & mflGeneric.mcStatusBar .~ Nothing
                                         & mflLength .~ 12
                                         & mflVisibleLength .~ 12
                                         & mflBuffer .~ hostname
                                         & mflCursor .~ B.length hostname)
     modifyRef startServerDMOptionsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                      & maGeneric.mcName .~ Just " deathmatch flags"
                                                      & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                      & maGeneric.mcX .~ 24
                                                      & maGeneric.mcY .~ 108
                                                      & maGeneric.mcStatusBar .~ Nothing
                                                      & maGeneric.mcCallback .~ Just dmOptionsFunc)
     modifyRef startServerStartActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                  & maGeneric.mcName .~ Just " begin"
                                                  & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                  & maGeneric.mcX .~ 24
                                                  & maGeneric.mcY .~ 128
                                                  & maGeneric.mcCallback .~ Just startServerActionFunc)
     menuAddItem startServerMenuRef (MenuListRef startMapListRef)
     menuAddItem startServerMenuRef (MenuListRef rulesBoxRef)
     menuAddItem startServerMenuRef (MenuFieldRef timeLimitFieldRef)
     menuAddItem startServerMenuRef (MenuFieldRef fragLimitFieldRef)
     menuAddItem startServerMenuRef (MenuFieldRef maxClientsFieldRef)
     menuAddItem startServerMenuRef (MenuFieldRef hostnameFieldRef)
     menuAddItem startServerMenuRef (MenuActionRef startServerDMOptionsActionRef)
     menuAddItem startServerMenuRef (MenuActionRef startServerStartActionRef)
     menuCenter startServerMenuRef
     -- call this now to set proper inital state
     rulesChangeFunc

loadMapNames :: Quake ()
loadMapNames =
  do gameDir <- FS.gameDir
     contents <- readMapsList =<< Lib.fOpen (gameDir `B.append` "/maps.lst") ReadMode
     either mapNamesError applyMapNames contents
  where mapNamesError = Com.comError Constants.errDrop

applyMapNames :: B.ByteString -> Quake ()
applyMapNames buffer =
  do when (numMaps == 0) $
       Com.comError Constants.errDrop "no maps in maps.lst\n"
     mapNames <- mapM parseMapLine mapLines
     menuGlobals.mgMapNames .= Just (V.fromList mapNames)
  where mapLines = Lib.tokenise "\r\n" buffer 
        numMaps = length mapLines

parseMapLine :: B.ByteString -> Quake B.ByteString
parseMapLine mapLine =
  do (mLongName, idx) <- Com.parse mapLine (B.length mapLine) 0
     (mShortName, _) <- Com.parse mapLine (B.length mapLine) idx
     return (B.concat [fromMaybe B.empty mLongName, "\n", fromMaybe B.empty mShortName])

readMapsList :: Maybe Handle -> Quake (Either B.ByteString B.ByteString)
readMapsList Nothing =
  do buffer <- FS.loadFile "maps.lst"
     maybe (return (Left "couldn't find maps.lst\n")) (return . Right) buffer
readMapsList (Just h) =
  do buffer <- request (io (B.hGetContents h)) -- IMPROVE: exception handling
     return (Right buffer)

startServerMenuDrawF :: XCommandT
startServerMenuDrawF = XCommandT "Menu.startServerMenuDrawF" (menuDraw startServerMenuRef)

startServerMenuKeyF :: KeyFuncT
startServerMenuKeyF = KeyFuncT "Menu.startServerMenuKeyF" startServerMenuKey

startServerMenuKey :: Int -> Quake (Maybe B.ByteString)
startServerMenuKey key
  | key == KeyConstants.kEscape =
      do menuGlobals.mgMapNames .= Nothing
         defaultMenuKey startServerMenuRef key
  | otherwise = defaultMenuKey startServerMenuRef key

dmOptionsMenuInit :: Quake ()
dmOptionsMenuInit =
  do vidDef <- use (globals.gVidDef)
     dmFlags <- fmap (truncate . (^.cvValue)) dmFlagsCVar
     dev <- FS.developerSearchPath 2
     modifyRef dmOptionsMenuRef (\v -> v & mfX .~ (vidDef^.vdWidth) `div` 2
                                         & mfNItems .~ 0)
     modifyRef fallsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                    & mlGeneric.mcX .~ 0
                                    & mlGeneric.mcY .~ 0
                                    & mlGeneric.mcName .~ Just "falling damage"
                                    & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just fallsBoxRef))
                                    & mlItemNames .~ yesNoNames
                                    & mlCurValue .~ (if dmFlags .&. Constants.dfNoFalling == 0 then 1 else 0))
     modifyRef weaponsStayBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                          & mlGeneric.mcX .~ 0
                                          & mlGeneric.mcY .~ 10
                                          & mlGeneric.mcName .~ Just "weapons stay"
                                          & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just weaponsStayBoxRef))
                                          & mlItemNames .~ yesNoNames
                                          & mlCurValue .~ (if dmFlags .&. Constants.dfWeaponsStay /= 0 then 1 else 0))
     modifyRef instantPowerUpsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                              & mlGeneric.mcX .~ 0
                                              & mlGeneric.mcY .~ 20
                                              & mlGeneric.mcName .~ Just "instant powerups"
                                              & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just instantPowerUpsBoxRef))
                                              & mlItemNames .~ yesNoNames
                                              & mlCurValue .~ (if dmFlags .&. Constants.dfInstantItems /= 0 then 1 else 0))
     modifyRef powerUpsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                       & mlGeneric.mcX .~ 0
                                       & mlGeneric.mcY .~ 30
                                       & mlGeneric.mcName .~ Just "allow powerups"
                                       & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just powerUpsBoxRef))
                                       & mlItemNames .~ yesNoNames
                                       & mlCurValue .~ (if dmFlags .&. Constants.dfNoItems == 0 then 1 else 0))
     modifyRef healthBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                     & mlGeneric.mcX .~ 0
                                     & mlGeneric.mcY .~ 40
                                     & mlGeneric.mcName .~ Just "allow health"
                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just healthBoxRef))
                                     & mlItemNames .~ yesNoNames
                                     & mlCurValue .~ (if dmFlags .&. Constants.dfNoHealth == 0 then 1 else 0))
     modifyRef armorBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                    & mlGeneric.mcX .~ 0
                                    & mlGeneric.mcY .~ 50
                                    & mlGeneric.mcName .~ Just "allow armor"
                                    & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just armorBoxRef))
                                    & mlItemNames .~ yesNoNames
                                    & mlCurValue .~ (if dmFlags .&. Constants.dfNoArmor == 0 then 1 else 0))
     modifyRef spawnFarthestBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                            & mlGeneric.mcX .~ 0
                                            & mlGeneric.mcY .~ 60
                                            & mlGeneric.mcName .~ Just "spawn farthest"
                                            & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just spawnFarthestBoxRef))
                                            & mlItemNames .~ yesNoNames
                                            & mlCurValue .~ (if dmFlags .&. Constants.dfSpawnFarthest /= 0 then 1 else 0))
     modifyRef sameLevelBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                        & mlGeneric.mcX .~ 0
                                        & mlGeneric.mcY .~ 70
                                        & mlGeneric.mcName .~ Just "same map"
                                        & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just sameLevelBoxRef))
                                        & mlItemNames .~ yesNoNames
                                        & mlCurValue .~ (if dmFlags .&. Constants.dfSameLevel /= 0 then 1 else 0))
     modifyRef forceRespawnBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                           & mlGeneric.mcX .~ 0
                                           & mlGeneric.mcY .~ 80
                                           & mlGeneric.mcName .~ Just "force respawn"
                                           & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just forceRespawnBoxRef))
                                           & mlItemNames .~ yesNoNames
                                           & mlCurValue .~ (if dmFlags .&. Constants.dfForceRespawn /= 0 then 1 else 0))
     modifyRef teamPlayBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                       & mlGeneric.mcX .~ 0
                                       & mlGeneric.mcY .~ 90
                                       & mlGeneric.mcName .~ Just "teamplay"
                                       & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just teamPlayBoxRef))
                                       & mlItemNames .~ teamPlayNames)
     modifyRef allowExitBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                        & mlGeneric.mcX .~ 0
                                        & mlGeneric.mcY .~ 90
                                        & mlGeneric.mcName .~ Just "allow exit"
                                        & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just allowExitBoxRef))
                                        & mlItemNames .~ yesNoNames
                                        & mlCurValue .~ (if dmFlags .&. Constants.dfAllowExit /= 0 then 1 else 0))
     modifyRef infiniteAmmoBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                           & mlGeneric.mcX .~ 0
                                           & mlGeneric.mcY .~ 100
                                           & mlGeneric.mcName .~ Just "infinite ammo"
                                           & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just infiniteAmmoBoxRef))
                                           & mlItemNames .~ yesNoNames
                                           & mlCurValue .~ (if dmFlags .&. Constants.dfInfiniteAmmo /= 0 then 1 else 0))
     modifyRef fixedFovBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                       & mlGeneric.mcX .~ 0
                                       & mlGeneric.mcY .~ 110
                                       & mlGeneric.mcName .~ Just "fixed FOV"
                                       & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just fixedFovBoxRef))
                                       & mlItemNames .~ yesNoNames
                                       & mlCurValue .~ (if dmFlags .&. Constants.dfFixedFov /= 0 then 1 else 0))
     modifyRef quadDropBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                       & mlGeneric.mcX .~ 0
                                       & mlGeneric.mcY .~ 120
                                       & mlGeneric.mcName .~ Just "quad drop"
                                       & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just quadDropBoxRef))
                                       & mlItemNames .~ yesNoNames
                                       & mlCurValue .~ (if dmFlags .&. Constants.dfQuadDrop /= 0 then 1 else 0))
     modifyRef friendlyFireBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                           & mlGeneric.mcX .~ 0
                                           & mlGeneric.mcY .~ 130
                                           & mlGeneric.mcName .~ Just "friendly fire"
                                           & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just friendlyFireBoxRef))
                                           & mlItemNames .~ yesNoNames
                                           & mlCurValue .~ (if dmFlags .&. Constants.dfNoFriendlyFire == 0 then 1 else 0))
     when (dev == 2) $
       do modifyRef noMinesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                           & mlGeneric.mcX .~ 0
                                           & mlGeneric.mcY .~ 140
                                           & mlGeneric.mcName .~ Just "remove mines"
                                           & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noMinesBoxRef))
                                           & mlItemNames .~ yesNoNames
                                           & mlCurValue .~ (if dmFlags .&. Constants.dfNoMines /= 0 then 1 else 0))
          modifyRef noNukesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                           & mlGeneric.mcX .~ 0
                                           & mlGeneric.mcY .~ 150
                                           & mlGeneric.mcName .~ Just "remove nukes"
                                           & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noNukesBoxRef))
                                           & mlItemNames .~ yesNoNames
                                           & mlCurValue .~ (if dmFlags .&. Constants.dfNoNukes /= 0 then 1 else 0))
          modifyRef stackDoubleBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                               & mlGeneric.mcX .~ 0
                                               & mlGeneric.mcY .~ 160
                                               & mlGeneric.mcName .~ Just "2x/4x stacking off"
                                               & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just stackDoubleBoxRef))
                                               & mlItemNames .~ yesNoNames
                                               & mlCurValue .~ (dmFlags .&. Constants.dfNoStackDouble))
          modifyRef noSpheresBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                             & mlGeneric.mcX .~ 0
                                             & mlGeneric.mcY .~ 170
                                             & mlGeneric.mcName .~ Just "remove spheres"
                                             & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noSpheresBoxRef))
                                             & mlItemNames .~ yesNoNames
                                             & mlCurValue .~ (if dmFlags .&. Constants.dfNoSpheres /= 0 then 1 else 0))
     menuAddItem dmOptionsMenuRef (MenuListRef fallsBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef weaponsStayBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef instantPowerUpsBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef powerUpsBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef healthBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef armorBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef spawnFarthestBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef sameLevelBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef forceRespawnBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef teamPlayBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef allowExitBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef infiniteAmmoBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef fixedFovBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef quadDropBoxRef)
     menuAddItem dmOptionsMenuRef (MenuListRef friendlyFireBoxRef)
     when (dev == 2) $
       do menuAddItem dmOptionsMenuRef (MenuListRef noMinesBoxRef)
          menuAddItem dmOptionsMenuRef (MenuListRef noNukesBoxRef)
          menuAddItem dmOptionsMenuRef (MenuListRef stackDoubleBoxRef)
          menuAddItem dmOptionsMenuRef (MenuListRef noSpheresBoxRef)
     menuCenter dmOptionsMenuRef
     -- set the original dmflags statusbar
     dmFlagCallback Nothing
     menuSetStatusBar dmOptionsMenuRef =<< use (menuGlobals.mgDmOptionsStatusBar)

dmOptionsMenuDrawF :: XCommandT
dmOptionsMenuDrawF = XCommandT "Menu.dmOptionsMenuDrawF" (menuDraw dmOptionsMenuRef)
  
dmOptionsMenuKeyF :: KeyFuncT
dmOptionsMenuKeyF = KeyFuncT "Menu.dmOptionsMenuKey" (defaultMenuKey dmOptionsMenuRef)

multiplayerMenuInit :: Quake ()
multiplayerMenuInit =
  do vidDef <- use (globals.gVidDef)
     modifyRef multiplayerMenuRef (\v -> v & mfX .~ ((vidDef^.vdWidth) `div` 2) - 64
                                           & mfNItems .~ 0)
     modifyRef joinNetworkServerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                   & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                   & maGeneric.mcX .~ 0
                                                   & maGeneric.mcY .~ 0
                                                   & maGeneric.mcName .~ Just " join network server"
                                                   & maGeneric.mcCallback .~ Just (menuJoinServerF^.xcCmd))
     modifyRef startNetworkServerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                    & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                    & maGeneric.mcX .~ 0
                                                    & maGeneric.mcY .~ 10
                                                    & maGeneric.mcName .~ Just " start network server"
                                                    & maGeneric.mcCallback .~ Just (menuStartServerF^.xcCmd))
     modifyRef playerSetupActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                             & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                             & maGeneric.mcX .~ 0
                                             & maGeneric.mcY .~ 20
                                             & maGeneric.mcName .~ Just " player setup"
                                             & maGeneric.mcCallback .~ Just (menuPlayerConfigF^.xcCmd))
     menuAddItem multiplayerMenuRef (MenuActionRef joinNetworkServerActionRef)
     menuAddItem multiplayerMenuRef (MenuActionRef startNetworkServerActionRef)
     menuAddItem multiplayerMenuRef (MenuActionRef playerSetupActionRef)
     menuSetStatusBar multiplayerMenuRef Nothing
     menuCenter multiplayerMenuRef

multiplayerMenuDrawF :: XCommandT
multiplayerMenuDrawF = XCommandT "Menu.multiplayerMenuDrawF" $
  do banner "m_banner_multiplayer"
     menuAdjustCursor multiplayerMenuRef 1
     menuDraw multiplayerMenuRef

multiplayerMenuKey :: KeyFuncT
multiplayerMenuKey = KeyFuncT "Menu.multiplayerMenuKey" (defaultMenuKey multiplayerMenuRef)

playerConfigMenuInit :: Quake Bool
playerConfigMenuInit = error "Menu.playerConfigMenuInit" -- TODO

menuSetStatusBar :: Ref MenuFrameworkS -> Maybe B.ByteString -> Quake ()
menuSetStatusBar menuRef str = modifyRef menuRef (\v -> v & mfStatusBar .~ str)

playerConfigMenuDrawF :: XCommandT
playerConfigMenuDrawF = error "Menu.playerConfigMenuDrawF" -- TODO
  
playerConfigMenuKeyF :: KeyFuncT
playerConfigMenuKeyF = error "Menu.playerConfigMenuKeyF" -- TODO

keysMenuInit :: Quake ()
keysMenuInit =
  do vidDef <- use (globals.gVidDef)
     modifyRef keysMenuRef (\v -> v & mfX .~ (vidDef^.vdWidth) `div` 2
                                    & mfNItems .~ 0
                                    & mfCursorDraw .~ Just (keyCursorDrawFunc keysMenuRef))
     modifyRef keysAttackActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                            & maGeneric.mcX .~ 0
                                            & maGeneric.mcY .~ 0
                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysAttackActionRef)
                                            & maGeneric.mcLocalData._x .~ 0
                                            & maGeneric.mcName .~ Just ((bindNames V.! 0)^._2))
     modifyRef keysChangeWeaponActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                  & maGeneric.mcX .~ 0
                                                  & maGeneric.mcY .~ 9
                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysChangeWeaponActionRef)
                                                  & maGeneric.mcLocalData._x .~ 1
                                                  & maGeneric.mcName .~ Just ((bindNames V.! 1)^._2))
     modifyRef keysWalkForwardActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                 & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                 & maGeneric.mcX .~ 0
                                                 & maGeneric.mcY .~ 18
                                                 & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysWalkForwardActionRef)
                                                 & maGeneric.mcLocalData._x .~ 2
                                                 & maGeneric.mcName .~ Just ((bindNames V.! 2)^._2))
     modifyRef keysBackpedalActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                               & maGeneric.mcX .~ 0
                                               & maGeneric.mcY .~ 27
                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysBackpedalActionRef)
                                               & maGeneric.mcLocalData._x .~ 3
                                               & maGeneric.mcName .~ Just ((bindNames V.! 3)^._2))
     modifyRef keysTurnLeftActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                              & maGeneric.mcX .~ 0
                                              & maGeneric.mcY .~ 36 
                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysTurnLeftActionRef)
                                              & maGeneric.mcLocalData._x .~ 4
                                              & maGeneric.mcName .~ Just ((bindNames V.! 4)^._2))
     modifyRef keysTurnRightActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                               & maGeneric.mcX .~ 0
                                               & maGeneric.mcY .~ 45 
                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysTurnRightActionRef)
                                               & maGeneric.mcLocalData._x .~ 5
                                               & maGeneric.mcName .~ Just ((bindNames V.! 5)^._2))
     modifyRef keysRunActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                         & maGeneric.mcFlags .~ Constants.qmfGrayed
                                         & maGeneric.mcX .~ 0
                                         & maGeneric.mcY .~ 54 
                                         & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysRunActionRef)
                                         & maGeneric.mcLocalData._x .~ 6
                                         & maGeneric.mcName .~ Just ((bindNames V.! 6)^._2))
     modifyRef keysStepLeftActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                              & maGeneric.mcX .~ 0
                                              & maGeneric.mcY .~ 63 
                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysStepLeftActionRef)
                                              & maGeneric.mcLocalData._x .~ 7
                                              & maGeneric.mcName .~ Just ((bindNames V.! 7)^._2))
     modifyRef keysStepRightActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                               & maGeneric.mcX .~ 0
                                               & maGeneric.mcY .~ 72 
                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysStepRightActionRef)
                                               & maGeneric.mcLocalData._x .~ 8
                                               & maGeneric.mcName .~ Just ((bindNames V.! 8)^._2))
     modifyRef keysSidestepActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                              & maGeneric.mcX .~ 0
                                              & maGeneric.mcY .~ 81 
                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysSidestepActionRef)
                                              & maGeneric.mcLocalData._x .~ 9
                                              & maGeneric.mcName .~ Just ((bindNames V.! 9)^._2))
     modifyRef keysLookUpActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                            & maGeneric.mcX .~ 0
                                            & maGeneric.mcY .~ 90 
                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysLookUpActionRef)
                                            & maGeneric.mcLocalData._x .~ 10
                                            & maGeneric.mcName .~ Just ((bindNames V.! 10)^._2))
     modifyRef keysLookDownActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                              & maGeneric.mcX .~ 0
                                              & maGeneric.mcY .~ 99 
                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysLookDownActionRef)
                                              & maGeneric.mcLocalData._x .~ 11
                                              & maGeneric.mcName .~ Just ((bindNames V.! 11)^._2))
     modifyRef keysCenterViewActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                & maGeneric.mcX .~ 0
                                                & maGeneric.mcY .~ 108 
                                                & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysCenterViewActionRef)
                                                & maGeneric.mcLocalData._x .~ 12
                                                & maGeneric.mcName .~ Just ((bindNames V.! 12)^._2))
     modifyRef keysMouseLookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                               & maGeneric.mcX .~ 0
                                               & maGeneric.mcY .~ 117
                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMouseLookActionRef)
                                               & maGeneric.mcLocalData._x .~ 13
                                               & maGeneric.mcName .~ Just ((bindNames V.! 13)^._2))
     modifyRef keysKeyboardLookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                  & maGeneric.mcX .~ 0
                                                  & maGeneric.mcY .~ 126
                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysKeyboardLookActionRef)
                                                  & maGeneric.mcLocalData._x .~ 14
                                                  & maGeneric.mcName .~ Just ((bindNames V.! 14)^._2))
     modifyRef keysMoveUpActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                            & maGeneric.mcX .~ 0
                                            & maGeneric.mcY .~ 135
                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMoveUpActionRef)
                                            & maGeneric.mcLocalData._x .~ 15
                                            & maGeneric.mcName .~ Just ((bindNames V.! 15)^._2))
     modifyRef keysMoveDownActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                              & maGeneric.mcX .~ 0
                                              & maGeneric.mcY .~ 144
                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMoveDownActionRef)
                                              & maGeneric.mcLocalData._x .~ 16
                                              & maGeneric.mcName .~ Just ((bindNames V.! 16)^._2))
     modifyRef keysInventoryActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                               & maGeneric.mcX .~ 0
                                               & maGeneric.mcY .~ 153
                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInventoryActionRef)
                                               & maGeneric.mcLocalData._x .~ 17
                                               & maGeneric.mcName .~ Just ((bindNames V.! 17)^._2))
     modifyRef keysInvUseActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                            & maGeneric.mcX .~ 0
                                            & maGeneric.mcY .~ 162
                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvUseActionRef)
                                            & maGeneric.mcLocalData._x .~ 18
                                            & maGeneric.mcName .~ Just ((bindNames V.! 18)^._2))
     modifyRef keysInvDropActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                             & maGeneric.mcX .~ 0
                                             & maGeneric.mcY .~ 171
                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvDropActionRef)
                                             & maGeneric.mcLocalData._x .~ 19
                                             & maGeneric.mcName .~ Just ((bindNames V.! 19)^._2))
     modifyRef keysInvPrevActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                             & maGeneric.mcX .~ 0
                                             & maGeneric.mcY .~ 180
                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvPrevActionRef)
                                             & maGeneric.mcLocalData._x .~ 20
                                             & maGeneric.mcName .~ Just ((bindNames V.! 20)^._2))
     modifyRef keysInvNextActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                             & maGeneric.mcX .~ 0
                                             & maGeneric.mcY .~ 189
                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvNextActionRef)
                                             & maGeneric.mcLocalData._x .~ 21
                                             & maGeneric.mcName .~ Just ((bindNames V.! 21)^._2))
     modifyRef keysHelpComputerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                  & maGeneric.mcX .~ 0
                                                  & maGeneric.mcY .~ 198
                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysHelpComputerActionRef)
                                                  & maGeneric.mcLocalData._x .~ 22
                                                  & maGeneric.mcName .~ Just ((bindNames V.! 22)^._2))
     menuAddItem keysMenuRef (MenuActionRef keysAttackActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysChangeWeaponActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysWalkForwardActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysBackpedalActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysTurnLeftActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysTurnRightActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysRunActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysStepLeftActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysStepRightActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysSidestepActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysLookUpActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysLookDownActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysCenterViewActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysMouseLookActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysKeyboardLookActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysMoveUpActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysMoveDownActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysInventoryActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysInvUseActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysInvDropActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysInvPrevActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysInvNextActionRef)
     menuAddItem keysMenuRef (MenuActionRef keysHelpComputerActionRef)
     menuSetStatusBar keysMenuRef (Just "enter to change, backspace to clear")
     menuCenter keysMenuRef

downloadOptionsMenuInit :: Quake ()
downloadOptionsMenuInit =
  do vidDef <- use (globals.gVidDef)
     allowDownload <- CVar.variableValue "allow_download"
     allowDownloadMaps <- CVar.variableValue "allow_download_maps"
     allowDownloadPlayers <- CVar.variableValue "allow_download_players"
     allowDownloadModels <- CVar.variableValue "allow_download_models"
     allowDownloadSounds <- CVar.variableValue "allow_download_sounds"
     modifyRef downloadOptionsMenuRef (\v -> v & mfX .~ ((vidDef^.vdWidth) `div` 2)
                                               & mfNItems .~ 0)
     modifyRef downloadTitleRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator
                                         & mspGeneric.mcName .~ Just "Download Options"
                                         & mspGeneric.mcX .~ 48
                                         & mspGeneric.mcY .~ 0)
     modifyRef allowDownloadBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                            & mlGeneric.mcX .~ 0
                                            & mlGeneric.mcY .~ 20
                                            & mlGeneric.mcName .~ Just "allow downloading"
                                            & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadBoxRef)
                                            & mlItemNames .~ yesNoNames
                                            & mlCurValue .~ if allowDownload /= 0 then 1 else 0)
     modifyRef allowDownloadMapsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                & mlGeneric.mcX .~ 0
                                                & mlGeneric.mcY .~ 40
                                                & mlGeneric.mcName .~ Just "maps"
                                                & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadMapsBoxRef)
                                                & mlItemNames .~ yesNoNames
                                                & mlCurValue .~ if allowDownloadMaps /= 0 then 1 else 0)
     modifyRef allowDownloadPlayersBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                   & mlGeneric.mcX .~ 0
                                                   & mlGeneric.mcY .~ 50
                                                   & mlGeneric.mcName .~ Just "player models/skins"
                                                   & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadPlayersBoxRef)
                                                   & mlItemNames .~ yesNoNames
                                                   & mlCurValue .~ if allowDownloadPlayers /= 0 then 1 else 0)
     modifyRef allowDownloadModelsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                  & mlGeneric.mcX .~ 0
                                                  & mlGeneric.mcY .~ 60
                                                  & mlGeneric.mcName .~ Just "models"
                                                  & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadModelsBoxRef)
                                                  & mlItemNames .~ yesNoNames
                                                  & mlCurValue .~ if allowDownloadModels /= 0 then 1 else 0)
     modifyRef allowDownloadSoundsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                  & mlGeneric.mcX .~ 0
                                                  & mlGeneric.mcY .~ 70
                                                  & mlGeneric.mcName .~ Just "sounds"
                                                  & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadSoundsBoxRef)
                                                  & mlItemNames .~ yesNoNames
                                                  & mlCurValue .~ if allowDownloadSounds /= 0 then 1 else 0)
     menuAddItem downloadOptionsMenuRef (MenuSeparatorRef downloadTitleRef)
     menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadBoxRef)
     menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadMapsBoxRef)
     menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadPlayersBoxRef)
     menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadModelsBoxRef)
     menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadSoundsBoxRef)
     menuCenter downloadOptionsMenuRef
     -- skip over title
     modifyRef downloadOptionsMenuRef (\v -> v & mfCursor %~ (\c -> if c == 0 then 1 else c))

downloadOptionsMenuDrawF :: XCommandT
downloadOptionsMenuDrawF = XCommandT "Menu.downloadOptionsMenuDrawF" (menuDraw downloadOptionsMenuRef)
  
downloadOptionsMenuKeyF :: KeyFuncT
downloadOptionsMenuKeyF = KeyFuncT "Menu.downloadOptionsMenuKeyF" (defaultMenuKey downloadOptionsMenuRef)

quitDrawF :: XCommandT
quitDrawF = XCommandT "Menu.quitDrawF" $
  do renderer <- use (globals.gRenderer)
     vidDef <- use (globals.gVidDef)
     maybe rendererError (quitDraw vidDef) renderer
  where quitDraw vidDef renderer =
          do dim <- (renderer^.rRefExport.reDrawGetPicSize) "quit"
             maybe dimError (drawPic vidDef renderer) dim
        drawPic vidDef renderer (w, h) =
          (renderer^.rRefExport.reDrawPic) (((vidDef^.vdWidth) - w) `div` 2) (((vidDef^.vdHeight) - h) `div` 2) "quit"
        rendererError = Com.fatalError "Menu.quitDrawF renderer is Nothing"
        dimError = Com.fatalError "Menu.quitDrawF reDrawGetPicSize returned Nothing"

quitKeyF :: KeyFuncT
quitKeyF = KeyFuncT "Menu.quitKeyF" quitKey

quitKey :: Int -> Quake (Maybe B.ByteString)
quitKey key =
  do checkQuitKey
     return Nothing
  where checkQuitKey
          | key `elem` [ KeyConstants.kEscape, ord 'n', ord 'N' ] = popMenu
          | key `elem` [ ord 'Y', ord 'y' ] =
              do globals.gCls.csKeyDest .= Constants.keyConsole
                 runXCommandT CL.quitF
          | otherwise = return ()

keysMenuDrawF :: XCommandT
keysMenuDrawF = XCommandT "Menu.keysMenuDrawF" $
  do menuAdjustCursor keysMenuRef 1
     menuDraw keysMenuRef

keysMenuKeyF :: KeyFuncT
keysMenuKeyF = error "Menu.keysMenuKeyF" -- TODO

banner :: B.ByteString -> Quake ()
banner name =
  do renderer <- use (globals.gRenderer)
     vidDef <- use (globals.gVidDef)
     maybe rendererError (proceedBanner vidDef) renderer
  where proceedBanner vidDef renderer =
          do dim <- (renderer^.rRefExport.reDrawGetPicSize) name
             maybe dimError (drawPic vidDef renderer) dim
        drawPic vidDef renderer (w, _) =
          (renderer^.rRefExport.reDrawPic) ((vidDef^.vdWidth) `div` 2 - w `div` 2) ((vidDef^.vdHeight) `div` 2 - 110) name
        rendererError = Com.fatalError "Menu.banner renderer is Nothing"
        dimError = Com.fatalError "Menu.banner reDrawGetPicSize returned Nothing"

defaultMenuKey :: Ref MenuFrameworkS -> Int -> Quake (Maybe B.ByteString)
defaultMenuKey = error "Menu.defaultMenuKey" -- TODO

menuDraw :: Ref MenuFrameworkS -> Quake ()
menuDraw = error "Menu.menuDraw" -- TODO

menuAdjustCursor :: Ref MenuFrameworkS -> Int -> Quake ()
menuAdjustCursor = error "Menu.menuAdjustCursor" -- TODO

popMenu :: Quake ()
popMenu =
  do S.startLocalSound menuOutSound
     menuGlobals.mgMenuDepth -= 1
     doPopMenu =<< use (menuGlobals.mgMenuDepth)
  where doPopMenu menuDepth
          | menuDepth < 0 = Com.fatalError "PopMenu: depth < 0"
          | menuDepth > 0 =
              do layers <- use (menuGlobals.mgLayers)
                 menuGlobals.mgDrawFunc .= ((layers V.! (menuDepth - 1))^.mlDraw)
                 menuGlobals.mgKeyFunc .= ((layers V.! (menuDepth - 1))^.mlKey)
          | otherwise = forceMenuOff

forceMenuOff :: Quake ()
forceMenuOff =
  do menuGlobals %= (\v -> v & mgDrawFunc .~ Nothing
                             & mgKeyFunc .~ Nothing
                             & mgMenuDepth .~ 0)
     globals.gCls.csKeyDest .= Constants.keyGame
     Key.clearStates
     void (CVar.set "paused" "0")

draw :: Renderer -> Int -> Quake ()
draw renderer keyDest =
  when (keyDest == Constants.keyMenu) $
    do SCR.dirtyScreen
       join (liftA2 (checkCinematicTime renderer) cinematicTime vidDef)
       runDrawFunc =<< use (menuGlobals.mgDrawFunc)
       checkEnterSound =<< use (menuGlobals.mgEnterSound)
  where cinematicTime = use (globals.gCl.csCinematicTime)
        vidDef = use (globals.gVidDef)

checkCinematicTime :: Renderer -> Int -> VidDefT -> Quake ()
checkCinematicTime renderer cinematicTime vidDef
  | cinematicTime > 0 =
      (renderer^.rRefExport.reDrawFill) 0 0 (vidDef^.vdWidth) (vidDef^.vdHeight) 0
  | otherwise = renderer^.rRefExport.reDrawFadeScreen

runDrawFunc :: Maybe XCommandT -> Quake ()
runDrawFunc Nothing = Com.fatalError "Menu.runDrawFunc drawFunc is Nothing"
runDrawFunc (Just drawFunc) = runXCommandT drawFunc

checkEnterSound :: Bool -> Quake ()
checkEnterSound enterSound
  | enterSound =
      do S.startLocalSound menuInSound
         menuGlobals.mgEnterSound .= False
  | otherwise = return ()

addToServerList :: NetAdrT -> B.ByteString -> Quake ()
addToServerList adr info =
  join (liftA2 (proceedAddToServerList adr info') numServers localServerNames)
  where info' = trim info
        trim = lstrip . rstrip
        lstrip = BC.dropWhile (`BC.elem` " \t")
        rstrip = BC.reverse . lstrip . BC.reverse
        numServers = use (menuGlobals.mgNumServers)
        localServerNames = use (menuGlobals.mgLocalServerNames)

proceedAddToServerList :: NetAdrT -> B.ByteString -> Int -> V.Vector B.ByteString -> Quake ()
proceedAddToServerList adr info numServers localServerNames
  | numServers == Constants.maxLocalServers = return ()
  | info `V.elem` localServerNames = return () -- ignore if duplicated
  | otherwise =
      do modifyRef (joinServerActions V.! numServers) (\v -> v & maGeneric.mcName .~ Just info)
         menuGlobals %= (\v -> v & mgLocalServerNetAdr.ix numServers .~ adr
                                 & mgLocalServerNames.ix numServers .~ info
                                 & mgNumServers +~ 1)

easyGameFunc :: Quake ()
easyGameFunc = CVar.forceSet "skill" "0" >> startGame

mediumGameFunc :: Quake ()
mediumGameFunc = CVar.forceSet "skill" "1" >> startGame

hardGameFunc :: Quake ()
hardGameFunc = CVar.forceSet "skill" "2" >> startGame

startGame :: Quake ()
startGame =
  do globals.gCl.csServerCount .= -1
     forceMenuOff
     CVar.setValueI "deathmatch" 0
     CVar.setValueI "coop" 0
     CVar.setValueI "gamerules" 0
     CBuf.addText "loading ; killserver ; wait ; newgame\n"
     globals.gCls.csKeyDest .= Constants.keyGame

creditsMenuDrawF :: XCommandT
creditsMenuDrawF = error "Menu.creditsMenuDraw" -- TODO

creditsKeyF :: KeyFuncT
creditsKeyF = KeyFuncT "Menu.creditsKey" creditsKey

creditsKey :: Int -> Quake (Maybe B.ByteString)
creditsKey key =
  do when (key == Constants.kEscape) popMenu
     return (Just menuOutSound)

drawCursor :: Int -> Int -> Int -> Quake ()
drawCursor = error "Menu.drawCursor" -- TODO

updateVolumeFunc :: Quake ()
updateVolumeFunc =
  do slider <- readRef optionsSfxVolumeSliderRef
     CVar.setValueF "s_volume" ((slider^.msCurValue) / 10)

updateCdVolumeFunc :: Quake ()
updateCdVolumeFunc =
  do slider <- readRef optionsCdVolumeBoxRef
     CVar.setValueI "cd_nocd" (1 - (slider^.mlCurValue))

updateSoundQualityFunc :: Quake ()
updateSoundQualityFunc = error "Menu.updateSoundQualityFunc" -- TODO

mouseSpeedFunc :: Quake ()
mouseSpeedFunc =
  do slider <- readRef optionsSensitivitySliderRef
     CVar.setValueF "sensitivity" ((slider^.msCurValue) / 2)

alwaysRunFunc :: Quake ()
alwaysRunFunc =
  do box <- readRef optionsAlwaysRunBoxRef
     CVar.setValueI "cl_run" (box^.mlCurValue)

invertMouseFunc :: Quake ()
invertMouseFunc =
  do pitch <- fmap (^.cvValue) mPitchCVar
     CVar.setValueF "m_pitch" (negate pitch)

lookSpringFunc :: Quake ()
lookSpringFunc =
  do lookSpring <- fmap (^.cvValue) lookSpringCVar
     CVar.setValueF "lookspring" (1 - lookSpring)

lookStrafeFunc :: Quake ()
lookStrafeFunc =
  do lookStrafe <- fmap (^.cvValue) lookStrafeCVar
     CVar.setValueF "lookstrafe" (1 - lookStrafe)

freeLookFunc :: Quake ()
freeLookFunc =
  do box <- readRef optionsFreeLookBoxRef
     CVar.setValueI "freelook" (box^.mlCurValue)

crosshairFunc :: Quake ()
crosshairFunc =
  do box <- readRef optionsCrosshairBoxRef
     CVar.setValueI "crosshair" (box^.mlCurValue)

joystickFunc :: Quake ()
joystickFunc =
  do box <- readRef optionsJoystickBoxRef
     CVar.setValueI "in_joystick" (box^.mlCurValue)

customizeControlsFunc :: Quake ()
customizeControlsFunc = runXCommandT menuKeysF

controlsResetDefaultsFunc :: Quake ()
controlsResetDefaultsFunc =
  do CBuf.addText "exec default.cfg\n"
     CBuf.execute
     controlsSetMenuItemValues

consoleFunc :: Quake ()
consoleFunc =
  do attractLoop <- use (globals.gCl.csAttractLoop)
     proceedConsoleFunc attractLoop
  where proceedConsoleFunc True = CBuf.addText "killserver\n"
        proceedConsoleFunc False =
          do Key.clearTyping
             Console.clearNotify
             forceMenuOff
             globals.gCls.csKeyDest .= Constants.keyConsole

controlsSetMenuItemValues :: Quake ()
controlsSetMenuItemValues = error "Menu.controlsSetMenuItemValues" -- TODO

createSaveStrings :: Quake ()
createSaveStrings =
  do gameDir <- FS.gameDir
     mapM_ (findSaveGame gameDir) [0..maxSaveGames-1]

findSaveGame :: B.ByteString -> Int -> Quake ()
findSaveGame gameDir idx =
  do havePermissions <- request (io (FS.canRead nameStr))
     updateSaveGameEntry havePermissions
  where name = B.concat [gameDir, "/save/save", encode idx, "/server.ssv"]
        nameStr = BC.unpack name
        updateSaveGameEntry True =
          do qf <- QuakeFile.open name
             maybe (updateSaveGameEntry False) saveGameWithName qf
        updateSaveGameEntry False =
          menuGlobals %= (\v -> v & mgSaveStrings.ix idx .~ "<EMPTY>"
                                  & mgSaveValid.ix idx .~ False)
        saveGameWithName qf =
          do saveName <- request (io (QuakeFile.readString qf))
             maybe (updateSaveGameEntry False) setSaveGameName saveName
        setSaveGameName saveName =
          menuGlobals %= (\v -> v & mgSaveStrings.ix idx .~ saveName
                                  & mgSaveValid.ix idx .~ True)

loadGameCallback :: Ref MenuActionS -> Quake ()
loadGameCallback actionRef =
  do action <- readRef actionRef
     saveValid <- use (menuGlobals.mgSaveValid)
     when (saveValid V.! (action^.maGeneric.mcLocalData._x)) $
       CBuf.addText (B.concat ["load save", encode (action^.maGeneric.mcLocalData._x), "\n"])
     forceMenuOff

saveGameCallback :: Ref MenuActionS -> Quake ()
saveGameCallback menuActionRef =
 do action <- readRef menuActionRef
    CBuf.addText (B.concat ["save save", encode (action^.maGeneric.mcLocalData._x), "\n"])
    forceMenuOff

searchLocalGames :: Quake ()
searchLocalGames =
  do menuGlobals.mgLocalServerNames .= V.replicate Constants.maxLocalServers Constants.noServerString
     renderer <- use (globals.gRenderer)
     maybe rendererError proceedSearchLocalGames renderer
  where rendererError = Com.fatalError "Menu.searchLocalGames renderer is Nothing"
        proceedSearchLocalGames renderer =
          do drawTextBox renderer 8 (120 - 48) 36 3
             menuPrint renderer (16 + 16) (120 - 48 + 8) "Searching for local servers, this"
             menuPrint renderer (16 + 16) (120 - 48 + 16) "could take up to a minute, so"
             menuPrint renderer (16 + 16) (120 - 48 + 24) "please be patient."
             renderer^.rRefExport.reEndFrame
             runXCommandT CL.pingServersF

joinServerFunc :: Ref MenuActionS -> Quake ()
joinServerFunc actionRef =
  do action <- readRef actionRef
     localServerNames <- use (menuGlobals.mgLocalServerNames)
     numServers <- use (menuGlobals.mgNumServers)
     when (correctServer (action^.maGeneric.mcN) localServerNames numServers) $
       do localServerNetAdr <- use (menuGlobals.mgLocalServerNetAdr)
          CBuf.addText (B.concat ["connect ", NET.adrToString (localServerNetAdr V.! (action^.maGeneric.mcN)), "\n"])
          forceMenuOff
  where correctServer index localServerNames numServers =
          BC.map toLower (localServerNames V.! index) /= Constants.noServerString && index < numServers

rulesChangeFunc :: Quake ()
rulesChangeFunc = error "Menu.rulesChangeFunc" -- TODO

dmOptionsFunc :: Quake ()
dmOptionsFunc =
  do rulesBox <- readRef rulesBoxRef
     unless ((rulesBox^.mlCurValue) == 1) $
       runXCommandT menuDMOptionsF

startServerActionFunc :: Quake ()
startServerActionFunc =
  do mapNames <- use (menuGlobals.mgMapNames)
     error "Menu.startServerActionFunc" -- TODO
{-
static void StartServerActionFunc(Object self) {
        //char startmap[1024];
        String startmap;
        int timelimit;
        int fraglimit;
        int maxclients;
        String spot;

        //strcpy(startmap, strchr(mapnames[s_startmap_list.curvalue], '\n') +
        // 1);
        String x = mapnames[s_startmap_list.curvalue];

        int pos = x.indexOf('\n');
        if (pos == -1)
            startmap = x;
        else
            startmap = x.substring(pos + 1, x.length());

        maxclients = Lib.atoi(s_maxclients_field.buffer.toString());
        timelimit = Lib.atoi(s_timelimit_field.buffer.toString());
        fraglimit = Lib.atoi(s_fraglimit_field.buffer.toString());

        Cvar.SetValue("maxclients", ClampCvar(0, maxclients, maxclients));
        Cvar.SetValue("timelimit", ClampCvar(0, timelimit, timelimit));
        Cvar.SetValue("fraglimit", ClampCvar(0, fraglimit, fraglimit));
        Cvar.Set("hostname", s_hostname_field.buffer.toString());
        //		Cvar.SetValue ("deathmatch", !s_rules_box.curvalue );
        //		Cvar.SetValue ("coop", s_rules_box.curvalue );

        //	  PGM
        if ((s_rules_box.curvalue < 2) || (FS.Developer_searchpath(2) != 2)) {
            Cvar.SetValue("deathmatch", 1 - (int) (s_rules_box.curvalue));
            Cvar.SetValue("coop", s_rules_box.curvalue);
            Cvar.SetValue("gamerules", 0);
        } else {
            Cvar.SetValue("deathmatch", 1);
            // deathmatch is always true for rogue games, right?
            Cvar.SetValue("coop", 0);
            // FIXME - this might need to depend on which game we're running
            Cvar.SetValue("gamerules", s_rules_box.curvalue);
        }
        //	  PGM

        spot = null;
        if (s_rules_box.curvalue == 1) // PGM
        {
            if (Lib.Q_stricmp(startmap, "bunk1") == 0)
                spot = "start";
            else if (Lib.Q_stricmp(startmap, "mintro") == 0)
                spot = "start";
            else if (Lib.Q_stricmp(startmap, "fact1") == 0)
                spot = "start";
            else if (Lib.Q_stricmp(startmap, "power1") == 0)
                spot = "pstart";
            else if (Lib.Q_stricmp(startmap, "biggun") == 0)
                spot = "bstart";
            else if (Lib.Q_stricmp(startmap, "hangar1") == 0)
                spot = "unitstart";
            else if (Lib.Q_stricmp(startmap, "city1") == 0)
                spot = "unitstart";
            else if (Lib.Q_stricmp(startmap, "boss1") == 0)
                spot = "bosstart";
        }

        if (spot != null) {
            if (Globals.server_state != 0)
                Cbuf.AddText("disconnect\n");
            Cbuf.AddText("gamemap \"*" + startmap + "$" + spot + "\"\n");
        } else {
            Cbuf.AddText("map " + startmap + "\n");
        }

        ForceMenuOff();
    }
    -}

dmFlagCallback :: Maybe (Ref MenuListS) -> Quake ()
dmFlagCallback = error "Menu.dmFlagCallback" -- TODO

drawKeyBindingFunc :: Ref MenuActionS -> Quake ()
drawKeyBindingFunc = error "Menu.drawKeyBindingFunc" -- TODO

keyCursorDrawFunc :: Ref MenuFrameworkS -> Quake ()
keyCursorDrawFunc menuRef =
  do menu <- readRef menuRef
     bindGrab <- use (menuGlobals.mgBindGrab)
     ch <- getCh bindGrab
     renderer <- use (globals.gRenderer)
     maybe rendererError (drawChar menu ch) renderer
  where getCh True = return (ord '=')
        getCh False =
          do ms <- Timer.milliseconds
             return (12 + ((ms `div` 250) .&. 1))
        rendererError = Com.fatalError "Menu.keyCursorDrawFunc renderer is Nothing"
        drawChar menu ch renderer = 
          (renderer^.rRefExport.reDrawChar) (menu^.mfX) ((menu^.mfY) + 9 * (menu^.mfCursor)) ch

downloadCallback :: Ref MenuListS -> Quake ()
downloadCallback menuListRef =
  processItem =<< readRef menuListRef
  where processItem item
          | menuListRef == allowDownloadBoxRef =
              CVar.setValueI "allow_download" (item^.mlCurValue)
          | menuListRef == allowDownloadMapsBoxRef =
              CVar.setValueI "allow_download_maps" (item^.mlCurValue)
          | menuListRef == allowDownloadModelsBoxRef =
              CVar.setValueI "allow_download_models" (item^.mlCurValue)
          | menuListRef == allowDownloadPlayersBoxRef =
              CVar.setValueI "allow_download_players" (item^.mlCurValue)
          | menuListRef == allowDownloadSoundsBoxRef =
              CVar.setValueI "allow_download_sounds" (item^.mlCurValue)
          | otherwise = return () -- IMPROVE: or throw error?

menuPrint :: Renderer -> Int -> Int -> B.ByteString -> Quake ()
menuPrint renderer cx cy str = drawString 0 (B.length str)
  where drawString idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise =
              do drawCharacter renderer (cx + 8 * idx) cy (ord (BC.index str idx) + 128)
                 drawString (idx + 1) maxIdx

drawCharacter :: Renderer -> Int -> Int -> Int -> Quake ()
drawCharacter renderer cx cy num =
  do vidDef <- use (globals.gVidDef)
     (renderer^.rRefExport.reDrawChar) (cx + (((vidDef^.vdWidth) - 320) `shiftR` 1)) (cy + (((vidDef^.vdHeight) - 240) `shiftR` 1)) num

drawTextBox :: Renderer -> Int -> Int -> Int -> Int -> Quake ()
drawTextBox = error "Menu.drawTextBox" -- TODO
