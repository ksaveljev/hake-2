{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Menu where

import Control.Lens (zoom, use, preuse, ix, (.=), (+=), (^.), (%=), (&), (.~), (%~), (+~), (-=))
import Control.Monad (when, void, unless)
import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Maybe (fromJust)
import Linear (V4(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
import qualified Client.CL as CL
import qualified Client.Key as Key
import qualified Client.KeyConstants as KeyConstants
import {-# SOURCE #-} qualified Client.SCR as SCR
import qualified Client.VID as VID
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import qualified Sound.S as S
import qualified Sys.Timer as Timer

mainItems :: Int
mainItems = 5

numCursorFrames :: Int
numCursorFrames = 15

yesNoNames :: V.Vector B.ByteString
yesNoNames = V.fromList ["no", "yes"]

menuInSound :: B.ByteString
menuInSound = "misc/menu1.wav"

menuMoveSound :: B.ByteString
menuMoveSound = "misc/menu2.wav"

menuOutSound :: B.ByteString
menuOutSound = "misc/menu3.wav"

init :: Quake ()
init = do
    Cmd.addCommand "menu_main" (Just menuMainF)
    Cmd.addCommand "menu_game" (Just menuGameF)
    Cmd.addCommand "menu_loadgame" (Just menuLoadGameF)
    Cmd.addCommand "menu_savegame" (Just menuSaveGameF)
    Cmd.addCommand "menu_joinserver" (Just menuJoinServerF)
    Cmd.addCommand "menu_addressbook" (Just menuAddressBookF)
    Cmd.addCommand "menu_startserver" (Just menuStartServerF)
    Cmd.addCommand "menu_dmoptions" (Just menuDMOptionsF)
    Cmd.addCommand "menu_playerconfig" (Just menuPlayerConfigF)
    Cmd.addCommand "menu_downloadoptions" (Just menuDownloadOptionsF)
    Cmd.addCommand "menu_credits" (Just menuCreditsF)
    Cmd.addCommand "menu_multiplayer" (Just menuMultiplayerF)
    Cmd.addCommand "menu_video" (Just menuVideoF)
    Cmd.addCommand "menu_options" (Just menuOptionsF)
    Cmd.addCommand "menu_keys" (Just menuKeysF)
    Cmd.addCommand "menu_quit" (Just menuQuitF)

    menuGlobals.mgLayers .= V.replicate maxMenuDepth newMenuLayerT

menuAddItem :: MenuFrameworkSReference -> MenuItemReference -> Quake ()
menuAddItem menuFrameworkRef menuItemRef = do
    menu <- readMenuFrameworkSReference menuFrameworkRef
    let nItems = menu^.mfNItems

    when (nItems == 0) $
      modifyMenuFrameworkSReference menuFrameworkRef (\v -> v & mfNSlots .~ 0)

    when (nItems < Constants.maxMenuItems) $ do
      modifyMenuFrameworkSReference menuFrameworkRef (\v -> v & mfItems %~ (`V.snoc` menuItemRef))

      case menuItemRef of
        MenuListRef ref ->
          modifyMenuListSReference ref (\v -> v & mlGeneric.mcParent .~ Just menuFrameworkRef)

        MenuActionRef ref ->
          modifyMenuActionSReference ref (\v -> v & maGeneric.mcParent .~ Just menuFrameworkRef)

        MenuSliderRef ref ->
          modifyMenuSliderSReference ref (\v -> v & msGeneric.mcParent .~ Just menuFrameworkRef)

      modifyMenuFrameworkSReference menuFrameworkRef (\v -> v & mfNItems +~ 1)

    menuTallySlots menuFrameworkRef >>= \n ->
      modifyMenuFrameworkSReference menuFrameworkRef (\v -> v & mfNSlots .~ n)

menuCenter :: MenuFrameworkSReference -> Quake ()
menuCenter menuFrameworkRef = do
    menu <- readMenuFrameworkSReference menuFrameworkRef
    let menuItemRef = V.last (menu^.mfItems)
    height <- case menuItemRef of
                MenuListRef ref -> do
                  menuItem <- readMenuListSReference ref
                  return (menuItem^.mlGeneric.mcY)

                MenuSliderRef ref -> do
                  menuItem <- readMenuSliderSReference ref
                  return (menuItem^.msGeneric.mcY)

                MenuActionRef ref -> do
                  menuItem <- readMenuActionSReference ref
                  return (menuItem^.maGeneric.mcY)
    h <- use $ globals.vidDef.vdHeight

    modifyMenuFrameworkSReference menuFrameworkRef (\v -> v & mfY .~ (h - (height + 10)) `div` 2)

menuTallySlots :: MenuFrameworkSReference -> Quake Int
menuTallySlots menuFrameworkRef = do
    menu <- readMenuFrameworkSReference menuFrameworkRef

    itemsNum <- V.mapM numberOfItems (menu^.mfItems)
    return $ V.foldl' (+) 0 itemsNum

  where numberOfItems :: MenuItemReference -> Quake Int
        numberOfItems (MenuListRef ref) = do
          menuItem <- readMenuListSReference ref
          return (V.length (menuItem^.mlItemNames))
        numberOfItems _ =
          return 1 -- MenuSliderRef and MenuActionRef

pushMenu :: XCommandT -> KeyFuncT -> Quake ()
pushMenu draw key = do
    maxClients <- CVar.variableValue "maxclients"
    serverState <- use $ globals.serverState

    when (maxClients == 1 && serverState /= 0) $
      void $ CVar.set "paused" "1"

    -- if this menu is already present, drop back to that level
    -- to avoid stacking menus by hotkeys
    menuDepth <- use $ menuGlobals.mgMenuDepth
    layers <- use $ menuGlobals.mgLayers

    i <- case V.findIndex (\layer -> (layer^.mlDraw) == Just draw && (layer^.mlKey) == Just key) layers of
           Nothing ->
             return menuDepth
           Just idx -> do
             if idx >= menuDepth
               then
                 return menuDepth
               else do
                 menuGlobals.mgMenuDepth .= idx
                 return idx

    when (i == menuDepth) $ do
      when (menuDepth == maxMenuDepth) $
        Com.comError Constants.errFatal "PushMenu: MAX_MENU_DEPTH"

      zoom (menuGlobals.mgLayers.ix i) $ do
        mlDraw .= Just draw
        mlKey .= Just key

    zoom menuGlobals $ do
      mgMenuDepth += 1
      mgDrawFunc .= Just draw
      mgKeyFunc .= Just key
      mgEnterSound .= True
    
    globals.cls.csKeyDest .= Constants.keyMenu

menuMainF :: XCommandT
menuMainF = XCommandT "Menu.menuMainF" (pushMenu mainDrawF mainKeyF)

mainDrawF :: XCommandT
mainDrawF =
  XCommandT "Menu.mainDrawF" (do
    let names = V.fromList [ "m_main_game"
                           , "m_main_multiplayer"
                           , "m_main_options"
                           , "m_main_video"
                           , "m_main_quit"
                           ]
    (widest, totalHeight) <- calcWidthHeight names (-1) 0 0 (V.length names)
    vidDef' <- use $ globals.vidDef
    mainCursor <- use $ menuGlobals.mgMainCursor
    realTime <- use $ globals.cls.csRealTime

    let yStart = (vidDef'^.vdHeight) `div` 2 - 110
        xOffset = ((vidDef'^.vdWidth) - widest + 70) `div` 2

    drawMenuPics names mainCursor yStart xOffset 0 (V.length names)

    let litName = (names V.! mainCursor) `B.append` "_sel"

    Just renderer <- use $ globals.re
    (renderer^.rRefExport.reDrawPic) xOffset (yStart + mainCursor * 40 + 13) litName

    drawCursor (xOffset - 25) (yStart + mainCursor * 40 + 11) ((realTime `div` 100) `mod` numCursorFrames)

    Just (w, h) <- (renderer^.rRefExport.reDrawGetPicSize) "m_main_plaque"
    (renderer^.rRefExport.reDrawPic) (xOffset - 30 - w) yStart "m_main_plaque"
    (renderer^.rRefExport.reDrawPic) (xOffset - 30 - w) (yStart + h + 5) "m_main_logo"
  )

  where calcWidthHeight :: V.Vector B.ByteString -> Int -> Int -> Int -> Int -> Quake (Int, Int)
        calcWidthHeight names widest totalHeight idx maxIdx
          | idx >= maxIdx = return (widest, totalHeight)
          | otherwise = do
              Just renderer <- use $ globals.re
              Just (w, h) <- (renderer^.rRefExport.reDrawGetPicSize) (names V.! idx)
              let widest' = if w > widest then w else widest
              calcWidthHeight names widest' (totalHeight + (h + 12)) (idx + 1) maxIdx

        drawMenuPics :: V.Vector B.ByteString -> Int -> Int -> Int -> Int -> Int -> Quake ()
        drawMenuPics names mainCursor yStart xOffset idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              when (idx /= mainCursor) $ do
                Just renderer <- use $ globals.re
                (renderer^.rRefExport.reDrawPic) xOffset (yStart + idx * 40 + 13) (names V.! idx)

              drawMenuPics names mainCursor yStart xOffset (idx + 1) maxIdx

mainKeyF :: KeyFuncT
mainKeyF =
  KeyFuncT "Menu.mainKeyF" (\key -> do
    if | key == KeyConstants.kEscape -> do
           popMenu
           return Nothing

       | key `elem` [ KeyConstants.kKpDownArrow, KeyConstants.kDownArrow ] -> do
           menuGlobals.mgMainCursor %= (\v -> if v + 1 >= mainItems then 0 else v + 1)
           return (Just menuMoveSound)

       | key `elem` [ KeyConstants.kKpUpArrow, KeyConstants.kUpArrow ] -> do
           menuGlobals.mgMainCursor %= (\v -> if v - 1 < 0 then mainItems - 1 else v - 1)
           return (Just menuMoveSound)

       | key `elem` [ KeyConstants.kKpEnter, KeyConstants.kEnter ] -> do
           menuGlobals.mgEnterSound .= True
           mainCursor <- use $ menuGlobals.mgMainCursor

           case mainCursor of
             0 -> (menuGameF)^.xcCmd
             1 -> (menuMultiplayerF)^.xcCmd
             2 -> (menuOptionsF)^.xcCmd
             3 -> (menuVideoF)^.xcCmd
             4 -> (menuQuitF)^.xcCmd
             _ -> return ()

           return Nothing
  )

menuGameF :: XCommandT
menuGameF =
  XCommandT "Menu.menuGame" (do
    gameMenuInit
    pushMenu gameMenuDrawF gameMenuKeyF
    menuGlobals.mgGameCursor .= 1
  )

gameMenuInit :: Quake ()
gameMenuInit = do
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference gameMenuRef (\v -> v & mfX .~ truncate (fromIntegral (vidDef'^.vdWidth) * 0.50)
                                                       & mfNItems .~ 0
                                                       )

    modifyMenuActionSReference easyGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 0
                                                          & maGeneric.mcName .~ "easy"
                                                          & maGeneric.mcCallback .~ Just easyGameFunc
                                                          )

    modifyMenuActionSReference mediumGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 10
                                                            & maGeneric.mcName .~ "medium"
                                                            & maGeneric.mcCallback .~ Just mediumGameFunc
                                                            )

    modifyMenuActionSReference hardGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 20
                                                          & maGeneric.mcName .~ "hard"
                                                          & maGeneric.mcCallback .~ Just hardGameFunc
                                                          )

    modifyMenuSeparatorSReference blankLineRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator)

    modifyMenuActionSReference loadGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 40
                                                          & maGeneric.mcName .~ "load game"
                                                          & maGeneric.mcCallback .~ Just (menuLoadGameF^.xcCmd)
                                                          )

    modifyMenuActionSReference saveGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 50
                                                          & maGeneric.mcName .~ "save game"
                                                          & maGeneric.mcCallback .~ Just (menuSaveGameF^.xcCmd)
                                                          )

    modifyMenuActionSReference creditsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                         & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                         & maGeneric.mcX .~ 0
                                                         & maGeneric.mcY .~ 60
                                                         & maGeneric.mcName .~ "credits"
                                                         & maGeneric.mcCallback .~ Just (menuCreditsF^.xcCmd)
                                                         )

    menuAddItem gameMenuRef (MenuActionRef easyGameActionRef)
    menuAddItem gameMenuRef (MenuActionRef mediumGameActionRef)
    menuAddItem gameMenuRef (MenuActionRef hardGameActionRef)
    menuAddItem gameMenuRef (MenuSeparatorRef blankLineRef)
    menuAddItem gameMenuRef (MenuActionRef loadGameActionRef)
    menuAddItem gameMenuRef (MenuActionRef saveGameActionRef)
    menuAddItem gameMenuRef (MenuSeparatorRef blankLineRef)
    menuAddItem gameMenuRef (MenuActionRef creditsActionRef)

    menuCenter gameMenuRef

easyGameFunc :: Quake ()
easyGameFunc = do
    CVar.forceSet "skill" "0"
    startGame

mediumGameFunc :: Quake ()
mediumGameFunc = do
    CVar.forceSet "skill" "1"
    startGame

hardGameFunc :: Quake ()
hardGameFunc = do
    CVar.forceSet "skill" "2"
    startGame

menuLoadGameF :: XCommandT
menuLoadGameF =
  XCommandT "Menu.loadGameF" (do
    loadGameMenuInit
    pushMenu loadGameMenuDrawF loadGameMenuKeyF
  )

loadGameMenuInit :: Quake ()
loadGameMenuInit = do
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference loadGameMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 120
                                                           & mfY .~ ((vidDef'^.vdHeight) `div` 2) - 58
                                                           & mfNItems .~ 0
                                                           )

    createSaveStrings
    
    setupLoadGameMenuActions 0 maxSaveGames

    io (putStrLn "Menu.loadGameMenuInit") >> undefined -- TODO

  where setupLoadGameMenuActions :: Int -> Int -> Quake ()
        setupLoadGameMenuActions idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              io (putStrLn "Menu.loadGameMenuInit") >> undefined -- TODO

-- Search the save dir for saved games and their names.
createSaveStrings :: Quake ()
createSaveStrings = do
    io (putStrLn "Menu.createSaveStrings") >> undefined -- TODO

loadGameMenuDrawF :: XCommandT
loadGameMenuDrawF =
  XCommandT "Menu.loadGameMenuInit" (do
    banner "m_banner_load_game"
    menuDraw loadGameMenuRef
  )

loadGameMenuKeyF :: KeyFuncT
loadGameMenuKeyF =
  KeyFuncT "Menu.loadGameMenuKeyF" (\key -> do
    when (key == KeyConstants.kEscape || key == KeyConstants.kEnter) $ do
      loadGameMenu <- readMenuFrameworkSReference loadGameMenuRef
      modifyMenuFrameworkSReference saveGameMenuRef (\v -> v & mfCursor .~ if (loadGameMenu^.mfCursor) - 1 < 0 then 0 else (loadGameMenu^.mfCursor) - 1)

    defaultMenuKey loadGameMenuRef key
  )

menuSaveGameF :: XCommandT
menuSaveGameF =
  XCommandT "Menu.saveGameF" (do
    serverState' <- use $ globals.serverState
    
    unless (serverState' == 0) $ do -- only when playing a game
      saveGameMenuInit
      pushMenu saveGameMenuDraw saveGameMenuKey
  )

menuCreditsF :: XCommandT
menuCreditsF =
  XCommandT "Menu.creditsF" (do
    io (putStrLn "Menu.creditsF") >> undefined -- TODO
  )

startGame :: Quake ()
startGame = do
    -- disable updates and start the cinematic going
    globals.cl.csServerCount .= -1
    forceMenuOff
    CVar.setValueI "deathmatch" 0
    CVar.setValueI "coop" 0

    CVar.setValueI "gamerules" 0

    CBuf.addText "loading ; killserver ; wait ; newgame\n"
    globals.cls.csKeyDest .= Constants.keyGame

gameMenuDrawF :: XCommandT
gameMenuDrawF =
  XCommandT "Menu.gameMenuDrawF" (do
    banner "m_banner_game"
    menuAdjustCursor gameMenuRef 1
    menuDraw gameMenuRef
  )

gameMenuKeyF :: KeyFuncT
gameMenuKeyF =
  KeyFuncT "Menu.gameMenuKeyF" (\key -> do
    defaultMenuKey gameMenuRef key
  )

menuJoinServerF :: XCommandT
menuJoinServerF =
  XCommandT "Menu.menuJoinServer" (do
    joinServerMenuInit
    pushMenu joinServerMenuDrawF joinServerMenuKeyF
  )

joinServerMenuInit :: Quake ()
joinServerMenuInit = do
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference joinServerMenuRef (\v -> v & mfX .~ truncate (fromIntegral (vidDef'^.vdWidth) * 0.50) - 120
                                                             & mfNItems .~ 0
                                                             )

    modifyMenuActionSReference joinServerAddressBookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                       & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                       & maGeneric.mcX .~ 0
                                                                       & maGeneric.mcY .~ 0
                                                                       & maGeneric.mcName .~ "address book"
                                                                       & maGeneric.mcCallback .~ Just (menuAddressBookF^.xcCmd)
                                                                       )

    modifyMenuActionSReference joinServerSearchActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 10
                                                                  & maGeneric.mcName .~ "refresh server list"
                                                                  & maGeneric.mcCallback .~ Just searchLocalGames
                                                                  & maGeneric.mcStatusBar .~ "search for servers"
                                                                  )

    modifyMenuSeparatorSReference joinServerServerTitleRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator
                                                                    & mspGeneric.mcName .~ "connect to..."
                                                                    & mspGeneric.mcX .~ 80
                                                                    & mspGeneric.mcY .~ 30
                                                                    )

    setupJoinServerActions 0 Constants.maxLocalServers

    menuAddItem joinServerMenuRef (MenuActionRef joinServerAddressBookActionRef)
    menuAddItem joinServerMenuRef (MenuSeparatorRef joinServerServerTitleRef)
    menuAddItem joinServerMenuRef (MenuActionRef joinServerSearchActionRef)

    addJoinServerActions 0 Constants.maxLocalServers

    menuCenter joinServerMenuRef

    searchLocalGames

  where setupJoinServerActions :: Int -> Int -> Quake ()
        setupJoinServerActions idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let actionRef = joinServerActions V.! idx
              modifyMenuActionSReference actionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 40 + idx * 10
                                                            & maGeneric.mcName .~ Constants.noServerString
                                                            & maGeneric.mcCallback .~ Just (joinServerFunc actionRef)
                                                            & maGeneric.mcStatusBar .~ "press ENTER to connect"
                                                            )
              menuGlobals.mgLocalServerNames.ix idx .= Constants.noServerString
              setupJoinServerActions (idx + 1) maxIdx

        addJoinServerActions :: Int -> Int -> Quake ()
        addJoinServerActions idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              menuAddItem joinServerMenuRef (MenuActionRef (joinServerActions V.! idx))
              addJoinServerActions (idx + 1) maxIdx

joinServerMenuDrawF :: XCommandT
joinServerMenuDrawF =
  XCommandT "Menu.joinServerMenuDrawF" (do
    banner "m_banner_join_server"
    menuDraw joinServerMenuRef
  )

joinServerMenuKeyF :: KeyFuncT
joinServerMenuKeyF =
  KeyFuncT "Menu.joinServerMenuKeyF" (\key ->
    defaultMenuKey joinServerMenuRef key
  )

menuAddressBookF :: XCommandT
menuAddressBookF =
  XCommandT "Menu.menuAddressBook" (do
    addressBookMenuInit
    pushMenu addressBookMenuDrawF addressBookMenuKeyF
  )

menuStartServerF :: XCommandT
menuStartServerF =
  XCommandT "Menu.menuStartServer" (do
    startServerMenuInit
    pushMenu startServerMenuDraw startServerMenuKey
  )

menuDMOptionsF :: XCommandT
menuDMOptionsF =
  XCommandT "Menu.menuDMOptions" (do
    io (putStrLn "Menu.menuDMOptions") >> undefined -- TODO
  )

menuPlayerConfigF :: XCommandT
menuPlayerConfigF =
  XCommandT "Menu.menuPlayerConfig" (do
    ok <- playerConfigMenuInit
    
    if not ok
      then
        menuSetStatusBar multiplayerMenuRef (Just "No valid player models found")
      else do
        menuSetStatusBar multiplayerMenuRef Nothing
        pushMenu playerConfigMenuDraw playerConfigMenuKey
  )

menuDownloadOptionsF :: XCommandT
menuDownloadOptionsF =
  XCommandT "Menu.menuDownloadOptions" (do
    downloadOptionsMenuInit
    pushMenu downloadOptionsMenuDraw downloadOptionsMenuKey
  )

menuMultiplayerF :: XCommandT
menuMultiplayerF =
  XCommandT "Menu.menuMultiplayer" (do
    multiplayerMenuInit
    pushMenu multiplayerMenuDraw multiplayerMenuKey
  )

menuVideoF :: XCommandT
menuVideoF =
  XCommandT "Menu.menuVideo" (do
    VID.menuInit
    pushMenu VID.menuDraw VID.menuKey
  )

menuOptionsF :: XCommandT
menuOptionsF =
  XCommandT "Menu.menuOptions" (do
    optionsMenuInit
    pushMenu optionsMenuDraw optionsMenuKey
  )

menuKeysF :: XCommandT
menuKeysF =
  XCommandT "Menu.menuKeys" (do
    keysMenuInit
    pushMenu keysMenuDrawF keysMenuKeyF
  )

menuQuitF :: XCommandT
menuQuitF =
  XCommandT "Menu.menuQuit" (pushMenu quitDrawF quitKeyF)

quitDrawF :: XCommandT
quitDrawF =
  XCommandT "Menu.quitDrawF" (do
    Just renderer <- use $ globals.re
    vidDef' <- use $ globals.vidDef
    Just (w, h) <- (renderer^.rRefExport.reDrawGetPicSize) "quit"
    (renderer^.rRefExport.reDrawPic) (((vidDef'^.vdWidth) - w) `div` 2) (((vidDef'^.vdHeight) - h) `div` 2) "quit"
  )

quitKeyF :: KeyFuncT
quitKeyF =
  KeyFuncT "Menu.quitKeyF" (\key -> do
    if | key `elem` [ KeyConstants.kEscape, ord 'n', ord 'N' ] ->
           popMenu

       | key `elem` [ ord 'Y', ord 'y' ] -> do
           globals.cls.csKeyDest .= Constants.keyConsole
           (CL.quitF)^.xcCmd

       | otherwise ->
           return ()

    return Nothing
  )

draw :: Quake ()
draw = do
    keyDest <- use $ globals.cls.csKeyDest

    when (keyDest == Constants.keyMenu) $ do
      -- repaint everything next frame
      SCR.dirtyScreen

      -- dim everything behind it down
      cinematicTime <- use $ globals.cl.csCinematicTime
      Just renderer <- use $ globals.re
      vidDef' <- use $ globals.vidDef

      if cinematicTime > 0
        then (renderer^.rRefExport.reDrawFill) 0 0 (vidDef'^.vdWidth) (vidDef'^.vdHeight) 0
        else renderer^.rRefExport.reDrawFadeScreen

      Just drawFunc <- use $ menuGlobals.mgDrawFunc
      drawFunc^.xcCmd

      -- delay playing the enter sound until after the menu has been drawn,
      -- to avoid delay while caching images
      enterSound <- use $ menuGlobals.mgEnterSound
      when enterSound $ do
        S.startLocalSound menuInSound
        menuGlobals.mgEnterSound .= False

keyDown :: Int -> Quake ()
keyDown key = do
    keyFunc <- use $ menuGlobals.mgKeyFunc

    case keyFunc of
      Nothing ->
        return ()

      Just kf -> do
        s <- (kf^.kfFunc) key

        case s of
          Nothing -> return ()
          Just sound -> S.startLocalSound sound

addToServerList :: NetAdrT -> B.ByteString -> Quake ()
addToServerList _ _ = do
    io (putStrLn "Menu.addToServerList") >> undefined -- TODO

{-
- ============= DrawCursor =============
- 
- Draws an animating cursor with the point at x,y. The pic will extend to
- the left of x, and both above and below y.
-}
drawCursor :: Int -> Int -> Int -> Quake ()
drawCursor x y f = do
    when (f < 0) $
      Com.comError Constants.errFatal "negative time and cursor bug"

    cached <- use $ menuGlobals.mgCached

    unless cached $ do
      registerCursorPics 0 numCursorFrames
      menuGlobals.mgCached .= True

    Just renderer <- use $ globals.re
    (renderer^.rRefExport.reDrawPic) x y ("m_cursor" `B.append` BC.pack (show f))

  where registerCursorPics :: Int -> Int -> Quake ()
        registerCursorPics idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reRegisterPic) ("m_cursor" `B.append` BC.pack (show idx))
              registerCursorPics (idx + 1) maxIdx

popMenu :: Quake ()
popMenu = do
    S.startLocalSound menuOutSound
    menuGlobals.mgMenuDepth -= 1

    menuDepth <- use $ menuGlobals.mgMenuDepth

    when (menuDepth < 0) $
      Com.comError Constants.errFatal "PopMenu: depth < 0"

    when (menuDepth > 0) $ do
      layers <- use $ menuGlobals.mgLayers
      zoom menuGlobals $ do
        mgDrawFunc .= ((layers V.! (menuDepth - 1))^.mlDraw)
        mgKeyFunc .= ((layers V.! (menuDepth - 1))^.mlKey)

    when (menuDepth == 0) $
      forceMenuOff

forceMenuOff :: Quake ()
forceMenuOff = do
    zoom menuGlobals $ do
      mgDrawFunc .= Nothing
      mgKeyFunc .= Nothing
      mgMenuDepth .= 0

    globals.cls.csKeyDest .= Constants.keyGame
    Key.clearStates
    void $ CVar.set "paused" "0"

banner :: B.ByteString -> Quake ()
banner name = do
    Just renderer <- use $ globals.re
    vidDef' <- use $ globals.vidDef
    Just (w, h) <- (renderer^.rRefExport.reDrawGetPicSize) name
    (renderer^.rRefExport.reDrawPic) ((vidDef'^.vdWidth) `div` 2 - w `div` 2) ((vidDef'^.vdHeight) `div` 2 - 110) name

{-
- Menu_AdjustCursor
-
- This function takes the given menu, the direction, and attempts 
- to adjust the menu's cursor so that it's at the next available 
- slot.
-}
menuAdjustCursor :: MenuFrameworkSReference -> Int -> Quake ()
menuAdjustCursor menuRef dir = do
    menuItemRef <- menuItemAtCursor menuRef
    
    case menuItemRef of
      -- it's not in a valid spot, so crawl in the direction indicated
      -- until we find a valid spot
      Nothing -> findValidSpot
      Just (MenuSeparatorRef _) -> findValidSpot
      -- it is in a valid spot
      _ -> return ()
  
  where findValidSpot :: Quake ()
        findValidSpot = do
          menuItemRef <- menuItemAtCursor menuRef
          
          case menuItemRef of
            Nothing -> updateCursor
            Just (MenuSeparatorRef _) -> updateCursor
            _ -> return ()
            
        updateCursor :: Quake ()
        updateCursor = do
          menu <- readMenuFrameworkSReference menuRef
          let cursor = (menu^.mfCursor) + dir
              newCursor = if dir == 1
                            then if cursor >= (menu^.mfNItems) then 0 else cursor
                            else if cursor < 0 then (menu^.mfNItems) - 1 else cursor
          modifyMenuFrameworkSReference menuRef (\v -> v & mfCursor .~ newCursor)
    
menuItemAtCursor :: MenuFrameworkSReference -> Quake (Maybe MenuItemReference)
menuItemAtCursor menuRef = do
    menu <- readMenuFrameworkSReference menuRef
    
    return $ if (menu^.mfCursor) < 0 || (menu^.mfCursor) >= (menu^.mfNItems)
               then Nothing
               else Just ((menu^.mfItems) V.! (menu^.mfCursor))

menuDraw :: MenuFrameworkSReference -> Quake ()
menuDraw menuRef = do
    menu <- readMenuFrameworkSReference menuRef

    drawContents menu 0 (menu^.mfNItems)
    
    menuItemRef <- menuItemAtCursor menuRef

    mItem <- case menuItemRef of
               Nothing -> return Nothing
               Just itemRef -> menuItemCommon itemRef >>= return . Just
    
    case mItem of
      Nothing -> case menu^.mfCursorDraw of
                   Nothing -> return ()
                   Just f -> f 
      
      Just item -> case item^.mcCursorDraw of
                     Just f -> f
                     Nothing ->
                       case menu^.mfCursorDraw of
                         Just f -> f
                         Nothing ->
                           when ((item^.mcType) /= Constants.mtypeField) $ do
                             Just renderer <- use $ globals.re
                             ms <- Timer.milliseconds
                             
                             if (item^.mcFlags) .&. Constants.qmfLeftJustify /= 0
                               then
                                 (renderer^.rRefExport.reDrawChar) ((menu^.mfX) + (item^.mcX) - 24 + (item^.mcCursorOffset))
                                                                   ((menu^.mfY) + (item^.mcY))
                                                                   (12 + ((ms `div` 250) .&. 1))
                               else
                                 (renderer^.rRefExport.reDrawChar) ((menu^.mfX) + (item^.mcCursorOffset))
                                                                   ((menu^.mfY) + (item^.mcY))
                                                                   (12 + ((ms `div` 250) .&. 1))
    
    case mItem of
      Nothing -> do
        undefined -- TODO
      
      Just item ->
        undefined -- TODO

  where drawContents :: MenuFrameworkS -> Int -> Int -> Quake ()
        drawContents menu idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let itemRef = (menu^.mfItems) V.! idx
              menuCommon <- menuItemCommon itemRef

              if | (menuCommon^.mcType) == Constants.mtypeField -> do
                     let MenuFieldRef menuItemRef = itemRef
                     menuItem <- readMenuFieldSReference menuItemRef
                     fieldDraw menuItem

                 | (menuCommon^.mcType) == Constants.mtypeSlider -> do
                     let MenuSliderRef menuItemRef = itemRef
                     menuItem <- readMenuSliderSReference menuItemRef
                     sliderDraw menuItem

                 | (menuCommon^.mcType) == Constants.mtypeList -> do
                     let MenuListRef menuItemRef = itemRef
                     menuItem <- readMenuListSReference menuItemRef
                     menuListDraw menuItem

                 | (menuCommon^.mcType) == Constants.mtypeSpinControl -> do
                     let MenuListRef menuItemRef = itemRef
                     menuItem <- readMenuListSReference menuItemRef
                     spinControlDraw menuItem

                 | (menuCommon^.mcType) == Constants.mtypeAction -> do
                     let MenuActionRef menuItemRef = itemRef
                     menuItem <- readMenuActionSReference menuItemRef
                     actionDraw menuItem

                 | (menuCommon^.mcType) == Constants.mtypeSeparator -> do
                     let MenuSeparatorRef menuItemRef = itemRef
                     menuItem <- readMenuSeparatorSReference menuItemRef
                     separatorDraw menuItem

              drawContents menu (idx + 1) maxIdx

menuItemCommon :: MenuItemReference -> Quake MenuCommonS
menuItemCommon menuItemRef = do
    case menuItemRef of
      MenuListRef itemRef -> do
        menuItem <- readMenuListSReference itemRef
        return (menuItem^.mlGeneric)
      MenuActionRef itemRef -> do
        menuItem <- readMenuActionSReference itemRef
        return (menuItem^.maGeneric)
      MenuSliderRef itemRef -> do
        menuItem <- readMenuSliderSReference itemRef
        return (menuItem^.msGeneric)
      MenuSeparatorRef itemRef -> do
        menuItem <- readMenuSeparatorSReference itemRef
        return (menuItem^.mspGeneric)

defaultMenuKey :: MenuFrameworkSReference -> Int -> Quake (Maybe B.ByteString)
defaultMenuKey _ _ = do
    io (putStrLn "Menu.defaultMenuKey") >> undefined -- TODO

searchLocalGames :: Quake ()
searchLocalGames = do
    io (putStrLn "Menu.searchLocalGames") >> undefined -- TODO

joinServerFunc :: MenuActionSReference -> Quake ()
joinServerFunc _ = do
    io (putStrLn "Menu.joinServerFunc") >> undefined -- TODO

-- TODO: make sure to initialize this
{-
//	   user readable information
    //	   network address
    static {
        for (int n = 0; n < MAX_LOCAL_SERVERS; n++) {
            local_server_netadr[n] = new netadr_t();
            local_server_names[n] = "";
            s_joinserver_server_actions[n] = new menuaction_s();
            s_joinserver_server_actions[n].n = n;
        }
    }
    -}

addressBookMenuInit :: Quake ()
addressBookMenuInit = do
    io (putStrLn "Menu.addressBookMenuInit") >> undefined -- TODO

addressBookMenuDrawF :: XCommandT
addressBookMenuDrawF =
  XCommandT "Menu.addressBookMenuDrawF" (do
    banner "m_banner_addressbook"
    menuDraw addressBookMenuRef
  )

addressBookMenuKeyF :: KeyFuncT
addressBookMenuKeyF =
  KeyFuncT "Menu.addressBookMenuKeyF" (\key -> do
    when (key == KeyConstants.kEscape) $
      setAddressBookCVars 0 Constants.numAddressBookEntries

    defaultMenuKey addressBookMenuRef key
  )

  where setAddressBookCVars :: Int -> Int -> Quake ()
        setAddressBookCVars idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              CVar.set undefined undefined -- TODO
              setAddressBookCVars (idx + 1) maxIdx

startServerMenuInit :: Quake ()
startServerMenuInit = do
    io (putStrLn "Menu.startServerMenuInit") >> undefined -- TODO

startServerMenuDraw :: XCommandT
startServerMenuDraw =
  XCommandT "Menu.startServerMenuDraw" (menuDraw startServerMenuRef)

startServerMenuKey :: KeyFuncT
startServerMenuKey =
  KeyFuncT "Menu.startServerMenuKey" (\key -> do
    io (putStrLn "Menu.startServerMenuKey") >> undefined -- TODO
  )

saveGameMenuInit :: Quake ()
saveGameMenuInit = do
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference saveGameMenuRef (\v -> v & mfX .~ truncate (fromIntegral (vidDef'^.vdWidth) * 0.50) - 120
                                                           & mfY .~ truncate (fromIntegral (vidDef'^.vdHeight) * 0.50) - 58
                                                           & mfNItems .~ 0
                                                          )

    createSaveStrings

    -- don't include the autosave slot
    saveStrings <- use $ menuGlobals.mgSaveStrings
    setupSaveGameMenuActions saveStrings 0 maxSaveGames

  where setupSaveGameMenuActions :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        setupSaveGameMenuActions saveStrings idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let actionRef = saveGameActions V.! idx
              modifyMenuActionSReference actionRef (\v -> v & maGeneric.mcName .~ (saveStrings V.! (idx + 1))
                                                            & maGeneric.mcLocalData .~ V4 (idx + 1) 0 0 0
                                                            & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                            & maGeneric.mcCallback .~ Just (saveGameCallback actionRef)
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ idx * 10
                                                            & maGeneric.mcType .~ Constants.mtypeAction
                                                            )

              menuAddItem saveGameMenuRef (MenuActionRef actionRef)
              setupSaveGameMenuActions saveStrings (idx + 1) maxIdx
  
saveGameMenuDraw :: XCommandT
saveGameMenuDraw =
  XCommandT "Menu.saveGameMenuDraw" (do
    banner "m_banner_save_game"
    menuAdjustCursor saveGameMenuRef 1
    menuDraw saveGameMenuRef
  )

saveGameMenuKey :: KeyFuncT
saveGameMenuKey =
  KeyFuncT "Menu.saveGameMenuKey" (\key -> do
    when (key == KeyConstants.kEnter || key == KeyConstants.kEscape) $ do
      saveGameMenu <- readMenuFrameworkSReference saveGameMenuRef
      modifyMenuFrameworkSReference loadGameMenuRef (\v -> v & mfCursor .~ if (saveGameMenu^.mfCursor) - 1 < 0 then 0 else (saveGameMenu^.mfCursor) - 1)
      
    defaultMenuKey saveGameMenuRef key
  )
  
playerConfigMenuInit :: Quake Bool
playerConfigMenuInit = do
    io (putStrLn "Menu.playerConfigMenuInit") >> undefined -- TODO
  
menuSetStatusBar :: MenuFrameworkSReference -> Maybe B.ByteString -> Quake ()
menuSetStatusBar menuRef str = modifyMenuFrameworkSReference menuRef (\v -> v & mfStatusBar .~ str)
  
playerConfigMenuDraw :: XCommandT
playerConfigMenuDraw =
  XCommandT "Menu.playerConfigMenuDraw" (do
    io (putStrLn "Menu.playerConfigMenuDraw") >> undefined -- TODO
  )
  
playerConfigMenuKey :: KeyFuncT
playerConfigMenuKey =
  KeyFuncT "Menu.playerConfigMenuKey" (\key -> do
    io (putStrLn "Menu.playerConfigMenuKey") >> undefined -- TODO
  )
  
downloadOptionsMenuInit :: Quake ()
downloadOptionsMenuInit = do
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference downloadOptionsMenuRef (\v -> v & mfX .~ truncate (fromIntegral (vidDef'^.vdWidth) * 0.50)
                                                                  & mfNItems .~ 0
                                                                  )

    modifyMenuSeparatorSReference downloadTitleRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator
                                                            & mspGeneric.mcName .~ "Download Options"
                                                            & mspGeneric.mcX .~ 48
                                                            & mspGeneric.mcY .~ 0
                                                            )

    allowDownload <- CVar.variableValue "allow_download"

    modifyMenuListSReference allowDownloadBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                          & mlGeneric.mcX .~ 0
                                                          & mlGeneric.mcY .~ 20
                                                          & mlGeneric.mcName .~ "allow downloading"
                                                          & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadBoxRef)
                                                          & mlItemNames .~ yesNoNames
                                                          & mlCurValue .~ if allowDownload /= 0 then 1 else 0
                                                          )

    allowDownloadMaps <- CVar.variableValue "allow_download_maps"

    modifyMenuListSReference allowDownloadMapsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                              & mlGeneric.mcX .~ 0
                                                              & mlGeneric.mcY .~ 40
                                                              & mlGeneric.mcName .~ "maps"
                                                              & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadMapsBoxRef)
                                                              & mlItemNames .~ yesNoNames
                                                              & mlCurValue .~ if allowDownloadMaps /= 0 then 1 else 0
                                                              )

    allowDownloadPlayers <- CVar.variableValue "allow_download_players"

    modifyMenuListSReference allowDownloadPlayersBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                                 & mlGeneric.mcX .~ 0
                                                                 & mlGeneric.mcY .~ 50
                                                                 & mlGeneric.mcName .~ "player models/skins"
                                                                 & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadPlayersBoxRef)
                                                                 & mlItemNames .~ yesNoNames
                                                                 & mlCurValue .~ if allowDownloadPlayers /= 0 then 1 else 0
                                                                 )

    allowDownloadModels <- CVar.variableValue "allow_download_models"

    modifyMenuListSReference allowDownloadModelsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                                & mlGeneric.mcX .~ 0
                                                                & mlGeneric.mcY .~ 60
                                                                & mlGeneric.mcName .~ "models"
                                                                & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadModelsBoxRef)
                                                                & mlItemNames .~ yesNoNames
                                                                & mlCurValue .~ if allowDownloadModels /= 0 then 1 else 0
                                                                )

    allowDownloadSounds <- CVar.variableValue "allow_download_sounds"

    modifyMenuListSReference allowDownloadSoundsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                                & mlGeneric.mcX .~ 0
                                                                & mlGeneric.mcY .~ 70
                                                                & mlGeneric.mcName .~ "sounds"
                                                                & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadSoundsBoxRef)
                                                                & mlItemNames .~ yesNoNames
                                                                & mlCurValue .~ if allowDownloadSounds /= 0 then 1 else 0
                                                                )

    menuAddItem downloadOptionsMenuRef (MenuSeparatorRef downloadTitleRef)
    menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadBoxRef)
    menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadMapsBoxRef)
    menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadPlayersBoxRef)
    menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadModelsBoxRef)
    menuAddItem downloadOptionsMenuRef (MenuListRef allowDownloadSoundsBoxRef)

    menuCenter downloadOptionsMenuRef

    -- skip over title
    modifyMenuFrameworkSReference downloadOptionsMenuRef (\v -> v & mfCursor %~ (\c -> if c == 0 then 1 else c))
  
downloadOptionsMenuDraw :: XCommandT
downloadOptionsMenuDraw =
  XCommandT "Menu.downloadOptionsMenuDraw" (menuDraw downloadOptionsMenuRef)
  
downloadOptionsMenuKey :: KeyFuncT
downloadOptionsMenuKey =
  KeyFuncT "Menu.downloadOptionsMenuKey" (\key ->
    defaultMenuKey downloadOptionsMenuRef key
  )

multiplayerMenuInit :: Quake ()
multiplayerMenuInit = do
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference multiplayerMenuRef (\v -> v & mfX .~ truncate (fromIntegral (vidDef'^.vdWidth) * 0.50 - 64)
                                                              & mfNItems .~ 0
                                                              )

    modifyMenuActionSReference joinNetworkServerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                   & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                   & maGeneric.mcX .~ 0
                                                                   & maGeneric.mcY .~ 0
                                                                   & maGeneric.mcName .~ " join network server"
                                                                   & maGeneric.mcCallback .~ Just (menuJoinServerF^.xcCmd)
                                                                   )

    modifyMenuActionSReference startNetworkServerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                    & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                    & maGeneric.mcX .~ 0
                                                                    & maGeneric.mcY .~ 10
                                                                    & maGeneric.mcName .~ " start network server"
                                                                    & maGeneric.mcCallback .~ Just (menuStartServerF^.xcCmd)
                                                                    )

    modifyMenuActionSReference playerSetupActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 20
                                                             & maGeneric.mcName .~ " player setup"
                                                             & maGeneric.mcCallback .~ Just (menuPlayerConfigF^.xcCmd)
                                                             )

    menuAddItem multiplayerMenuRef (MenuActionRef joinNetworkServerActionRef)
    menuAddItem multiplayerMenuRef (MenuActionRef startNetworkServerActionRef)
    menuAddItem multiplayerMenuRef (MenuActionRef playerSetupActionRef)

    menuSetStatusBar multiplayerMenuRef Nothing

    menuCenter multiplayerMenuRef
  
multiplayerMenuDraw :: XCommandT
multiplayerMenuDraw =
  XCommandT "Menu.multiplayerMenuDraw" (do
    io (putStrLn "Menu.multiplayerMenuDraw") >> undefined -- TODO
  )

multiplayerMenuKey :: KeyFuncT
multiplayerMenuKey =
  KeyFuncT "Menu.multiplayerMenuDraw" (\key -> do
    io (putStrLn "Menu.multiplayerMenuKey") >> undefined -- TODO
  )
  
optionsMenuInit :: Quake ()
optionsMenuInit = do
    io (putStrLn "Menu.optionsMenuInit") >> undefined -- TODO

optionsMenuDraw :: XCommandT
optionsMenuDraw =
  XCommandT "Menu.optionsMenuDraw" (do
    io (putStrLn "Menu.optionsMenuDraw") >> undefined -- TODO
  )

optionsMenuKey :: KeyFuncT
optionsMenuKey =
  KeyFuncT "Menu.optionsMenuKey" (\key -> do
    io (putStrLn "Menu.optionsMenuKey") >> undefined -- TODO
  )
  
keysMenuInit :: Quake ()
keysMenuInit = do
    io (putStrLn "Menu.keysMenuInit") >> undefined -- TODO

keysMenuDrawF :: XCommandT
keysMenuDrawF =
  XCommandT "Menu.keysMenuDrawF" (do
    io (putStrLn "Menu.keysMenuDrawF") >> undefined -- TODO
  )

keysMenuKeyF :: KeyFuncT
keysMenuKeyF =
  KeyFuncT "Menu.keysMenuKeyF" (\key -> do
    io (putStrLn "Menu.keysMenuKeyF") >> undefined -- TODO
  )

saveGameCallback :: MenuActionSReference -> Quake ()
saveGameCallback _ = do
    io (putStrLn "Menu.saveGameCallback") >> undefined -- TODO

fieldDraw :: MenuFieldS -> Quake ()
fieldDraw _ = do
    io (putStrLn "Menu.fieldDraw") >> undefined -- TODO

sliderDraw :: MenuSliderS -> Quake ()
sliderDraw _ = do
    io (putStrLn "Menu.sliderDraw") >> undefined -- TODO

menuListDraw :: MenuListS -> Quake ()
menuListDraw _ = do
    io (putStrLn "Menu.menuListDraw") >> undefined -- TODO

spinControlDraw :: MenuListS -> Quake ()
spinControlDraw _ = do
    io (putStrLn "Menu.spinControlDraw") >> undefined -- TODO

actionDraw :: MenuActionS -> Quake ()
actionDraw _ = do
    io (putStrLn "Menu.actionDraw") >> undefined -- TODO

separatorDraw :: MenuSeparatorS -> Quake ()
separatorDraw _ = do
    io (putStrLn "Menu.separatorDraw") >> undefined -- TODO

downloadCallback :: MenuListSReference -> Quake ()
downloadCallback _ = do
    io (putStrLn "Menu.downloadCallback") >> undefined -- TODO
