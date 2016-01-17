{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Menu where

import Control.Lens (zoom, use, preuse, ix, (.=), (+=), (^.), (%=), (&), (.~), (%~), (+~), (-=), _1, _2)
import Control.Monad (when, void, unless, liftM)
import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Maybe (fromJust)
import Linear (V4(..), _x)
import System.Directory (doesFileExist, readable, getPermissions)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
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
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Sound.S as S
import qualified Sys.Timer as Timer

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

    modifyMenuFrameworkSReference gameMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
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

  where setupLoadGameMenuActions :: Int -> Int -> Quake ()
        setupLoadGameMenuActions idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let actionRef = loadGameActions V.! idx
              
              saveStrings <- use $ menuGlobals.mgSaveStrings
              
              modifyMenuActionSReference actionRef (\v -> v & maGeneric.mcName .~ saveStrings V.! idx
                                                            & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                            & maGeneric.mcLocalData._x .~ idx
                                                            & maGeneric.mcCallback .~ Just (loadGameCallback actionRef)
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ (if idx > 0 then idx * 10 + 10 else idx * 10)
                                                            & maGeneric.mcType .~ Constants.mtypeAction
                                                            )
              
              menuAddItem loadGameMenuRef (MenuActionRef actionRef)
              setupLoadGameMenuActions (idx + 1) maxIdx

-- Search the save dir for saved games and their names.
createSaveStrings :: Quake ()
createSaveStrings = do
    gameDir <- FS.gameDir
    findSaveGames gameDir 0 maxSaveGames
    
  where findSaveGames :: B.ByteString -> Int -> Int -> Quake ()
        findSaveGames gameDir idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let name = gameDir `B.append` "/save/save" `B.append` (BC.pack (show idx)) `B.append` "/server.ssv" -- IMPROVE
                  nameUnpacked = BC.unpack name
              
              fileExists <- io $ doesFileExist nameUnpacked
              canRead <- if fileExists
                           then io $ liftM readable (getPermissions nameUnpacked)
                           else return False
              if canRead
                then
                  zoom menuGlobals $ do
                    mgSaveStrings.ix idx .= name
                    mgSaveValid.ix idx .= True
                else
                  zoom menuGlobals $ do
                    mgSaveStrings.ix idx .= "<EMPTY>"
                    mgSaveValid.ix idx .= False
              
              findSaveGames gameDir (idx + 1) maxIdx

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
    b <- FS.loadFile "credits"
    
    credits <- case b of
                 Nothing -> do
                   isDeveloper <- FS.developerSearchPath 1
                   
                   return $ case isDeveloper of
                              1 -> xatCredits
                              2 -> rogueCredits
                              _ -> idCredits
                              
                 Just contents ->
                   return (V.fromList (tokenise "\r\n" contents))
                   
    realTime <- use $ globals.cls.csRealTime
    
    zoom menuGlobals $ do
      mgCreditsStartTime .= realTime
      mgCredits .= credits
    
    pushMenu creditsMenuDraw creditsKey
  )
  
  where tokenise :: B.ByteString -> B.ByteString -> [B.ByteString]
        tokenise x y = let (h, t) = B.breakSubstring x y
                       in h : if B.null t then [] else tokenise x (B.drop (B.length x) t)

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

    modifyMenuFrameworkSReference joinServerMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 120
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
    dmOptionsMenuInit
    pushMenu dmOptionsMenuDraw dmOptionsMenuKey
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
addToServerList adr info = do
    numServers <- use $ menuGlobals.mgNumServers
    
    unless (numServers == Constants.maxLocalServers) $ do
      let info' = trim info
      localServerNames <- use $ menuGlobals.mgLocalServerNames
      
      -- ignore if duplicated
      unless (info' `V.elem` localServerNames) $ do
        let actionRef = joinServerActions V.! numServers
        modifyMenuActionSReference actionRef (\v -> v & maGeneric.mcName .~ info')
        
        zoom menuGlobals $ do
          mgLocalServerNetAdr.ix numServers .= adr
          mgLocalServerNames.ix numServers .= info'
          mgNumServers += 1

  where trim :: B.ByteString -> B.ByteString
        trim = lstrip . rstrip
        
        lstrip :: B.ByteString -> B.ByteString
        lstrip = BC.dropWhile (`BC.elem` " \t")
        
        rstrip :: B.ByteString -> B.ByteString
        rstrip = BC.reverse . lstrip . BC.reverse

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
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference addressBookMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 142
                                                              & mfY .~ ((vidDef'^.vdHeight) `div` 2) - 58
                                                              & mfNItems .~ 0
                                                              )
                                                              
    setupAddressBookMenuActions 0 Constants.numAddressBookEntries
    
  where setupAddressBookMenuActions :: Int -> Int -> Quake ()
        setupAddressBookMenuActions idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              adr <- CVar.get ("adr" `B.append` (BC.pack (show idx))) "" Constants.cvarArchive
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

    modifyMenuFrameworkSReference saveGameMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 120
                                                           & mfY .~ ((vidDef'^.vdHeight) `div` 2) - 58
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

    modifyMenuFrameworkSReference downloadOptionsMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2)
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

    modifyMenuFrameworkSReference multiplayerMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 64
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
    banner "m_banner_multiplayer"
    menuAdjustCursor multiplayerMenuRef 1
    menuDraw multiplayerMenuRef
  )

multiplayerMenuKey :: KeyFuncT
multiplayerMenuKey =
  KeyFuncT "Menu.multiplayerMenuDraw" (\key ->
    defaultMenuKey multiplayerMenuRef key
  )
  
optionsMenuInit :: Quake ()
optionsMenuInit = do
    drivers <- S.getDriverNames
    let labels = fmap (\driverName -> if driverName == "dummy" then "off" else driverName) drivers
    
    winNoAltTab <- CVar.get "win_noalttab" "0" Constants.cvarArchive
    
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference optionsMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                          & mfY .~ ((vidDef'^.vdHeight) `div` 2) - 58
                                                          & mfNItems .~ 0
                                                          )
    
    volumeValue <- liftM (* 10) (CVar.variableValue "s_volume")
    
    modifyMenuSliderSReference optionsSfxVolumeSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                                  & msGeneric.mcX .~ 0
                                                                  & msGeneric.mcY .~ 0
                                                                  & msGeneric.mcName .~ "effects volume"
                                                                  & msGeneric.mcCallback .~ Just updateVolumeFunc
                                                                  & msMinValue .~ 0
                                                                  & msMaxValue .~ 10
                                                                  & msCurValue .~ volumeValue
                                                                  )
    
    cdnocd <- CVar.variableValue "cd_nocd"
    
    modifyMenuListSReference optionsCdVolumeBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 10
                                                            & mlGeneric.mcName .~ "CD music"
                                                            & mlGeneric.mcCallback .~ Just updateCdVolumeFunc
                                                            & mlItemNames .~ cdMusicItems
                                                            & mlCurValue .~ 1 - truncate cdnocd
                                                            )
                                                            
    modifyMenuListSReference optionsQualityListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 20
                                                            & mlGeneric.mcName .~ "sound"
                                                            & mlGeneric.mcCallback .~ Just updateSoundQualityFunc
                                                            & mlItemNames .~ labels
                                                            )
                                                         
    modifyMenuSliderSReference optionsSensitivitySliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                                    & msGeneric.mcX .~ 0
                                                                    & msGeneric.mcY .~ 50
                                                                    & msGeneric.mcName .~ "mouse speed"
                                                                    & msGeneric.mcCallback .~ Just mouseSpeedFunc
                                                                    & msMinValue .~ 2
                                                                    & msMaxValue .~ 22
                                                                    )
                                                                    
    modifyMenuListSReference optionsAlwaysRunBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                             & mlGeneric.mcX .~ 0
                                                             & mlGeneric.mcY .~ 60
                                                             & mlGeneric.mcName .~ "always run"
                                                             & mlGeneric.mcCallback .~ Just alwaysRunFunc
                                                             & mlItemNames .~ yesNoNames
                                                             )
                                                             
    modifyMenuListSReference optionsInvertMouseBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                               & mlGeneric.mcX .~ 0
                                                               & mlGeneric.mcY .~ 70
                                                               & mlGeneric.mcName .~ "invert mouse"
                                                               & mlGeneric.mcCallback .~ Just invertMouseFunc
                                                               & mlItemNames .~ yesNoNames
                                                               )
                                                               
    modifyMenuListSReference optionsLookSpringBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                             & mlGeneric.mcX .~ 0
                                                             & mlGeneric.mcY .~ 80
                                                             & mlGeneric.mcName .~ "lookspring"
                                                             & mlGeneric.mcCallback .~ Just lookSpringFunc
                                                             & mlItemNames .~ yesNoNames
                                                             )
                                                             
    modifyMenuListSReference optionsLookStrafeBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                              & mlGeneric.mcX .~ 0
                                                              & mlGeneric.mcY .~ 90
                                                              & mlGeneric.mcName .~ "lookstrafe"
                                                              & mlGeneric.mcCallback .~ Just lookStrafeFunc
                                                              & mlItemNames .~ yesNoNames
                                                              )
                                                              
    modifyMenuListSReference optionsFreeLookBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 100
                                                            & mlGeneric.mcName .~ "free look"
                                                            & mlGeneric.mcCallback .~ Just freeLookFunc
                                                            & mlItemNames .~ yesNoNames
                                                            )
                                                            
    modifyMenuListSReference optionsCrosshairBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                             & mlGeneric.mcX .~ 0
                                                             & mlGeneric.mcY .~ 110
                                                             & mlGeneric.mcName .~ "crosshair"
                                                             & mlGeneric.mcCallback .~ Just crosshairFunc
                                                             & mlItemNames .~ yesNoNames
                                                             )
                                                             
    modifyMenuListSReference optionsJoystickBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 120
                                                            & mlGeneric.mcName .~ "use joystick"
                                                            & mlGeneric.mcCallback .~ Just joystickFunc
                                                            & mlItemNames .~ yesNoNames
                                                            )
                                                            
    modifyMenuActionSReference optionsCustomizeOptionsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                         & maGeneric.mcX .~ 0
                                                                         & maGeneric.mcY .~ 140
                                                                         & maGeneric.mcName .~ "customize controls"
                                                                         & maGeneric.mcCallback .~ Just customizeControlsFunc
                                                                         )
                                                                         
    modifyMenuActionSReference optionsDefaultsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                 & maGeneric.mcX .~ 0
                                                                 & maGeneric.mcY .~ 150
                                                                 & maGeneric.mcName .~ "reset defaults"
                                                                 & maGeneric.mcCallback .~ Just controlsResetDefaultsFunc
                                                                 )
                                                                 
    modifyMenuActionSReference optionsConsoleActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                & maGeneric.mcX .~ 0
                                                                & maGeneric.mcY .~ 160
                                                                & maGeneric.mcName .~ "go to console"
                                                                & maGeneric.mcCallback .~ Just consoleFunc
                                                                )
    
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

optionsMenuDraw :: XCommandT
optionsMenuDraw =
  XCommandT "Menu.optionsMenuDraw" (do
    banner "m_banner_options"
    menuAdjustCursor optionsMenuRef 1
    menuDraw optionsMenuRef
  )

optionsMenuKey :: KeyFuncT
optionsMenuKey =
  KeyFuncT "Menu.optionsMenuKey" (\key ->
    defaultMenuKey optionsMenuRef key
  )
  
keysMenuInit :: Quake ()
keysMenuInit = do
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference keysMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                       & mfNItems .~ 0
                                                       & mfCursorDraw .~ Just (keyCursorDrawFunc keysMenuRef)
                                                       )
    
    modifyMenuActionSReference keysAttackActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 0
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysAttackActionRef)
                                                            & maGeneric.mcLocalData._x .~ 0
                                                            & maGeneric.mcName .~ (bindNames V.! 0)^._2
                                                            )
                                                            
    modifyMenuActionSReference keysChangeWeaponActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 9
                                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysChangeWeaponActionRef)
                                                                  & maGeneric.mcLocalData._x .~ 1
                                                                  & maGeneric.mcName .~ (bindNames V.! 1)^._2
                                                                  )
                                                                  
    modifyMenuActionSReference keysWalkForwardActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                 & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                 & maGeneric.mcX .~ 0
                                                                 & maGeneric.mcY .~ 18
                                                                 & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysWalkForwardActionRef)
                                                                 & maGeneric.mcLocalData._x .~ 2
                                                                 & maGeneric.mcName .~ (bindNames V.! 2)^._2
                                                                 )
                                                                 
    modifyMenuActionSReference keysBackpedalActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 27
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysBackpedalActionRef)
                                                               & maGeneric.mcLocalData._x .~ 3
                                                               & maGeneric.mcName .~ (bindNames V.! 3)^._2
                                                               )
                                                               
    modifyMenuActionSReference keysTurnLeftActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 36 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysTurnLeftActionRef)
                                                              & maGeneric.mcLocalData._x .~ 4
                                                              & maGeneric.mcName .~ (bindNames V.! 4)^._2
                                                              )
                                                              
    modifyMenuActionSReference keysTurnRightActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 45 
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysTurnRightActionRef)
                                                               & maGeneric.mcLocalData._x .~ 5
                                                               & maGeneric.mcName .~ (bindNames V.! 5)^._2
                                                               )
                                                               
    modifyMenuActionSReference keysRunActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                         & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                         & maGeneric.mcX .~ 0
                                                         & maGeneric.mcY .~ 54 
                                                         & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysRunActionRef)
                                                         & maGeneric.mcLocalData._x .~ 6
                                                         & maGeneric.mcName .~ (bindNames V.! 6)^._2
                                                         )
                                                         
    modifyMenuActionSReference keysStepLeftActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 63 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysStepLeftActionRef)
                                                              & maGeneric.mcLocalData._x .~ 7
                                                              & maGeneric.mcName .~ (bindNames V.! 7)^._2
                                                              )
                                                              
    modifyMenuActionSReference keysStepRightActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 72 
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysStepRightActionRef)
                                                               & maGeneric.mcLocalData._x .~ 8
                                                               & maGeneric.mcName .~ (bindNames V.! 8)^._2
                                                               )
                                                               
    modifyMenuActionSReference keysSidestepActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 81 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysSidestepActionRef)
                                                              & maGeneric.mcLocalData._x .~ 9
                                                              & maGeneric.mcName .~ (bindNames V.! 9)^._2
                                                              )
                                                              
    modifyMenuActionSReference keysLookUpActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 90 
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysLookUpActionRef)
                                                            & maGeneric.mcLocalData._x .~ 10
                                                            & maGeneric.mcName .~ (bindNames V.! 10)^._2
                                                            )
                                                            
    modifyMenuActionSReference keysLookDownActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 99 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysLookDownActionRef)
                                                              & maGeneric.mcLocalData._x .~ 11
                                                              & maGeneric.mcName .~ (bindNames V.! 11)^._2
                                                              )
                                                              
    modifyMenuActionSReference keysCenterViewActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                & maGeneric.mcX .~ 0
                                                                & maGeneric.mcY .~ 108 
                                                                & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysCenterViewActionRef)
                                                                & maGeneric.mcLocalData._x .~ 12
                                                                & maGeneric.mcName .~ (bindNames V.! 12)^._2
                                                                )
                                                              
    modifyMenuActionSReference keysMouseLookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 117
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMouseLookActionRef)
                                                               & maGeneric.mcLocalData._x .~ 13
                                                               & maGeneric.mcName .~ (bindNames V.! 13)^._2
                                                               )
                                                               
    modifyMenuActionSReference keysKeyboardLookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 126
                                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysKeyboardLookActionRef)
                                                                  & maGeneric.mcLocalData._x .~ 14
                                                                  & maGeneric.mcName .~ (bindNames V.! 14)^._2
                                                                  )
                                                                  
    modifyMenuActionSReference keysMoveUpActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 135
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMoveUpActionRef)
                                                            & maGeneric.mcLocalData._x .~ 15
                                                            & maGeneric.mcName .~ (bindNames V.! 15)^._2
                                                            )
                                                            
    modifyMenuActionSReference keysMoveDownActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 144
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMoveDownActionRef)
                                                              & maGeneric.mcLocalData._x .~ 16
                                                              & maGeneric.mcName .~ (bindNames V.! 16)^._2
                                                              )
                                                              
    modifyMenuActionSReference keysInventoryActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 153
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInventoryActionRef)
                                                               & maGeneric.mcLocalData._x .~ 17
                                                               & maGeneric.mcName .~ (bindNames V.! 17)^._2
                                                               )
                                                               
    modifyMenuActionSReference keysInvUseActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 162
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvUseActionRef)
                                                            & maGeneric.mcLocalData._x .~ 18
                                                            & maGeneric.mcName .~ (bindNames V.! 18)^._2
                                                            )
                                                            
    modifyMenuActionSReference keysInvDropActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 171
                                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvDropActionRef)
                                                             & maGeneric.mcLocalData._x .~ 19
                                                             & maGeneric.mcName .~ (bindNames V.! 19)^._2
                                                             )
                                                             
    modifyMenuActionSReference keysInvPrevActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 180
                                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvPrevActionRef)
                                                             & maGeneric.mcLocalData._x .~ 20
                                                             & maGeneric.mcName .~ (bindNames V.! 20)^._2
                                                             )
                                                             
    modifyMenuActionSReference keysInvNextActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 189
                                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvNextActionRef)
                                                             & maGeneric.mcLocalData._x .~ 21
                                                             & maGeneric.mcName .~ (bindNames V.! 21)^._2
                                                             )
                                                             
    modifyMenuActionSReference keysHelpComputerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 198
                                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysHelpComputerActionRef)
                                                                  & maGeneric.mcLocalData._x .~ 22
                                                                  & maGeneric.mcName .~ (bindNames V.! 22)^._2
                                                                  )

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

keysMenuDrawF :: XCommandT
keysMenuDrawF =
  XCommandT "Menu.keysMenuDrawF" (do
    menuAdjustCursor keysMenuRef 1
    menuDraw keysMenuRef
  )

keysMenuKeyF :: KeyFuncT
keysMenuKeyF =
  KeyFuncT "Menu.keysMenuKeyF" (\key -> do
    Just (MenuActionRef actionItemRef) <- menuItemAtCursor keysMenuRef
    item <- readMenuActionSReference actionItemRef
    bindGrab <- use $ menuGlobals.mgBindGrab

    if bindGrab
      then do
        when (key /= KeyConstants.kEscape && key /= ord '`') $ do
          keyName <- Key.keynumToString key
          let cmd = "bind \"" `B.append` keyName
                              `B.append` "\" \""
                              `B.append` ((bindNames V.! (item^.maGeneric.mcLocalData._x))^._1)
                              `B.append` "\""
          CBuf.insertText cmd

        menuSetStatusBar keysMenuRef (Just "enter to change, backspace to clear")
        menuGlobals.mgBindGrab .= False
        return (Just menuOutSound)

      else do
        if | key `elem` [KeyConstants.kKpEnter, KeyConstants.kEnter] -> do
               keyBindingFunc actionItemRef
               return (Just menuInSound)

           | key `elem` [KeyConstants.kBackspace, KeyConstants.kDel, KeyConstants.kKpDel] -> do
               unbindCommand ((bindNames V.! (item^.maGeneric.mcLocalData._x))^._1)
               return (Just menuOutSound)

           | otherwise ->
               defaultMenuKey keysMenuRef key
  )
  
dmOptionsMenuInit :: Quake ()
dmOptionsMenuInit = do
    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
    vidDef' <- use $ globals.vidDef

    modifyMenuFrameworkSReference dmOptionsMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                            & mfNItems .~ 0
                                                            )

    modifyMenuListSReference fallsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                  & mlGeneric.mcX .~ 0
                                                  & mlGeneric.mcY .~ 0
                                                  & mlGeneric.mcName .~ "falling damage"
                                                  & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just fallsBoxRef))
                                                  & mlItemNames .~ yesNoNames
                                                  & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoFalling == 0 then 1 else 0)
                                                  )

    modifyMenuListSReference weaponsStayBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                        & mlGeneric.mcX .~ 0
                                                        & mlGeneric.mcY .~ 10
                                                        & mlGeneric.mcName .~ "weapons stay"
                                                        & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just weaponsStayBoxRef))
                                                        & mlItemNames .~ yesNoNames
                                                        & mlCurValue .~ (if dmFlagsValue .&. Constants.dfWeaponsStay /= 0 then 1 else 0)
                                                        )

    modifyMenuListSReference instantPowerUpsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 20
                                                            & mlGeneric.mcName .~ "instant powerups"
                                                            & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just instantPowerUpsBoxRef))
                                                            & mlItemNames .~ yesNoNames
                                                            & mlCurValue .~ (if dmFlagsValue .&. Constants.dfInstantItems /= 0 then 1 else 0)
                                                            )

    modifyMenuListSReference powerUpsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 30
                                                     & mlGeneric.mcName .~ "allow powerups"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just powerUpsBoxRef))
                                                     & mlItemNames .~ yesNoNames
                                                     & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoItems == 0 then 1 else 0)
                                                     )

    modifyMenuListSReference healthBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                   & mlGeneric.mcX .~ 0
                                                   & mlGeneric.mcY .~ 40
                                                   & mlGeneric.mcName .~ "allow health"
                                                   & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just healthBoxRef))
                                                   & mlItemNames .~ yesNoNames
                                                   & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoHealth == 0 then 1 else 0)
                                                   )

    modifyMenuListSReference armorBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                  & mlGeneric.mcX .~ 0
                                                  & mlGeneric.mcY .~ 50
                                                  & mlGeneric.mcName .~ "allow armor"
                                                  & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just armorBoxRef))
                                                  & mlItemNames .~ yesNoNames
                                                  & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoArmor == 0 then 1 else 0)
                                                  )

    modifyMenuListSReference spawnFarthestBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                          & mlGeneric.mcX .~ 0
                                                          & mlGeneric.mcY .~ 60
                                                          & mlGeneric.mcName .~ "spawn farthest"
                                                          & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just spawnFarthestBoxRef))
                                                          & mlItemNames .~ yesNoNames
                                                          & mlCurValue .~ (if dmFlagsValue .&. Constants.dfSpawnFarthest /= 0 then 1 else 0)
                                                          )

    modifyMenuListSReference sameLevelBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 70
                                                      & mlGeneric.mcName .~ "same map"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just sameLevelBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfSameLevel /= 0 then 1 else 0)
                                                      )

    modifyMenuListSReference forceRespawnBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                         & mlGeneric.mcX .~ 0
                                                         & mlGeneric.mcY .~ 80
                                                         & mlGeneric.mcName .~ "force respawn"
                                                         & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just forceRespawnBoxRef))
                                                         & mlItemNames .~ yesNoNames
                                                         & mlCurValue .~ (if dmFlagsValue .&. Constants.dfForceRespawn /= 0 then 1 else 0)
                                                         )

    modifyMenuListSReference teamPlayBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 90
                                                     & mlGeneric.mcName .~ "teamplay"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just teamPlayBoxRef))
                                                     & mlItemNames .~ teamPlayNames
                                                     )

    modifyMenuListSReference allowExitBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 90
                                                      & mlGeneric.mcName .~ "allow exit"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just allowExitBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfAllowExit /= 0 then 1 else 0)
                                                      )

    modifyMenuListSReference infiniteAmmoBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                         & mlGeneric.mcX .~ 0
                                                         & mlGeneric.mcY .~ 100
                                                         & mlGeneric.mcName .~ "infinite ammo"
                                                         & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just infiniteAmmoBoxRef))
                                                         & mlItemNames .~ yesNoNames
                                                         & mlCurValue .~ (if dmFlagsValue .&. Constants.dfInfiniteAmmo /= 0 then 1 else 0)
                                                         )

    modifyMenuListSReference fixedFovBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 110
                                                     & mlGeneric.mcName .~ "fixed FOV"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just fixedFovBoxRef))
                                                     & mlItemNames .~ yesNoNames
                                                     & mlCurValue .~ (if dmFlagsValue .&. Constants.dfFixedFov /= 0 then 1 else 0)
                                                     )

    modifyMenuListSReference quadDropBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 120
                                                     & mlGeneric.mcName .~ "quad drop"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just quadDropBoxRef))
                                                     & mlItemNames .~ yesNoNames
                                                     & mlCurValue .~ (if dmFlagsValue .&. Constants.dfQuadDrop /= 0 then 1 else 0)
                                                     )

    modifyMenuListSReference friendlyFireBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                         & mlGeneric.mcX .~ 0
                                                         & mlGeneric.mcY .~ 130
                                                         & mlGeneric.mcName .~ "friendly fire"
                                                         & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just friendlyFireBoxRef))
                                                         & mlItemNames .~ yesNoNames
                                                         & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoFriendlyFire == 0 then 1 else 0)
                                                         )

    dev <- FS.developerSearchPath 2

    when (dev == 2) $ do
      modifyMenuListSReference noMinesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 140
                                                      & mlGeneric.mcName .~ "remove mines"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noMinesBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoMines /= 0 then 1 else 0)
                                                      )

      modifyMenuListSReference noNukesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 150
                                                      & mlGeneric.mcName .~ "remove nukes"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noNukesBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoNukes /= 0 then 1 else 0)
                                                      )

      modifyMenuListSReference stackDoubleBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                          & mlGeneric.mcX .~ 0
                                                          & mlGeneric.mcY .~ 160
                                                          & mlGeneric.mcName .~ "2x/4x stacking off"
                                                          & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just stackDoubleBoxRef))
                                                          & mlItemNames .~ yesNoNames
                                                          & mlCurValue .~ (dmFlagsValue .&. Constants.dfNoStackDouble)
                                                          )

      modifyMenuListSReference noSpheresBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                        & mlGeneric.mcX .~ 0
                                                        & mlGeneric.mcY .~ 170
                                                        & mlGeneric.mcName .~ "remove spheres"
                                                        & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noSpheresBoxRef))
                                                        & mlItemNames .~ yesNoNames
                                                        & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoSpheres /= 0 then 1 else 0)
                                                        )

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

    when (dev == 2) $ do
      menuAddItem dmOptionsMenuRef (MenuListRef noMinesBoxRef)
      menuAddItem dmOptionsMenuRef (MenuListRef noNukesBoxRef)
      menuAddItem dmOptionsMenuRef (MenuListRef stackDoubleBoxRef)
      menuAddItem dmOptionsMenuRef (MenuListRef noSpheresBoxRef)

    menuCenter dmOptionsMenuRef

    -- set the original dmflags statusbar
    dmFlagCallback Nothing
    dmOptionsStatusBar <- use $ menuGlobals.mgDmOptionsStatusBar
    menuSetStatusBar dmOptionsMenuRef dmOptionsStatusBar

saveGameCallback :: MenuActionSReference -> Quake ()
saveGameCallback menuActionRef = do
    action <- readMenuActionSReference menuActionRef
    CBuf.addText ("save save" `B.append` BC.pack (show (action^.maGeneric.mcLocalData._x)) `B.append` "\n")
    forceMenuOff

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
downloadCallback menuListRef = do
    item <- readMenuListSReference menuListRef

    if | menuListRef == allowDownloadBoxRef ->
           CVar.setValueI "allow_download" (item^.mlCurValue)

       | menuListRef == allowDownloadMapsBoxRef ->
           CVar.setValueI "allow_download_maps" (item^.mlCurValue)

       | menuListRef == allowDownloadModelsBoxRef ->
           CVar.setValueI "allow_download_models" (item^.mlCurValue)

       | menuListRef == allowDownloadPlayersBoxRef ->
           CVar.setValueI "allow_download_players" (item^.mlCurValue)

       | menuListRef == allowDownloadSoundsBoxRef ->
           CVar.setValueI "allow_download_sounds" (item^.mlCurValue)

       | otherwise ->
           return () -- IMPROVE: or throw error?

updateVolumeFunc :: Quake ()
updateVolumeFunc = do
  io (putStrLn "Menu.updateVolumeFunc") >> undefined -- TODO

updateCdVolumeFunc :: Quake ()
updateCdVolumeFunc = do
  io (putStrLn "Menu.updateCdVolumeFunc") >> undefined -- TODO

updateSoundQualityFunc :: Quake ()
updateSoundQualityFunc = do
  io (putStrLn "Menu.updateSoundQualityFunc") >> undefined -- TODO

mouseSpeedFunc :: Quake ()
mouseSpeedFunc = do
  io (putStrLn "Menu.mouseSpeedFunc") >> undefined -- TODO

alwaysRunFunc :: Quake ()
alwaysRunFunc = do
  io (putStrLn "Menu.alwaysRunFunc") >> undefined -- TODO

invertMouseFunc :: Quake ()
invertMouseFunc = do
  io (putStrLn "Menu.invertMouseFunc") >> undefined -- TODO

lookSpringFunc :: Quake ()
lookSpringFunc = do
  io (putStrLn "Menu.lookSpringFunc") >> undefined -- TODO
  
lookStrafeFunc :: Quake ()
lookStrafeFunc = do
  io (putStrLn "Menu.lookStrafeFunc") >> undefined -- TODO

freeLookFunc :: Quake ()
freeLookFunc = do
  io (putStrLn "Menu.freeLookFunc") >> undefined -- TODO

crosshairFunc :: Quake ()
crosshairFunc = do
  io (putStrLn "Menu.crosshairFunc") >> undefined -- TODO

joystickFunc :: Quake ()
joystickFunc = do
  io (putStrLn "Menu.joystickFunc") >> undefined -- TODO
  
customizeControlsFunc :: Quake ()
customizeControlsFunc = do
  io (putStrLn "Menu.customizeControlsFunc") >> undefined -- TODO

controlsResetDefaultsFunc :: Quake ()
controlsResetDefaultsFunc = do
  io (putStrLn "Menu.controlsResetDefaultsFunc") >> undefined -- TODO

consoleFunc :: Quake ()
consoleFunc = do
  io (putStrLn "Menu.consoleFunc") >> undefined -- TODO

controlsSetMenuItemValues :: Quake ()
controlsSetMenuItemValues = do
  io (putStrLn "Menu.controlsSetMenuItemValues") >> undefined -- TODO

keyCursorDrawFunc :: MenuFrameworkSReference -> Quake ()
keyCursorDrawFunc _ = do
  io (putStrLn "Menu.keyCursorDrawFunc") >> undefined -- TODO

drawKeyBindingFunc :: MenuActionSReference -> Quake ()
drawKeyBindingFunc _ = do
  io (putStrLn "Menu.drawKeyBindingFunc") >> undefined -- TODO

loadGameCallback :: MenuActionSReference -> Quake ()
loadGameCallback _ = do
  io (putStrLn "Menu.loadGameCallback") >> undefined -- TODO
  
creditsMenuDraw :: XCommandT
creditsMenuDraw =
  XCommandT "Menu.creditsMenuDraw" (do
    io (putStrLn "Menu.creditsMenuDraw") >> undefined -- TODO
  )
  
creditsKey :: KeyFuncT
creditsKey =
  KeyFuncT "Menu.creditsKey" (\key -> do
    io (putStrLn "Menu.creditsKey") >> undefined -- TODO
  )
  
dmOptionsMenuDraw :: XCommandT
dmOptionsMenuDraw =
  XCommandT "Menu.dmOptionsMenuDraw" (do
    io (putStrLn "Menu.dmOptionsMenuDraw") >> undefined -- TODO
  )
  
dmOptionsMenuKey :: KeyFuncT
dmOptionsMenuKey =
  KeyFuncT "Menu.dmOptionsMenuKey" (\key -> do
    io (putStrLn "Menu.dmOptionsMenuKey") >> undefined -- TODO
  )

keyBindingFunc :: MenuActionSReference -> Quake ()
keyBindingFunc _ = do
    io (putStrLn "Menu.keyBindingFunc") >> undefined -- TODO

unbindCommand :: B.ByteString -> Quake ()
unbindCommand _ = do
    io (putStrLn "Menu.unbindCommand") >> undefined -- TODO

dmFlagCallback :: Maybe MenuListSReference -> Quake ()
dmFlagCallback _ = do
    io (putStrLn "Menu.dmFlagCallback") >> undefined -- TODO
