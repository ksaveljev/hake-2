{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Menu where

import Control.Lens (zoom, use, preuse, ix, (.=), (+=), (^.), (%=), (&), (.~), (%~), (+~), (-=))
import Control.Monad (when, void, unless)
import Data.Char (ord)
import Data.Maybe (fromJust)
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
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import qualified Sound.S as S

mainItems :: Int
mainItems = 5

numCursorFrames :: Int
numCursorFrames = 15

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

addItem :: MenuFrameworkSReference -> MenuItemReference -> Quake ()
addItem menuFrameworkRef menuItemRef = do
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

    tallySlots menuFrameworkRef >>= \n ->
      modifyMenuFrameworkSReference menuFrameworkRef (\v -> v & mfNSlots .~ n)

center :: MenuFrameworkSReference -> Quake ()
center menuFrameworkRef = do
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

tallySlots :: MenuFrameworkSReference -> Quake Int
tallySlots menuFrameworkRef = do
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
                                                          & maGeneric.mcCallback .~ Just (loadGameF^.xcCmd)
                                                          )

    modifyMenuActionSReference saveGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 50
                                                          & maGeneric.mcName .~ "save game"
                                                          & maGeneric.mcCallback .~ Just (saveGameF^.xcCmd)
                                                          )

    modifyMenuActionSReference creditsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                         & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                         & maGeneric.mcX .~ 0
                                                         & maGeneric.mcY .~ 60
                                                         & maGeneric.mcName .~ "credits"
                                                         & maGeneric.mcCallback .~ Just (creditsF^.xcCmd)
                                                         )

    addItem gameMenuRef (MenuActionRef easyGameActionRef)
    addItem gameMenuRef (MenuActionRef mediumGameActionRef)
    addItem gameMenuRef (MenuActionRef hardGameActionRef)
    addItem gameMenuRef (MenuSeparatorRef blankLineRef)
    addItem gameMenuRef (MenuActionRef loadGameActionRef)
    addItem gameMenuRef (MenuActionRef saveGameActionRef)
    addItem gameMenuRef (MenuSeparatorRef blankLineRef)
    addItem gameMenuRef (MenuActionRef creditsActionRef)

    center gameMenuRef

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

loadGameF :: XCommandT
loadGameF =
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

    createSaveStrings 0 maxSaveGames

    io (putStrLn "Menu.loadGameMenuInit") >> undefined -- TODO

-- Search the save dir for saved games and their names.
createSaveStrings :: Int -> Int -> Quake ()
createSaveStrings idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise = do
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
    io (putStrLn "Menu.loadGameMenuKeyF") >> undefined -- TODO
  )

saveGameF :: XCommandT
saveGameF =
  XCommandT "Menu.saveGameF" (do
    io (putStrLn "Menu.saveGameF") >> undefined -- TODO
  )

creditsF :: XCommandT
creditsF =
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
    adjustCursor gameMenuRef 1
    menuDraw gameMenuRef
  )

gameMenuKeyF :: KeyFuncT
gameMenuKeyF =
  KeyFuncT "Menu.gameMenuKeyF" (\key -> do
    io (putStrLn "Menu.gameMenuKeyF") >> undefined -- TODO
  )

menuLoadGameF :: XCommandT
menuLoadGameF =
  XCommandT "Menu.menuLoadGame" (do
    io (putStrLn "Menu.menuLoadGame") >> undefined -- TODO
  )

menuSaveGameF :: XCommandT
menuSaveGameF =
  XCommandT "Menu.menuSaveGame" (do
    io (putStrLn "Menu.menuSaveGame") >> undefined -- TODO
  )

menuJoinServerF :: XCommandT
menuJoinServerF =
  XCommandT "Menu.menuJoinServer" (do
    io (putStrLn "Menu.menuJoinServer") >> undefined -- TODO
  )

menuAddressBookF :: XCommandT
menuAddressBookF =
  XCommandT "Menu.menuAddressBook" (do
    io (putStrLn "Menu.menuAddressBook") >> undefined -- TODO
  )

menuStartServerF :: XCommandT
menuStartServerF =
  XCommandT "Menu.menuStartServer" (do
    io (putStrLn "Menu.menuStartServer") >> undefined -- TODO
  )

menuDMOptionsF :: XCommandT
menuDMOptionsF =
  XCommandT "Menu.menuDMOptions" (do
    io (putStrLn "Menu.menuDMOptions") >> undefined -- TODO
  )

menuPlayerConfigF :: XCommandT
menuPlayerConfigF =
  XCommandT "Menu.menuPlayerConfig" (do
    io (putStrLn "Menu.menuPlayerConfig") >> undefined -- TODO
  )

menuDownloadOptionsF :: XCommandT
menuDownloadOptionsF =
  XCommandT "Menu.menuDownloadOptions" (do
    io (putStrLn "Menu.menuDownloadOptions") >> undefined -- TODO
  )

menuCreditsF :: XCommandT
menuCreditsF =
  XCommandT "Menu.menuCredits" (do
    io (putStrLn "Menu.menuCredits") >> undefined -- TODO
  )

menuMultiplayerF :: XCommandT
menuMultiplayerF =
  XCommandT "Menu.menuMultiplayer" (do
    io (putStrLn "Menu.menuMultiplayer") >> undefined -- TODO
  )

menuVideoF :: XCommandT
menuVideoF =
  XCommandT "Menu.menuVideo" (do
    io (putStrLn "Menu.menuVideo") >> undefined -- TODO
  )

menuOptionsF :: XCommandT
menuOptionsF =
  XCommandT "Menu.menuOptions" (do
    io (putStrLn "Menu.menuOptions") >> undefined -- TODO
  )

menuKeysF :: XCommandT
menuKeysF =
  XCommandT "Menu.menuKeys" (do
    io (putStrLn "Menu.menuKeys") >> undefined -- TODO
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

menuKeyDown :: Int -> Quake ()
menuKeyDown key = do
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
adjustCursor :: MenuFrameworkSReference -> Int -> Quake ()
adjustCursor _ _ = do
    io (putStrLn "Menu.adjustCursor") >> undefined -- TODO

menuDraw :: MenuFrameworkSReference -> Quake ()
menuDraw _ = do
    io (putStrLn "Menu.menuDraw") >> undefined -- TODO
