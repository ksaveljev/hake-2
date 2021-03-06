{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Menu where

import Control.Lens (zoom, use, preuse, ix, (.=), (+=), (^.), (%=), (&), (.~), (%~), (+~), (-=), _1, _2, (-~))
import Control.Monad (when, void, unless, liftM)
import Data.Bits ((.&.), shiftR)
import Data.Char (ord, chr)
import Data.Maybe (fromJust)
import Linear (V4(..), _x)
import System.Directory (doesFileExist, readable, getPermissions)
import System.IO (IOMode(ReadMode))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Game.CVarT
import Client.MenuLayerT
import Client.MenuFrameworkS
import QuakeRef
import Client.RefExportT
import Render.Renderer
import Client.ClientStateT
import Client.ClientStaticT
import Client.MenuItem
import Client.KeyFuncT
import Client.MenuCommonS
import Types
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CL as CL
import {-# SOURCE #-} qualified Client.Console as Console
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
import {-# SOURCE #-} qualified Util.Lib as Lib
import qualified Util.QuakeFile as QuakeFile

mainItems :: Int
mainItems = 5

numCursorFrames :: Int
numCursorFrames = 15

sliderRange :: Int
sliderRange = 10

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

menuAddItem :: Ref MenuFrameworkS -> MenuItemRef -> Quake ()
menuAddItem menuFrameworkRef menuItemRef = do
    menu <- readRef menuFrameworkRef
    let nItems = menu^.mfNItems

    when (nItems == 0) $
      modifyRef menuFrameworkRef (\v -> v & mfNSlots .~ 0)

    when (nItems < Constants.maxMenuItems) $ do
      modifyRef menuFrameworkRef (\v -> v & mfItems %~ (`V.snoc` menuItemRef))

      case menuItemRef of
        MenuListRef ref ->
          modifyRef ref (\v -> v & mlGeneric.mcParent .~ Just menuFrameworkRef)

        MenuActionRef ref ->
          modifyRef ref (\v -> v & maGeneric.mcParent .~ Just menuFrameworkRef)

        MenuSliderRef ref ->
          modifyRef ref (\v -> v & msGeneric.mcParent .~ Just menuFrameworkRef)

        MenuSeparatorRef ref ->
          modifyRef ref (\v -> v & mspGeneric.mcParent .~ Just menuFrameworkRef)

        MenuFieldRef ref ->
          modifyRef ref (\v -> v & mflGeneric.mcParent .~ Just menuFrameworkRef)

      modifyRef menuFrameworkRef (\v -> v & mfNItems +~ 1)

    menuTallySlots menuFrameworkRef >>= \n ->
      modifyRef menuFrameworkRef (\v -> v & mfNSlots .~ n)

menuCenter :: Ref MenuFrameworkS -> Quake ()
menuCenter menuFrameworkRef = do
    menu <- readRef menuFrameworkRef
    let menuItemRef = V.last (menu^.mfItems)
    height <- case menuItemRef of
                MenuListRef ref -> do
                  menuItem <- readRef ref
                  return (menuItem^.mlGeneric.mcY)

                MenuSliderRef ref -> do
                  menuItem <- readRef ref
                  return (menuItem^.msGeneric.mcY)

                MenuActionRef ref -> do
                  menuItem <- readRef ref
                  return (menuItem^.maGeneric.mcY)
    h <- use $ globals.gVidDef.vdHeight

    modifyRef menuFrameworkRef (\v -> v & mfY .~ (h - (height + 10)) `div` 2)

menuTallySlots :: Ref MenuFrameworkS -> Quake Int
menuTallySlots menuFrameworkRef = do
    menu <- readRef menuFrameworkRef

    itemsNum <- V.mapM numberOfItems (menu^.mfItems)
    return $ V.foldl' (+) 0 itemsNum

  where numberOfItems :: MenuItemRef -> Quake Int
        numberOfItems (MenuListRef ref) = do
          menuItem <- readRef ref
          return (V.length (menuItem^.mlItemNames))
        numberOfItems _ =
          return 1 -- MenuSliderRef and MenuActionRef

pushMenu :: XCommandT -> KeyFuncT -> Quake ()
pushMenu draw key = do
    maxClients <- CVar.variableValue "maxclients"
    serverState <- use $ globals.gServerState

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
    
    globals.gCls.csKeyDest .= Constants.keyMenu

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
    vidDef' <- use $ globals.gVidDef
    mainCursor <- use $ menuGlobals.mgMainCursor
    realTime <- use $ globals.gCls.csRealTime

    let yStart = (vidDef'^.vdHeight) `div` 2 - 110
        xOffset = ((vidDef'^.vdWidth) - widest + 70) `div` 2

    drawMenuPics names mainCursor yStart xOffset 0 (V.length names)

    let litName = (names V.! mainCursor) `B.append` "_sel"

    Just renderer <- use $ globals.gRenderer
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
              Just renderer <- use $ globals.gRenderer
              Just (w, h) <- (renderer^.rRefExport.reDrawGetPicSize) (names V.! idx)
              let widest' = if w > widest then w else widest
              calcWidthHeight names widest' (totalHeight + (h + 12)) (idx + 1) maxIdx

        drawMenuPics :: V.Vector B.ByteString -> Int -> Int -> Int -> Int -> Int -> Quake ()
        drawMenuPics names mainCursor yStart xOffset idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              when (idx /= mainCursor) $ do
                Just renderer <- use $ globals.gRenderer
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

       | otherwise ->
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
    vidDef' <- use $ globals.gVidDef

    modifyRef gameMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                       & mfNItems .~ 0
                                                       )

    modifyRef easyGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 0
                                                          & maGeneric.mcName .~ Just "easy"
                                                          & maGeneric.mcCallback .~ Just easyGameFunc
                                                          )

    modifyRef mediumGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 10
                                                            & maGeneric.mcName .~ Just "medium"
                                                            & maGeneric.mcCallback .~ Just mediumGameFunc
                                                            )

    modifyRef hardGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 20
                                                          & maGeneric.mcName .~ Just "hard"
                                                          & maGeneric.mcCallback .~ Just hardGameFunc
                                                          )

    modifyRef blankLineRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator)

    modifyRef loadGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 40
                                                          & maGeneric.mcName .~ Just "load game"
                                                          & maGeneric.mcCallback .~ Just (menuLoadGameF^.xcCmd)
                                                          )

    modifyRef saveGameActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 50
                                                          & maGeneric.mcName .~ Just "save game"
                                                          & maGeneric.mcCallback .~ Just (menuSaveGameF^.xcCmd)
                                                          )

    modifyRef creditsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                         & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                         & maGeneric.mcX .~ 0
                                                         & maGeneric.mcY .~ 60
                                                         & maGeneric.mcName .~ Just "credits"
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
    vidDef' <- use $ globals.gVidDef

    modifyRef loadGameMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 120
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
              
              modifyRef actionRef (\v -> v & maGeneric.mcName .~ Just (saveStrings V.! idx)
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
                then do
                  qf <- io $ QuakeFile.open name
                  mSaveName <- io $ QuakeFile.readString qf
                  io $ QuakeFile.close qf

                  case mSaveName of
                    Nothing ->
                      zoom menuGlobals $ do
                        mgSaveStrings.ix idx .= "<EMPTY>"
                        mgSaveValid.ix idx .= False

                    Just saveName ->
                      zoom menuGlobals $ do
                        mgSaveStrings.ix idx .= saveName
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
      loadGameMenu <- readRef loadGameMenuRef
      modifyRef saveGameMenuRef (\v -> v & mfCursor .~ if (loadGameMenu^.mfCursor) - 1 < 0 then 0 else (loadGameMenu^.mfCursor) - 1)

    defaultMenuKey loadGameMenuRef key
  )

menuSaveGameF :: XCommandT
menuSaveGameF =
  XCommandT "Menu.saveGameF" (do
    serverState' <- use $ globals.gServerState

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
                   
    realTime <- use $ globals.gCls.csRealTime
    
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
    globals.gCl.csServerCount .= -1
    forceMenuOff
    CVar.setValueI "deathmatch" 0
    CVar.setValueI "coop" 0

    CVar.setValueI "gamerules" 0

    CBuf.addText "loading ; killserver ; wait ; newgame\n"
    globals.gCls.csKeyDest .= Constants.keyGame

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
    vidDef' <- use $ globals.gVidDef

    modifyRef joinServerMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 120
                                                             & mfNItems .~ 0
                                                             )

    modifyRef joinServerAddressBookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                       & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                       & maGeneric.mcX .~ 0
                                                                       & maGeneric.mcY .~ 0
                                                                       & maGeneric.mcName .~ Just "address book"
                                                                       & maGeneric.mcCallback .~ Just (menuAddressBookF^.xcCmd)
                                                                       )

    modifyRef joinServerSearchActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 10
                                                                  & maGeneric.mcName .~ Just "refresh server list"
                                                                  & maGeneric.mcCallback .~ Just searchLocalGames
                                                                  & maGeneric.mcStatusBar .~ Just "search for servers"
                                                                  )

    modifyRef joinServerServerTitleRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator
                                                                    & mspGeneric.mcName .~ Just "connect to..."
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
              modifyRef actionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 40 + idx * 10
                                                            & maGeneric.mcName .~ Just Constants.noServerString
                                                            & maGeneric.mcCallback .~ Just (joinServerFunc actionRef)
                                                            & maGeneric.mcStatusBar .~ Just "press ENTER to connect"
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
    Just renderer <- use $ globals.gRenderer
    vidDef' <- use $ globals.gVidDef
    Just (w, h) <- (renderer^.rRefExport.reDrawGetPicSize) "quit"
    (renderer^.rRefExport.reDrawPic) (((vidDef'^.vdWidth) - w) `div` 2) (((vidDef'^.vdHeight) - h) `div` 2) "quit"
  )

quitKeyF :: KeyFuncT
quitKeyF =
  KeyFuncT "Menu.quitKeyF" (\key -> do
    if | key `elem` [ KeyConstants.kEscape, ord 'n', ord 'N' ] ->
           popMenu

       | key `elem` [ ord 'Y', ord 'y' ] -> do
           globals.gCls.csKeyDest .= Constants.keyConsole
           (CL.quitF)^.xcCmd

       | otherwise ->
           return ()

    return Nothing
  )

draw :: Quake ()
draw = do
    keyDest <- use $ globals.gCls.csKeyDest

    when (keyDest == Constants.keyMenu) $ do
      -- repaint everything next frame
      SCR.dirtyScreen

      -- dim everything behind it down
      cinematicTime <- use $ globals.gCl.csCinematicTime
      Just renderer <- use $ globals.gRenderer
      vidDef' <- use $ globals.gVidDef

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
        modifyRef actionRef (\v -> v & maGeneric.mcName .~ Just info')
        
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

    Just renderer <- use $ globals.gRenderer
    (renderer^.rRefExport.reDrawPic) x y ("m_cursor" `B.append` BC.pack (show f))

  where registerCursorPics :: Int -> Int -> Quake ()
        registerCursorPics idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
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

    globals.gCls.csKeyDest .= Constants.keyGame
    Key.clearStates
    void $ CVar.set "paused" "0"

banner :: B.ByteString -> Quake ()
banner name = do
    Just renderer <- use $ globals.gRenderer
    vidDef' <- use $ globals.gVidDef
    Just (w, h) <- (renderer^.rRefExport.reDrawGetPicSize) name
    (renderer^.rRefExport.reDrawPic) ((vidDef'^.vdWidth) `div` 2 - w `div` 2) ((vidDef'^.vdHeight) `div` 2 - 110) name

{-
- Menu_AdjustCursor
-
- This function takes the given menu, the direction, and attempts 
- to adjust the menu's cursor so that it's at the next available 
- slot.
-}
menuAdjustCursor :: Ref MenuFrameworkS -> Int -> Quake ()
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
          menu <- readRef menuRef
          let cursor = (menu^.mfCursor) + dir
              newCursor = if dir == 1
                            then if cursor >= (menu^.mfNItems) then 0 else cursor
                            else if cursor < 0 then (menu^.mfNItems) - 1 else cursor
          modifyRef menuRef (\v -> v & mfCursor .~ newCursor)
    
menuItemAtCursor :: Ref MenuFrameworkS -> Quake (Maybe MenuItemRef)
menuItemAtCursor menuRef = do
    menu <- readRef menuRef
    
    return $ if (menu^.mfCursor) < 0 || (menu^.mfCursor) >= (menu^.mfNItems)
               then Nothing
               else Just ((menu^.mfItems) V.! (menu^.mfCursor))

menuDraw :: Ref MenuFrameworkS -> Quake ()
menuDraw menuRef = do
    menu <- readRef menuRef

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
                             Just renderer <- use $ globals.gRenderer
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
      Nothing ->
        menuDrawStatusBar (menu^.mfStatusBar)
      
      Just item ->
        case item^.mcStatusBarFunc of
          Just f -> f
          Nothing ->
            case item^.mcStatusBar of
              Just _ -> menuDrawStatusBar (item^.mcStatusBar)
              Nothing -> menuDrawStatusBar (menu^.mfStatusBar)

  where drawContents :: MenuFrameworkS -> Int -> Int -> Quake ()
        drawContents menu idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let itemRef = (menu^.mfItems) V.! idx
              menuCommon <- menuItemCommon itemRef

              if | (menuCommon^.mcType) == Constants.mtypeField -> do
                     let MenuFieldRef menuItemRef = itemRef
                     menuItem <- readRef menuItemRef
                     fieldDraw menuItemRef

                 | (menuCommon^.mcType) == Constants.mtypeSlider -> do
                     let MenuSliderRef menuItemRef = itemRef
                     menuItem <- readRef menuItemRef
                     sliderDraw menuItemRef

                 | (menuCommon^.mcType) == Constants.mtypeList -> do
                     let MenuListRef menuItemRef = itemRef
                     menuItem <- readRef menuItemRef
                     menuListDraw menuItemRef

                 | (menuCommon^.mcType) == Constants.mtypeSpinControl -> do
                     let MenuListRef menuItemRef = itemRef
                     menuItem <- readRef menuItemRef
                     spinControlDraw menuItemRef

                 | (menuCommon^.mcType) == Constants.mtypeAction -> do
                     let MenuActionRef menuItemRef = itemRef
                     menuItem <- readRef menuItemRef
                     actionDraw menuItemRef

                 | (menuCommon^.mcType) == Constants.mtypeSeparator -> do
                     let MenuSeparatorRef menuItemRef = itemRef
                     menuItem <- readRef menuItemRef
                     separatorDraw menuItemRef

              drawContents menu (idx + 1) maxIdx

menuItemCommon :: MenuItemRef -> Quake MenuCommonS
menuItemCommon menuItemRef = do
    case menuItemRef of
      MenuListRef itemRef -> do
        menuItem <- readRef itemRef
        return (menuItem^.mlGeneric)
      MenuActionRef itemRef -> do
        menuItem <- readRef itemRef
        return (menuItem^.maGeneric)
      MenuSliderRef itemRef -> do
        menuItem <- readRef itemRef
        return (menuItem^.msGeneric)
      MenuSeparatorRef itemRef -> do
        menuItem <- readRef itemRef
        return (menuItem^.mspGeneric)

defaultMenuKey :: Ref MenuFrameworkS -> Int -> Quake (Maybe B.ByteString)
defaultMenuKey menuRef key = do
    done <- checkFieldKey
    
    if done
      then
        return Nothing
        
      else do
        if | key == KeyConstants.kEscape -> do
               popMenu
               return (Just menuOutSound)

           | key `elem` [KeyConstants.kKpUpArrow, KeyConstants.kUpArrow] -> do
               modifyRef menuRef (\v -> v & mfCursor -~ 1)
               menuAdjustCursor menuRef (-1)
               return (Just menuMoveSound)

           | key == KeyConstants.kTab -> do
               modifyRef menuRef (\v -> v & mfCursor +~ 1)
               menuAdjustCursor menuRef 1
               return (Just menuMoveSound)

           | key `elem` [KeyConstants.kKpDownArrow, KeyConstants.kDownArrow] -> do
               modifyRef menuRef (\v -> v & mfCursor +~ 1)
               menuAdjustCursor menuRef 1
               return (Just menuMoveSound)

           | key `elem` [KeyConstants.kKpLeftArrow, KeyConstants.kLeftArrow] -> do
               menuSlideItem menuRef (-1)
               return (Just menuMoveSound)

           | key `elem` [KeyConstants.kKpRightArrow, KeyConstants.kRightArrow] -> do
               menuSlideItem menuRef 1
               return (Just menuMoveSound)

           | key `elem` [KeyConstants.kMouse1, KeyConstants.kMouse2, KeyConstants.kMouse3
                        , KeyConstants.kJoy1, KeyConstants.kJoy2, KeyConstants.kJoy3
                        , KeyConstants.kJoy4, KeyConstants.kKpEnter, KeyConstants.kEnter] -> do
               menuSelectItem menuRef
               return (Just menuMoveSound)

           | otherwise ->
               return Nothing
    
  where checkFieldKey :: Quake Bool
        checkFieldKey = do
          menuItemRef <- menuItemAtCursor menuRef

          mItem <- case menuItemRef of
                     Nothing -> return Nothing
                     Just itemRef -> menuItemCommon itemRef >>= return . Just
          
          case mItem of
            Nothing ->
              return False
            
            Just item -> do
              if (item^.mcType) == Constants.mtypeField
                then do
                  let Just (MenuFieldRef fieldRef) = menuItemRef
                  fieldKey fieldRef key
                  
                else
                  return False

fieldKey :: Ref MenuFieldS -> Int -> Quake Bool
fieldKey fieldRef key = do
    let k = if | key == KeyConstants.kKpSlash -> '/'
               | key == KeyConstants.kKpMinus -> '-'
               | key == KeyConstants.kKpPlus -> '+'
               | key == KeyConstants.kKpHome -> '7'
               | key == KeyConstants.kKpUpArrow -> '8'
               | key == KeyConstants.kKpPgUp -> '9'
               | key == KeyConstants.kKpLeftArrow -> '4'
               | key == KeyConstants.kKp5 -> '5'
               | key == KeyConstants.kKpRightArrow -> '6'
               | key == KeyConstants.kKpEnd -> '1'
               | key == KeyConstants.kKpDownArrow -> '2'
               | key == KeyConstants.kKpPgDn -> '3'
               | key == KeyConstants.kKpIns -> '0'
               | key == KeyConstants.kKpDel -> '.'
               | otherwise -> chr key
    
    if ord k > 127
      then
        return False
        
      else do
        -- support pasting from the clipboard
        io (putStrLn "Menu.fieldKey") >> undefined -- TODO

searchLocalGames :: Quake ()
searchLocalGames = do
    io (putStrLn "Menu.searchLocalGames") >> undefined -- TODO

joinServerFunc :: Ref MenuActionS -> Quake ()
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
    vidDef' <- use $ globals.gVidDef

    modifyRef addressBookMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 142
                                                              & mfY .~ ((vidDef'^.vdHeight) `div` 2) - 58
                                                              & mfNItems .~ 0
                                                              )
                                                              
    setupAddressBookMenuActions 0 Constants.numAddressBookEntries
    
  where setupAddressBookMenuActions :: Int -> Int -> Quake ()
        setupAddressBookMenuActions idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just adr <- CVar.get ("adr" `B.append` (BC.pack (show idx))) "" Constants.cvarArchive
              let fieldRef = addressBookFields V.! idx
              
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
                                                          & mflBuffer .~ (adr^.cvString)
                                                          )
              
              menuAddItem addressBookMenuRef (MenuFieldRef fieldRef)
              setupAddressBookMenuActions (idx + 1) maxIdx

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
              let fieldRef = addressBookFields V.! idx
              field <- readRef fieldRef
              CVar.set ("adr" `B.append` BC.pack (show idx)) (field^.mflBuffer)
              setAddressBookCVars (idx + 1) maxIdx

startServerMenuInit :: Quake ()
startServerMenuInit = do
    -- load the list of map names
    gameDir <- FS.gameDir
    
    let mapsFileName = gameDir `B.append` "/maps.lst"
    
    maybeFp <- Lib.fOpen mapsFileName ReadMode
    
    contents <- case maybeFp of
                  Nothing -> do
                    buffer <- FS.loadFile "maps.lst"
                    case buffer of
                      Nothing -> return (Left "couldn't find maps.lst\n")
                      Just buf -> return (Right buf)
      
                  Just fp -> do
                    -- IMPROVE: exception handing
                    -- } catch (Exception e) {
                    --     Com.Error(ERR_DROP, "couldn't load maps.lst\n");
                    -- }
                    buffer <- io $ B.hGetContents fp
                    return (Right buffer)
    
    case contents of
      Left errMsg ->
        Com.comError Constants.errDrop errMsg
        
      Right buffer -> do
        let mapLines = tokenise "\r\n" buffer 
            numMaps = length mapLines
            
        when (numMaps == 0) $
          Com.comError Constants.errDrop "no maps in maps.lst\n"
        
        mapNames <- mapM parseMapLine mapLines
        let mapNames' = V.fromList mapNames
        menuGlobals.mgMapNames .= Just mapNames'
        
        -- initialize the menu stuff
        vidDef' <- use $ globals.gVidDef

        modifyRef startServerMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                                  & mfNItems .~ 0
                                                                  )
                                                                  
        modifyRef startMapListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                          & mlGeneric.mcX .~ 0
                                                          & mlGeneric.mcY .~ 0
                                                          & mlGeneric.mcName .~ Just "initial map"
                                                          & mlItemNames .~ mapNames'
                                                          )
        
        dev <- FS.developerSearchPath 2
        coopValue <- liftM (^.cvValue) coopCVar
        
        modifyRef rulesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 20
                                                      & mlGeneric.mcName .~ Just "rules"
                                                      & mlItemNames .~ (if dev == 2 then dmCoopNamesRogue else dmCoopNames)
                                                      & mlCurValue .~ (if coopValue /= 0 then 1 else 0)
                                                      & mlGeneric.mcCallback .~ Just rulesChangeFunc
                                                      )
                                                      
        timeLimitStr <- CVar.variableString "timelimit"
                                                      
        modifyRef timeLimitFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                                             & mflGeneric.mcName .~ Just "time limit"
                                                             & mflGeneric.mcFlags .~ Constants.qmfNumbersOnly
                                                             & mflGeneric.mcX .~ 0
                                                             & mflGeneric.mcY .~ 36
                                                             & mflGeneric.mcStatusBar .~ Just "0 = no limit"
                                                             & mflLength .~ 3
                                                             & mflVisibleLength .~ 3
                                                             & mflBuffer .~ timeLimitStr
                                                             )
        
        fragLimitStr <- CVar.variableString "fraglimit"
                                                             
        modifyRef fragLimitFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                                             & mflGeneric.mcName .~ Just "frag limit"
                                                             & mflGeneric.mcFlags .~ Constants.qmfNumbersOnly
                                                             & mflGeneric.mcX .~ 0
                                                             & mflGeneric.mcY .~ 54
                                                             & mflGeneric.mcStatusBar .~ Just "0 = no limit"
                                                             & mflLength .~ 3
                                                             & mflVisibleLength .~ 3
                                                             & mflBuffer .~ fragLimitStr
                                                             )
                                                          
        -- maxclients determines the maximum number of players that can join
        -- the game. If maxclients is only "1" then we should default the menu
        -- option to 8 players, otherwise use whatever its current value is.
        -- Clamping will be done when the server is actually started.
        maxClientsValue <- CVar.variableValue "maxclients"
        maxClientsStr <- CVar.variableString "maxclients"
        
        modifyRef maxClientsFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                                              & mflGeneric.mcName .~ Just "max players"
                                                              & mflGeneric.mcFlags .~ Constants.qmfNumbersOnly
                                                              & mflGeneric.mcX .~ 0
                                                              & mflGeneric.mcY .~ 72
                                                              & mflGeneric.mcStatusBar .~ Nothing
                                                              & mflLength .~ 3
                                                              & mflVisibleLength .~ 3
                                                              & mflBuffer .~ (if maxClientsValue == 1 then "8" else maxClientsStr)
                                                              )
                                                              
        hostnameStr <- CVar.variableString "hostname"
                                                              
        modifyRef hostnameFieldRef (\v -> v & mflGeneric.mcType .~ Constants.mtypeField
                                                            & mflGeneric.mcName .~ Just "hostname"
                                                            & mflGeneric.mcFlags .~ 0
                                                            & mflGeneric.mcX .~ 0
                                                            & mflGeneric.mcY .~ 90
                                                            & mflGeneric.mcStatusBar .~ Nothing
                                                            & mflLength .~ 12
                                                            & mflVisibleLength .~ 12
                                                            & mflBuffer .~ hostnameStr
                                                            & mflCursor .~ B.length hostnameStr
                                                            )
                                                            
        modifyRef startServerDMOptionsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                          & maGeneric.mcName .~ Just " deathmatch flags"
                                                                          & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                          & maGeneric.mcX .~ 24
                                                                          & maGeneric.mcY .~ 108
                                                                          & maGeneric.mcStatusBar .~ Nothing
                                                                          & maGeneric.mcCallback .~ Just dmOptionsFunc
                                                                          )
                                                                          
        modifyRef startServerStartActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                      & maGeneric.mcName .~ Just " begin"
                                                                      & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                      & maGeneric.mcX .~ 24
                                                                      & maGeneric.mcY .~ 128
                                                                      & maGeneric.mcCallback .~ Just startServerActionFunc
                                                                      )
                                                                      
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
        
  where tokenise :: B.ByteString -> B.ByteString -> [B.ByteString]
        tokenise x y = let (h, t) = B.breakSubstring x y
                       in h : if B.null t then [] else tokenise x (B.drop (B.length x) t)
                       
        parseMapLine :: B.ByteString -> Quake B.ByteString
        parseMapLine mapLine = do
          (mLongName, idx) <- Com.parse mapLine (B.length mapLine) 0
          (mShortName, _) <- Com.parse mapLine (B.length mapLine) idx
          let longName = case mLongName of
                           Nothing -> ""
                           Just str -> str
              shortName = case mShortName of
                            Nothing -> ""
                            Just str -> str
          return (longName `B.append` "\n" `B.append` shortName)

startServerMenuDraw :: XCommandT
startServerMenuDraw =
  XCommandT "Menu.startServerMenuDraw" (menuDraw startServerMenuRef)

startServerMenuKey :: KeyFuncT
startServerMenuKey =
  KeyFuncT "Menu.startServerMenuKey" (\key -> do
    when (key == KeyConstants.kEscape) $ do
      menuGlobals.mgMapNames .= Nothing
      
    defaultMenuKey startServerMenuRef key
  )

saveGameMenuInit :: Quake ()
saveGameMenuInit = do
    vidDef' <- use $ globals.gVidDef

    modifyRef saveGameMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 120
                                                           & mfY .~ ((vidDef'^.vdHeight) `div` 2) - 58
                                                           & mfNItems .~ 0
                                                          )

    createSaveStrings

    -- don't include the autosave slot
    saveStrings <- use $ menuGlobals.mgSaveStrings
    setupSaveGameMenuActions saveStrings 0 (maxSaveGames - 1)

  where setupSaveGameMenuActions :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        setupSaveGameMenuActions saveStrings idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let actionRef = saveGameActions V.! idx
              modifyRef actionRef (\v -> v & maGeneric.mcName .~ Just (saveStrings V.! (idx + 1))
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
      saveGameMenu <- readRef saveGameMenuRef
      modifyRef loadGameMenuRef (\v -> v & mfCursor .~ if (saveGameMenu^.mfCursor) - 1 < 0 then 0 else (saveGameMenu^.mfCursor) - 1)
      
    defaultMenuKey saveGameMenuRef key
  )
  
playerConfigMenuInit :: Quake Bool
playerConfigMenuInit = do
    io (putStrLn "Menu.playerConfigMenuInit") >> undefined -- TODO
  
menuSetStatusBar :: Ref MenuFrameworkS -> Maybe B.ByteString -> Quake ()
menuSetStatusBar menuRef str = modifyRef menuRef (\v -> v & mfStatusBar .~ str)
  
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
    vidDef' <- use $ globals.gVidDef

    modifyRef downloadOptionsMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2)
                                                                  & mfNItems .~ 0
                                                                  )

    modifyRef downloadTitleRef (\v -> v & mspGeneric.mcType .~ Constants.mtypeSeparator
                                                            & mspGeneric.mcName .~ Just "Download Options"
                                                            & mspGeneric.mcX .~ 48
                                                            & mspGeneric.mcY .~ 0
                                                            )

    allowDownload <- CVar.variableValue "allow_download"

    modifyRef allowDownloadBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                          & mlGeneric.mcX .~ 0
                                                          & mlGeneric.mcY .~ 20
                                                          & mlGeneric.mcName .~ Just "allow downloading"
                                                          & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadBoxRef)
                                                          & mlItemNames .~ yesNoNames
                                                          & mlCurValue .~ if allowDownload /= 0 then 1 else 0
                                                          )

    allowDownloadMaps <- CVar.variableValue "allow_download_maps"

    modifyRef allowDownloadMapsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                              & mlGeneric.mcX .~ 0
                                                              & mlGeneric.mcY .~ 40
                                                              & mlGeneric.mcName .~ Just "maps"
                                                              & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadMapsBoxRef)
                                                              & mlItemNames .~ yesNoNames
                                                              & mlCurValue .~ if allowDownloadMaps /= 0 then 1 else 0
                                                              )

    allowDownloadPlayers <- CVar.variableValue "allow_download_players"

    modifyRef allowDownloadPlayersBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                                 & mlGeneric.mcX .~ 0
                                                                 & mlGeneric.mcY .~ 50
                                                                 & mlGeneric.mcName .~ Just "player models/skins"
                                                                 & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadPlayersBoxRef)
                                                                 & mlItemNames .~ yesNoNames
                                                                 & mlCurValue .~ if allowDownloadPlayers /= 0 then 1 else 0
                                                                 )

    allowDownloadModels <- CVar.variableValue "allow_download_models"

    modifyRef allowDownloadModelsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                                & mlGeneric.mcX .~ 0
                                                                & mlGeneric.mcY .~ 60
                                                                & mlGeneric.mcName .~ Just "models"
                                                                & mlGeneric.mcCallback .~ Just (downloadCallback allowDownloadModelsBoxRef)
                                                                & mlItemNames .~ yesNoNames
                                                                & mlCurValue .~ if allowDownloadModels /= 0 then 1 else 0
                                                                )

    allowDownloadSounds <- CVar.variableValue "allow_download_sounds"

    modifyRef allowDownloadSoundsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                                & mlGeneric.mcX .~ 0
                                                                & mlGeneric.mcY .~ 70
                                                                & mlGeneric.mcName .~ Just "sounds"
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
    modifyRef downloadOptionsMenuRef (\v -> v & mfCursor %~ (\c -> if c == 0 then 1 else c))
  
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
    vidDef' <- use $ globals.gVidDef

    modifyRef multiplayerMenuRef (\v -> v & mfX .~ ((vidDef'^.vdWidth) `div` 2) - 64
                                                              & mfNItems .~ 0
                                                              )

    modifyRef joinNetworkServerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                   & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                   & maGeneric.mcX .~ 0
                                                                   & maGeneric.mcY .~ 0
                                                                   & maGeneric.mcName .~ Just " join network server"
                                                                   & maGeneric.mcCallback .~ Just (menuJoinServerF^.xcCmd)
                                                                   )

    modifyRef startNetworkServerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                    & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                                    & maGeneric.mcX .~ 0
                                                                    & maGeneric.mcY .~ 10
                                                                    & maGeneric.mcName .~ Just " start network server"
                                                                    & maGeneric.mcCallback .~ Just (menuStartServerF^.xcCmd)
                                                                    )

    modifyRef playerSetupActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfLeftJustify
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 20
                                                             & maGeneric.mcName .~ Just " player setup"
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
    
    vidDef' <- use $ globals.gVidDef

    modifyRef optionsMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                          & mfY .~ ((vidDef'^.vdHeight) `div` 2) - 58
                                                          & mfNItems .~ 0
                                                          )
    
    volumeValue <- liftM (* 10) (CVar.variableValue "s_volume")
    
    modifyRef optionsSfxVolumeSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                                  & msGeneric.mcX .~ 0
                                                                  & msGeneric.mcY .~ 0
                                                                  & msGeneric.mcName .~ Just "effects volume"
                                                                  & msGeneric.mcCallback .~ Just updateVolumeFunc
                                                                  & msMinValue .~ 0
                                                                  & msMaxValue .~ 10
                                                                  & msCurValue .~ volumeValue
                                                                  )
    
    cdnocd <- CVar.variableValue "cd_nocd"
    
    modifyRef optionsCdVolumeBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 10
                                                            & mlGeneric.mcName .~ Just "CD music"
                                                            & mlGeneric.mcCallback .~ Just updateCdVolumeFunc
                                                            & mlItemNames .~ cdMusicItems
                                                            & mlCurValue .~ 1 - truncate cdnocd
                                                            )
                                                            
    modifyRef optionsQualityListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 20
                                                            & mlGeneric.mcName .~ Just "sound"
                                                            & mlGeneric.mcCallback .~ Just updateSoundQualityFunc
                                                            & mlItemNames .~ labels
                                                            )
                                                         
    modifyRef optionsSensitivitySliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                                    & msGeneric.mcX .~ 0
                                                                    & msGeneric.mcY .~ 50
                                                                    & msGeneric.mcName .~ Just "mouse speed"
                                                                    & msGeneric.mcCallback .~ Just mouseSpeedFunc
                                                                    & msMinValue .~ 2
                                                                    & msMaxValue .~ 22
                                                                    )
                                                                    
    modifyRef optionsAlwaysRunBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                             & mlGeneric.mcX .~ 0
                                                             & mlGeneric.mcY .~ 60
                                                             & mlGeneric.mcName .~ Just "always run"
                                                             & mlGeneric.mcCallback .~ Just alwaysRunFunc
                                                             & mlItemNames .~ yesNoNames
                                                             )
                                                             
    modifyRef optionsInvertMouseBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                               & mlGeneric.mcX .~ 0
                                                               & mlGeneric.mcY .~ 70
                                                               & mlGeneric.mcName .~ Just "invert mouse"
                                                               & mlGeneric.mcCallback .~ Just invertMouseFunc
                                                               & mlItemNames .~ yesNoNames
                                                               )
                                                               
    modifyRef optionsLookSpringBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                             & mlGeneric.mcX .~ 0
                                                             & mlGeneric.mcY .~ 80
                                                             & mlGeneric.mcName .~ Just "lookspring"
                                                             & mlGeneric.mcCallback .~ Just lookSpringFunc
                                                             & mlItemNames .~ yesNoNames
                                                             )
                                                             
    modifyRef optionsLookStrafeBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                              & mlGeneric.mcX .~ 0
                                                              & mlGeneric.mcY .~ 90
                                                              & mlGeneric.mcName .~ Just "lookstrafe"
                                                              & mlGeneric.mcCallback .~ Just lookStrafeFunc
                                                              & mlItemNames .~ yesNoNames
                                                              )
                                                              
    modifyRef optionsFreeLookBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 100
                                                            & mlGeneric.mcName .~ Just "free look"
                                                            & mlGeneric.mcCallback .~ Just freeLookFunc
                                                            & mlItemNames .~ yesNoNames
                                                            )
                                                            
    modifyRef optionsCrosshairBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                             & mlGeneric.mcX .~ 0
                                                             & mlGeneric.mcY .~ 110
                                                             & mlGeneric.mcName .~ Just "crosshair"
                                                             & mlGeneric.mcCallback .~ Just crosshairFunc
                                                             & mlItemNames .~ yesNoNames
                                                             )
                                                             
    modifyRef optionsJoystickBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 120
                                                            & mlGeneric.mcName .~ Just "use joystick"
                                                            & mlGeneric.mcCallback .~ Just joystickFunc
                                                            & mlItemNames .~ yesNoNames
                                                            )
                                                            
    modifyRef optionsCustomizeOptionsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                         & maGeneric.mcX .~ 0
                                                                         & maGeneric.mcY .~ 140
                                                                         & maGeneric.mcName .~ Just "customize controls"
                                                                         & maGeneric.mcCallback .~ Just customizeControlsFunc
                                                                         )
                                                                         
    modifyRef optionsDefaultsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                 & maGeneric.mcX .~ 0
                                                                 & maGeneric.mcY .~ 150
                                                                 & maGeneric.mcName .~ Just "reset defaults"
                                                                 & maGeneric.mcCallback .~ Just controlsResetDefaultsFunc
                                                                 )
                                                                 
    modifyRef optionsConsoleActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                & maGeneric.mcX .~ 0
                                                                & maGeneric.mcY .~ 160
                                                                & maGeneric.mcName .~ Just "go to console"
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
    vidDef' <- use $ globals.gVidDef

    modifyRef keysMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                       & mfNItems .~ 0
                                                       & mfCursorDraw .~ Just (keyCursorDrawFunc keysMenuRef)
                                                       )
    
    modifyRef keysAttackActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 0
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysAttackActionRef)
                                                            & maGeneric.mcLocalData._x .~ 0
                                                            & maGeneric.mcName .~ Just ((bindNames V.! 0)^._2)
                                                            )
                                                            
    modifyRef keysChangeWeaponActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 9
                                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysChangeWeaponActionRef)
                                                                  & maGeneric.mcLocalData._x .~ 1
                                                                  & maGeneric.mcName .~ Just ((bindNames V.! 1)^._2)
                                                                  )
                                                                  
    modifyRef keysWalkForwardActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                 & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                 & maGeneric.mcX .~ 0
                                                                 & maGeneric.mcY .~ 18
                                                                 & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysWalkForwardActionRef)
                                                                 & maGeneric.mcLocalData._x .~ 2
                                                                 & maGeneric.mcName .~ Just ((bindNames V.! 2)^._2)
                                                                 )
                                                                 
    modifyRef keysBackpedalActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 27
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysBackpedalActionRef)
                                                               & maGeneric.mcLocalData._x .~ 3
                                                               & maGeneric.mcName .~ Just ((bindNames V.! 3)^._2)
                                                               )
                                                               
    modifyRef keysTurnLeftActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 36 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysTurnLeftActionRef)
                                                              & maGeneric.mcLocalData._x .~ 4
                                                              & maGeneric.mcName .~ Just ((bindNames V.! 4)^._2)
                                                              )
                                                              
    modifyRef keysTurnRightActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 45 
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysTurnRightActionRef)
                                                               & maGeneric.mcLocalData._x .~ 5
                                                               & maGeneric.mcName .~ Just ((bindNames V.! 5)^._2)
                                                               )
                                                               
    modifyRef keysRunActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                         & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                         & maGeneric.mcX .~ 0
                                                         & maGeneric.mcY .~ 54 
                                                         & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysRunActionRef)
                                                         & maGeneric.mcLocalData._x .~ 6
                                                         & maGeneric.mcName .~ Just ((bindNames V.! 6)^._2)
                                                         )
                                                         
    modifyRef keysStepLeftActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 63 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysStepLeftActionRef)
                                                              & maGeneric.mcLocalData._x .~ 7
                                                              & maGeneric.mcName .~ Just ((bindNames V.! 7)^._2)
                                                              )
                                                              
    modifyRef keysStepRightActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 72 
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysStepRightActionRef)
                                                               & maGeneric.mcLocalData._x .~ 8
                                                               & maGeneric.mcName .~ Just ((bindNames V.! 8)^._2)
                                                               )
                                                               
    modifyRef keysSidestepActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 81 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysSidestepActionRef)
                                                              & maGeneric.mcLocalData._x .~ 9
                                                              & maGeneric.mcName .~ Just ((bindNames V.! 9)^._2)
                                                              )
                                                              
    modifyRef keysLookUpActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 90 
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysLookUpActionRef)
                                                            & maGeneric.mcLocalData._x .~ 10
                                                            & maGeneric.mcName .~ Just ((bindNames V.! 10)^._2)
                                                            )
                                                            
    modifyRef keysLookDownActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 99 
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysLookDownActionRef)
                                                              & maGeneric.mcLocalData._x .~ 11
                                                              & maGeneric.mcName .~ Just ((bindNames V.! 11)^._2)
                                                              )
                                                              
    modifyRef keysCenterViewActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                & maGeneric.mcX .~ 0
                                                                & maGeneric.mcY .~ 108 
                                                                & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysCenterViewActionRef)
                                                                & maGeneric.mcLocalData._x .~ 12
                                                                & maGeneric.mcName .~ Just ((bindNames V.! 12)^._2)
                                                                )
                                                              
    modifyRef keysMouseLookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 117
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMouseLookActionRef)
                                                               & maGeneric.mcLocalData._x .~ 13
                                                               & maGeneric.mcName .~ Just ((bindNames V.! 13)^._2)
                                                               )
                                                               
    modifyRef keysKeyboardLookActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 126
                                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysKeyboardLookActionRef)
                                                                  & maGeneric.mcLocalData._x .~ 14
                                                                  & maGeneric.mcName .~ Just ((bindNames V.! 14)^._2)
                                                                  )
                                                                  
    modifyRef keysMoveUpActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 135
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMoveUpActionRef)
                                                            & maGeneric.mcLocalData._x .~ 15
                                                            & maGeneric.mcName .~ Just ((bindNames V.! 15)^._2)
                                                            )
                                                            
    modifyRef keysMoveDownActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                              & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                              & maGeneric.mcX .~ 0
                                                              & maGeneric.mcY .~ 144
                                                              & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysMoveDownActionRef)
                                                              & maGeneric.mcLocalData._x .~ 16
                                                              & maGeneric.mcName .~ Just ((bindNames V.! 16)^._2)
                                                              )
                                                              
    modifyRef keysInventoryActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                               & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                               & maGeneric.mcX .~ 0
                                                               & maGeneric.mcY .~ 153
                                                               & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInventoryActionRef)
                                                               & maGeneric.mcLocalData._x .~ 17
                                                               & maGeneric.mcName .~ Just ((bindNames V.! 17)^._2)
                                                               )
                                                               
    modifyRef keysInvUseActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                            & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                            & maGeneric.mcX .~ 0
                                                            & maGeneric.mcY .~ 162
                                                            & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvUseActionRef)
                                                            & maGeneric.mcLocalData._x .~ 18
                                                            & maGeneric.mcName .~ Just ((bindNames V.! 18)^._2)
                                                            )
                                                            
    modifyRef keysInvDropActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 171
                                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvDropActionRef)
                                                             & maGeneric.mcLocalData._x .~ 19
                                                             & maGeneric.mcName .~ Just ((bindNames V.! 19)^._2)
                                                             )
                                                             
    modifyRef keysInvPrevActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 180
                                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvPrevActionRef)
                                                             & maGeneric.mcLocalData._x .~ 20
                                                             & maGeneric.mcName .~ Just ((bindNames V.! 20)^._2)
                                                             )
                                                             
    modifyRef keysInvNextActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                             & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                             & maGeneric.mcX .~ 0
                                                             & maGeneric.mcY .~ 189
                                                             & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysInvNextActionRef)
                                                             & maGeneric.mcLocalData._x .~ 21
                                                             & maGeneric.mcName .~ Just ((bindNames V.! 21)^._2)
                                                             )
                                                             
    modifyRef keysHelpComputerActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                                  & maGeneric.mcFlags .~ Constants.qmfGrayed
                                                                  & maGeneric.mcX .~ 0
                                                                  & maGeneric.mcY .~ 198
                                                                  & maGeneric.mcOwnerDraw .~ Just (drawKeyBindingFunc keysHelpComputerActionRef)
                                                                  & maGeneric.mcLocalData._x .~ 22
                                                                  & maGeneric.mcName .~ Just ((bindNames V.! 22)^._2)
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
    item <- readRef actionItemRef
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
    vidDef' <- use $ globals.gVidDef

    modifyRef dmOptionsMenuRef (\v -> v & mfX .~ (vidDef'^.vdWidth) `div` 2
                                                            & mfNItems .~ 0
                                                            )

    modifyRef fallsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                  & mlGeneric.mcX .~ 0
                                                  & mlGeneric.mcY .~ 0
                                                  & mlGeneric.mcName .~ Just "falling damage"
                                                  & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just fallsBoxRef))
                                                  & mlItemNames .~ yesNoNames
                                                  & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoFalling == 0 then 1 else 0)
                                                  )

    modifyRef weaponsStayBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                        & mlGeneric.mcX .~ 0
                                                        & mlGeneric.mcY .~ 10
                                                        & mlGeneric.mcName .~ Just "weapons stay"
                                                        & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just weaponsStayBoxRef))
                                                        & mlItemNames .~ yesNoNames
                                                        & mlCurValue .~ (if dmFlagsValue .&. Constants.dfWeaponsStay /= 0 then 1 else 0)
                                                        )

    modifyRef instantPowerUpsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                            & mlGeneric.mcX .~ 0
                                                            & mlGeneric.mcY .~ 20
                                                            & mlGeneric.mcName .~ Just "instant powerups"
                                                            & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just instantPowerUpsBoxRef))
                                                            & mlItemNames .~ yesNoNames
                                                            & mlCurValue .~ (if dmFlagsValue .&. Constants.dfInstantItems /= 0 then 1 else 0)
                                                            )

    modifyRef powerUpsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 30
                                                     & mlGeneric.mcName .~ Just "allow powerups"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just powerUpsBoxRef))
                                                     & mlItemNames .~ yesNoNames
                                                     & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoItems == 0 then 1 else 0)
                                                     )

    modifyRef healthBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                   & mlGeneric.mcX .~ 0
                                                   & mlGeneric.mcY .~ 40
                                                   & mlGeneric.mcName .~ Just "allow health"
                                                   & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just healthBoxRef))
                                                   & mlItemNames .~ yesNoNames
                                                   & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoHealth == 0 then 1 else 0)
                                                   )

    modifyRef armorBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                  & mlGeneric.mcX .~ 0
                                                  & mlGeneric.mcY .~ 50
                                                  & mlGeneric.mcName .~ Just "allow armor"
                                                  & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just armorBoxRef))
                                                  & mlItemNames .~ yesNoNames
                                                  & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoArmor == 0 then 1 else 0)
                                                  )

    modifyRef spawnFarthestBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                          & mlGeneric.mcX .~ 0
                                                          & mlGeneric.mcY .~ 60
                                                          & mlGeneric.mcName .~ Just "spawn farthest"
                                                          & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just spawnFarthestBoxRef))
                                                          & mlItemNames .~ yesNoNames
                                                          & mlCurValue .~ (if dmFlagsValue .&. Constants.dfSpawnFarthest /= 0 then 1 else 0)
                                                          )

    modifyRef sameLevelBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 70
                                                      & mlGeneric.mcName .~ Just "same map"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just sameLevelBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfSameLevel /= 0 then 1 else 0)
                                                      )

    modifyRef forceRespawnBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                         & mlGeneric.mcX .~ 0
                                                         & mlGeneric.mcY .~ 80
                                                         & mlGeneric.mcName .~ Just "force respawn"
                                                         & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just forceRespawnBoxRef))
                                                         & mlItemNames .~ yesNoNames
                                                         & mlCurValue .~ (if dmFlagsValue .&. Constants.dfForceRespawn /= 0 then 1 else 0)
                                                         )

    modifyRef teamPlayBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 90
                                                     & mlGeneric.mcName .~ Just "teamplay"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just teamPlayBoxRef))
                                                     & mlItemNames .~ teamPlayNames
                                                     )

    modifyRef allowExitBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 90
                                                      & mlGeneric.mcName .~ Just "allow exit"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just allowExitBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfAllowExit /= 0 then 1 else 0)
                                                      )

    modifyRef infiniteAmmoBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                         & mlGeneric.mcX .~ 0
                                                         & mlGeneric.mcY .~ 100
                                                         & mlGeneric.mcName .~ Just "infinite ammo"
                                                         & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just infiniteAmmoBoxRef))
                                                         & mlItemNames .~ yesNoNames
                                                         & mlCurValue .~ (if dmFlagsValue .&. Constants.dfInfiniteAmmo /= 0 then 1 else 0)
                                                         )

    modifyRef fixedFovBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 110
                                                     & mlGeneric.mcName .~ Just "fixed FOV"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just fixedFovBoxRef))
                                                     & mlItemNames .~ yesNoNames
                                                     & mlCurValue .~ (if dmFlagsValue .&. Constants.dfFixedFov /= 0 then 1 else 0)
                                                     )

    modifyRef quadDropBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                     & mlGeneric.mcX .~ 0
                                                     & mlGeneric.mcY .~ 120
                                                     & mlGeneric.mcName .~ Just "quad drop"
                                                     & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just quadDropBoxRef))
                                                     & mlItemNames .~ yesNoNames
                                                     & mlCurValue .~ (if dmFlagsValue .&. Constants.dfQuadDrop /= 0 then 1 else 0)
                                                     )

    modifyRef friendlyFireBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                         & mlGeneric.mcX .~ 0
                                                         & mlGeneric.mcY .~ 130
                                                         & mlGeneric.mcName .~ Just "friendly fire"
                                                         & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just friendlyFireBoxRef))
                                                         & mlItemNames .~ yesNoNames
                                                         & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoFriendlyFire == 0 then 1 else 0)
                                                         )

    dev <- FS.developerSearchPath 2

    when (dev == 2) $ do
      modifyRef noMinesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 140
                                                      & mlGeneric.mcName .~ Just "remove mines"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noMinesBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoMines /= 0 then 1 else 0)
                                                      )

      modifyRef noNukesBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                      & mlGeneric.mcX .~ 0
                                                      & mlGeneric.mcY .~ 150
                                                      & mlGeneric.mcName .~ Just "remove nukes"
                                                      & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just noNukesBoxRef))
                                                      & mlItemNames .~ yesNoNames
                                                      & mlCurValue .~ (if dmFlagsValue .&. Constants.dfNoNukes /= 0 then 1 else 0)
                                                      )

      modifyRef stackDoubleBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                          & mlGeneric.mcX .~ 0
                                                          & mlGeneric.mcY .~ 160
                                                          & mlGeneric.mcName .~ Just "2x/4x stacking off"
                                                          & mlGeneric.mcCallback .~ Just (dmFlagCallback (Just stackDoubleBoxRef))
                                                          & mlItemNames .~ yesNoNames
                                                          & mlCurValue .~ (dmFlagsValue .&. Constants.dfNoStackDouble)
                                                          )

      modifyRef noSpheresBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                        & mlGeneric.mcX .~ 0
                                                        & mlGeneric.mcY .~ 170
                                                        & mlGeneric.mcName .~ Just "remove spheres"
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

saveGameCallback :: Ref MenuActionS -> Quake ()
saveGameCallback menuActionRef = do
    action <- readRef menuActionRef
    CBuf.addText ("save save" `B.append` BC.pack (show (action^.maGeneric.mcLocalData._x)) `B.append` "\n")
    forceMenuOff

fieldDraw :: Ref MenuFieldS -> Quake ()
fieldDraw fieldRef = do
    Just renderer <- use $ globals.gRenderer
    field <- readRef fieldRef
    let Just menuRef = field^.mflGeneric.mcParent
    menu <- readRef menuRef
    
    menuDrawStringR2LDark ((field^.mflGeneric.mcX) + (menu^.mfX) + Constants.lColumnOffset) ((field^.mflGeneric.mcY) + (menu^.mfY)) (field^.mflGeneric.mcName)
        
    let tempBuffer = B.drop (field^.mflVisibleOffset) (field^.mflBuffer)
    
    (renderer^.rRefExport.reDrawChar) ((field^.mflGeneric.mcX) + (menu^.mfX) + 16) ((field^.mflGeneric.mcY) + (menu^.mfY) - 4) 18
    (renderer^.rRefExport.reDrawChar) ((field^.mflGeneric.mcX) + (menu^.mfX) + 16) ((field^.mflGeneric.mcY) + (menu^.mfY) + 4) 24
    
    (renderer^.rRefExport.reDrawChar) ((field^.mflGeneric.mcX) + (menu^.mfX) + 24 + 8 * (field^.mflVisibleLength)) ((field^.mflGeneric.mcY) + (menu^.mfY) - 4) 20
    (renderer^.rRefExport.reDrawChar) ((field^.mflGeneric.mcX) + (menu^.mfX) + 24 + 8 * (field^.mflVisibleLength)) ((field^.mflGeneric.mcY) + (menu^.mfY) + 4) 26
    
    drawVisible renderer menu field 0 (field^.mflVisibleLength)
    
    menuDrawString ((field^.mflGeneric.mcX) + (menu^.mfX) + 24) ((field^.mflGeneric.mcY) + (menu^.mfY)) (Just tempBuffer)
    
    menuItemRef <- menuItemAtCursor menuRef
    
    when (menuItemRef == Just (MenuFieldRef fieldRef)) $ do
      let offset = if (field^.mflVisibleOffset) /= 0
                     then field^.mflVisibleLength
                     else field^.mflCursor
      
      ms <- Timer.milliseconds
      
      let ch = if (ms `div` 250) .&. 1 /= 0
                 then 11
                 else ord ' '
                 
      (renderer^.rRefExport.reDrawChar) ((field^.mflGeneric.mcX) + (menu^.mfX) + 8 * (offset + 2) + 8) ((field^.mflGeneric.mcY) + (menu^.mfY)) ch
    
  where drawVisible :: Renderer -> MenuFrameworkS -> MenuFieldS -> Int -> Int -> Quake ()
        drawVisible renderer menu field idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              (renderer^.rRefExport.reDrawChar) ((field^.mflGeneric.mcX) + (menu^.mfX) + 24 + 8 * idx) ((field^.mflGeneric.mcY) + (menu^.mfY) - 4) 19
              (renderer^.rRefExport.reDrawChar) ((field^.mflGeneric.mcX) + (menu^.mfX) + 24 + 8 * idx) ((field^.mflGeneric.mcY) + (menu^.mfY) + 4) 25
              drawVisible renderer menu field (idx + 1) maxIdx

menuDrawString :: Int -> Int -> Maybe B.ByteString -> Quake ()
menuDrawString _ _ Nothing = return ()
menuDrawString x y (Just str) = do
    Just renderer <- use $ globals.gRenderer
    drawString renderer 0 (B.length str)

  where drawString :: Renderer -> Int -> Int -> Quake ()
        drawString renderer idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ch = ord (BC.index str idx)
              (renderer^.rRefExport.reDrawChar) (x + idx * 8) y ch
              drawString renderer (idx + 1) maxIdx

menuDrawStringDark :: Int -> Int -> Maybe B.ByteString -> Quake ()
menuDrawStringDark _ _ Nothing = return ()
menuDrawStringDark x y (Just str) = do
    Just renderer <- use $ globals.gRenderer
    drawString renderer 0 (B.length str)

  where drawString :: Renderer -> Int -> Int -> Quake ()
        drawString renderer idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ch = ord (BC.index str idx) + 128
              (renderer^.rRefExport.reDrawChar) (x + idx * 8) y ch
              drawString renderer (idx + 1) maxIdx

menuDrawStringR2L :: Int -> Int -> Maybe B.ByteString -> Quake ()
menuDrawStringR2L _ _ Nothing = return ()
menuDrawStringR2L x y (Just str) = do
    Just renderer <- use $ globals.gRenderer
    drawString renderer 0 (B.length str)

  where drawString :: Renderer -> Int -> Int -> Quake ()
        drawString renderer idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ch = ord (BC.index str (maxIdx - idx - 1))
              (renderer^.rRefExport.reDrawChar) (x - idx * 8) y ch
              drawString renderer (idx + 1) maxIdx

menuDrawStringR2LDark :: Int -> Int -> Maybe B.ByteString -> Quake ()
menuDrawStringR2LDark _ _ Nothing = return ()
menuDrawStringR2LDark x y (Just str) = do
    Just renderer <- use $ globals.gRenderer
    drawString renderer 0 (B.length str)
    
  where drawString :: Renderer -> Int -> Int -> Quake ()
        drawString renderer idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ch = ord (BC.index str (maxIdx - idx - 1)) + 128
              (renderer^.rRefExport.reDrawChar) (x - idx * 8) y ch
              drawString renderer (idx + 1) maxIdx

sliderDraw :: Ref MenuSliderS -> Quake ()
sliderDraw sliderRef = do
    slider <- readRef sliderRef
    let Just menuRef = slider^.msGeneric.mcParent
    menu <- readRef menuRef

    menuDrawStringR2LDark ((slider^.msGeneric.mcX) + (menu^.mfX) + Constants.lColumnOffset) ((slider^.msGeneric.mcY) + (menu^.mfY)) (slider^.msGeneric.mcName)

    let r = ((slider^.msCurValue) - (slider^.msMinValue)) / ((slider^.msMaxValue) - (slider^.msMinValue))
        range = if | r < 0 -> 0
                   | r > 1 -> 1
                   | otherwise -> r

    Just renderer <- use $ globals.gRenderer

    (renderer^.rRefExport.reDrawChar) ((slider^.msGeneric.mcX) + (menu^.mfX) + Constants.rColumnOffset) ((slider^.msGeneric.mcY) + (menu^.mfY)) 128

    drawSliderRange renderer menu slider 0 sliderRange

    (renderer^.rRefExport.reDrawChar) (Constants.rColumnOffset + (slider^.msGeneric.mcX) + sliderRange * 8 + (menu^.mfX) + 8) ((slider^.msGeneric.mcY) + (menu^.mfY)) 130
    (renderer^.rRefExport.reDrawChar) (8 + Constants.rColumnOffset + (menu^.mfX) + (slider^.msGeneric.mcX) + truncate (fromIntegral (sliderRange - 1) * 8 * range)) ((slider^.msGeneric.mcY) + (menu^.mfY)) 131

  where drawSliderRange :: Renderer -> MenuFrameworkS -> MenuSliderS -> Int -> Int -> Quake ()
        drawSliderRange renderer menu slider idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              (renderer^.rRefExport.reDrawChar) (Constants.rColumnOffset + (slider^.msGeneric.mcX) + idx * 8 + (menu^.mfX) + 8) ((slider^.msGeneric.mcY) + (menu^.mfY)) 129
              drawSliderRange renderer menu slider (idx + 1) maxIdx

menuListDraw :: Ref MenuListS -> Quake ()
menuListDraw menuListRef = do
    menuList <- readRef menuListRef
    let Just menuRef = menuList^.mlGeneric.mcParent
    menu <- readRef menuRef

    menuDrawStringR2LDark ((menuList^.mlGeneric.mcX) + (menu^.mfX) + Constants.lColumnOffset) ((menuList^.mlGeneric.mcY) + (menu^.mfY)) (menuList^.mlGeneric.mcName)

    Just renderer <- use $ globals.gRenderer
    (renderer^.rRefExport.reDrawFill) ((menuList^.mlGeneric.mcX) - 112 + (menu^.mfX)) ((menu^.mfY) + (menuList^.mlGeneric.mcY) + 10 * (menuList^.mlCurValue) + 10) 128 10 16

    V.imapM_ (drawListItem menu menuList) (menuList^.mlItemNames)

  where drawListItem :: MenuFrameworkS -> MenuListS -> Int -> B.ByteString -> Quake ()
        drawListItem menu menuList idx name = do
          let y = idx * 10
          menuDrawStringR2LDark ((menuList^.mlGeneric.mcX) + (menu^.mfX) + Constants.lColumnOffset) ((menuList^.mlGeneric.mcY) + (menu^.mfY) + y + 10) (Just name)

spinControlDraw :: Ref MenuListS -> Quake ()
spinControlDraw menuListRef = do
    menuList <- readRef menuListRef
    let Just menuRef = menuList^.mlGeneric.mcParent
    menu <- readRef menuRef

    menuDrawStringR2LDark ((menuList^.mlGeneric.mcX) + (menu^.mfX) + Constants.lColumnOffset) ((menuList^.mlGeneric.mcY) + (menu^.mfY)) (menuList^.mlGeneric.mcName)

    let name = (menuList^.mlItemNames) V.! (menuList^.mlCurValue)

    if '\n' `BC.elem` name
      then
        menuDrawString (Constants.rColumnOffset + (menuList^.mlGeneric.mcX) + (menu^.mfX)) ((menuList^.mlGeneric.mcY) + (menu^.mfY)) (Just name)
      else do
        let line1 = Lib.leftFrom name '\n'
            line2 = Lib.rightFrom name '\n'
            line2' = case '\n' `BC.elemIndex` line2 of
                       Nothing -> line2
                       Just pos -> B.take pos line2

        menuDrawString (Constants.rColumnOffset + (menuList^.mlGeneric.mcX) + (menu^.mfX)) ((menuList^.mlGeneric.mcY) + (menu^.mfY)) (Just line1)
        menuDrawString (Constants.rColumnOffset + (menuList^.mlGeneric.mcX) + (menu^.mfX)) ((menuList^.mlGeneric.mcY) + (menu^.mfY) + 10) (Just line2')

actionDraw :: Ref MenuActionS -> Quake ()
actionDraw actionRef = do
    action <- readRef actionRef
    let Just menuRef = action^.maGeneric.mcParent
    menu <- readRef menuRef
    
    let drawingFunc = if (action^.maGeneric.mcFlags) .&. Constants.qmfLeftJustify /= 0
                        then if (action^.maGeneric.mcFlags) .&. Constants.qmfGrayed /= 0
                               then menuDrawStringDark
                               else menuDrawString
                        else if (action^.maGeneric.mcFlags) .&. Constants.qmfGrayed /= 0
                               then menuDrawStringR2LDark
                               else menuDrawStringR2L
    
    drawingFunc ((action^.maGeneric.mcX) + (menu^.mfX) + Constants.lColumnOffset) ((action^.maGeneric.mcY) + (menu^.mfY)) (action^.maGeneric.mcName)
    
    case action^.maGeneric.mcOwnerDraw of
      Nothing -> return ()
      Just ownerDraw -> ownerDraw

separatorDraw :: Ref MenuSeparatorS -> Quake ()
separatorDraw separatorRef = do
    separator <- readRef separatorRef
    let Just menuRef = separator^.mspGeneric.mcParent
    menu <- readRef menuRef
    
    menuDrawStringR2LDark ((separator^.mspGeneric.mcX) + (menu^.mfX)) ((separator^.mspGeneric.mcY) + (menu^.mfY)) (separator^.mspGeneric.mcName)

downloadCallback :: Ref MenuListS -> Quake ()
downloadCallback menuListRef = do
    item <- readRef menuListRef

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
    slider <- readRef optionsSfxVolumeSliderRef
    CVar.setValueF "s_volume" ((slider^.msCurValue) / 10)

updateCdVolumeFunc :: Quake ()
updateCdVolumeFunc = do
    slider <- readRef optionsCdVolumeBoxRef
    CVar.setValueI "cd_nocd" (1 - (slider^.mlCurValue))

updateSoundQualityFunc :: Quake ()
updateSoundQualityFunc = do
    drivers <- S.getDriverNames
    qualityList <- readRef optionsQualityListRef
    let selectedDriver = drivers V.! (qualityList^.mlCurValue)
    currentDriver <- S.getDriverName
    Just renderer <- use $ globals.gRenderer
    
    if selectedDriver == currentDriver
      then
        renderer^.rRefExport.reEndFrame
      
      else do
        CVar.set "s_impl" selectedDriver
        
        drawTextBox 8 (120 - 48) 36 3
        menuPrint (16 + 16) (120 - 48 + 8) "Restarting the sound system. This"
        menuPrint (16 + 16) (120 - 48 + 16) "could take up to a minute, so"
        menuPrint (16 + 16) (120 - 48 + 24) "please be patient."
        
        -- the text box won't show up unless we do a buffer swap
        renderer^.rRefExport.reEndFrame
        
        CL.sndRestartF^.xcCmd

mouseSpeedFunc :: Quake ()
mouseSpeedFunc = do
    slider <- readRef optionsSensitivitySliderRef
    CVar.setValueF "sensitivity" ((slider^.msCurValue) / 2)

alwaysRunFunc :: Quake ()
alwaysRunFunc = do
    box <- readRef optionsAlwaysRunBoxRef
    CVar.setValueI "cl_run" (box^.mlCurValue)

invertMouseFunc :: Quake ()
invertMouseFunc = do
    pitchValue <- liftM (^.cvValue) mPitchCVar
    CVar.setValueF "m_pitch" (negate pitchValue)

lookSpringFunc :: Quake ()
lookSpringFunc = do
    lookSpringValue <- liftM (^.cvValue) lookSpringCVar
    CVar.setValueF "lookspring" (1 - lookSpringValue)
  
lookStrafeFunc :: Quake ()
lookStrafeFunc = do
    lookStrafeValue <- liftM (^.cvValue) lookStrafeCVar
    CVar.setValueF "lookstrafe" (1 - lookStrafeValue)

freeLookFunc :: Quake ()
freeLookFunc = do
    box <- readRef optionsFreeLookBoxRef
    CVar.setValueI "freelook" (box^.mlCurValue)

crosshairFunc :: Quake ()
crosshairFunc = do
    box <- readRef optionsCrosshairBoxRef
    CVar.setValueI "crosshair" (box^.mlCurValue)

joystickFunc :: Quake ()
joystickFunc = do
    box <- readRef optionsJoystickBoxRef
    CVar.setValueI "in_joystick" (box^.mlCurValue)
  
customizeControlsFunc :: Quake ()
customizeControlsFunc = menuKeysF^.xcCmd

controlsResetDefaultsFunc :: Quake ()
controlsResetDefaultsFunc = do
    CBuf.addText "exec default.cfg\n"
    CBuf.execute
    
    controlsSetMenuItemValues

consoleFunc :: Quake ()
consoleFunc = do
    -- the proper way to do this is probably to have ToggleConsole_f
    -- accept a parameter
    attractLoop <- use $ globals.gCl.csAttractLoop
    
    if attractLoop
      then
        CBuf.addText "killserver\n"
        
      else do
        Key.clearTyping
        Console.clearNotify
        
        forceMenuOff
        globals.gCls.csKeyDest .= Constants.keyConsole

controlsSetMenuItemValues :: Quake ()
controlsSetMenuItemValues = do
    io (putStrLn "Menu.controlsSetMenuItemValues") >> undefined -- TODO

keyCursorDrawFunc :: Ref MenuFrameworkS -> Quake ()
keyCursorDrawFunc menuRef = do
    menu <- readRef menuRef
    bindGrab <- use $ menuGlobals.mgBindGrab
    
    ch <- if bindGrab
            then
              return (ord '=')
            else do
              ms <- Timer.milliseconds
              return (12 + ((ms `div` 250) .&. 1))
    
    Just renderer <- use $ globals.gRenderer
    (renderer^.rRefExport.reDrawChar) (menu^.mfX) ((menu^.mfY) + 9 * (menu^.mfCursor)) ch

drawKeyBindingFunc :: Ref MenuActionS -> Quake ()
drawKeyBindingFunc _ = do
    io (putStrLn "Menu.drawKeyBindingFunc") >> undefined -- TODO

loadGameCallback :: Ref MenuActionS -> Quake ()
loadGameCallback actionRef = do
    action <- readRef actionRef

    saveValid <- use $ menuGlobals.mgSaveValid

    when (saveValid V.! (action^.maGeneric.mcLocalData._x)) $
      CBuf.addText ("load save" `B.append` BC.pack (show (action^.maGeneric.mcLocalData._x)) `B.append` "\n") -- IMPROVE

    forceMenuOff
  
creditsMenuDraw :: XCommandT
creditsMenuDraw =
  XCommandT "Menu.creditsMenuDraw" (do
    io (putStrLn "Menu.creditsMenuDraw") >> undefined -- TODO
  )
  
creditsKey :: KeyFuncT
creditsKey =
  KeyFuncT "Menu.creditsKey" (\key -> do
    when (key == KeyConstants.kEscape) popMenu
    return (Just menuOutSound)
  )
  
dmOptionsMenuDraw :: XCommandT
dmOptionsMenuDraw =
  XCommandT "Menu.dmOptionsMenuDraw" (menuDraw dmOptionsMenuRef)
  
dmOptionsMenuKey :: KeyFuncT
dmOptionsMenuKey =
  KeyFuncT "Menu.dmOptionsMenuKey" (\key ->
    defaultMenuKey dmOptionsMenuRef key
  )

keyBindingFunc :: Ref MenuActionS -> Quake ()
keyBindingFunc actionRef = do
    action <- readRef actionRef
    let command = (bindNames V.! (action^.maGeneric.mcLocalData._x))^._1
    (_, key) <- findKeysForCommand command
    
    when (key /= -1) $
      unbindCommand command
    
    menuGlobals.mgBindGrab .= True
    
    menuSetStatusBar keysMenuRef (Just "press a key or button for this action")

findKeysForCommand :: B.ByteString -> Quake (Int, Int)
findKeysForCommand command = do
    kb <- use $ globals.gKeyBindings
    return $ findKeyBindings kb (-1) 0 256
    
  where findKeyBindings :: V.Vector (Maybe B.ByteString) -> Int -> Int -> Int -> (Int, Int)
        findKeyBindings kb key1 idx maxIdx
          | idx >= maxIdx = (key1, -1)
          | otherwise = if (kb V.! idx) == Just command
                          then
                            if key1 == -1
                              then findKeyBindings kb idx (idx + 1) maxIdx
                              else (key1, idx)
                          else
                            findKeyBindings kb key1 (idx + 1) maxIdx

unbindCommand :: B.ByteString -> Quake ()
unbindCommand command = do
    kb <- use $ globals.gKeyBindings
    unbind kb 0 256
    
  where unbind :: V.Vector (Maybe B.ByteString) -> Int -> Int -> Quake ()
        unbind kb idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              when ((kb V.! idx) == Just command) $
                Key.setBinding idx (Just "")
                
              unbind kb (idx + 1) maxIdx

dmFlagCallback :: Maybe (Ref MenuListS) -> Quake ()
dmFlagCallback _ = do
    io (putStrLn "Menu.dmFlagCallback") >> undefined -- TODO

rulesChangeFunc :: Quake ()
rulesChangeFunc = do
    io (putStrLn "Menu.rulesChangeFunc") >> undefined -- TODO

dmOptionsFunc :: Quake ()
dmOptionsFunc = do
    rulesBox <- readRef rulesBoxRef

    unless ((rulesBox^.mlCurValue) == 1) $
      menuDMOptionsF^.xcCmd
    
startServerActionFunc :: Quake ()
startServerActionFunc = do
    io (putStrLn "Menu.startServerActionFunc") >> undefined -- TODO

drawTextBox :: Int -> Int -> Int -> Int -> Quake ()
drawTextBox _ _ _ _ = do
    io (putStrLn "Menu.drawTextBox") >> undefined -- TODO

menuPrint :: Int -> Int -> B.ByteString -> Quake ()
menuPrint cx cy str =
    drawString 0 (B.length str)
    
  where drawString :: Int -> Int -> Quake ()
        drawString idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              drawCharacter (cx + 8 * idx) cy (ord (BC.index str idx) + 128)
              drawString (idx + 1) maxIdx

{- ================ DrawCharacter ================
- 
- Draws one solid graphics character cx and cy are in 320*240 coordinates,
- and will be centered on higher res screens.
-}
drawCharacter :: Int -> Int -> Int -> Quake ()
drawCharacter cx cy num = do
    Just renderer <- use $ globals.gRenderer
    vidDef' <- use $ globals.gVidDef
    (renderer^.rRefExport.reDrawChar) (cx + (((vidDef'^.vdWidth) - 320) `shiftR` 1)) (cy + (((vidDef'^.vdHeight) - 240) `shiftR` 1)) num

menuSlideItem :: Ref MenuFrameworkS -> Int -> Quake ()
menuSlideItem menuRef dir = do
    menuItemRef <- menuItemAtCursor menuRef
    
    mItem <- case menuItemRef of
               Nothing -> return Nothing
               Just itemRef -> menuItemCommon itemRef >>= return . Just
               
    case mItem of
      Nothing ->
        return ()
      
      Just item -> do
        let Just itemRef = menuItemRef
        
        if | (item^.mcType) == Constants.mtypeSlider -> do
               let MenuSliderRef sliderRef = itemRef
               sliderDoSlide sliderRef dir
            
           | (item^.mcType) == Constants.mtypeSpinControl -> do
               let MenuListRef listRef = itemRef
               spinControlDoSlide listRef dir
            
           | otherwise ->
               return ()

sliderDoSlide :: Ref MenuSliderS -> Int -> Quake ()
sliderDoSlide menuSliderRef dir = do
    slider <- readRef menuSliderRef
    modifyRef menuSliderRef (\v -> v & msCurValue %~ (updateCurValue slider))

    case slider^.msGeneric.mcCallback of
      Nothing -> return ()
      Just callback -> callback

  where updateCurValue :: MenuSliderS -> Float -> Float
        updateCurValue slider curValue =
          let newValue = curValue + fromIntegral dir
              in if | newValue > (slider^.msMaxValue) -> slider^.msMaxValue
                    | newValue < (slider^.msMinValue) -> slider^.msMinValue
                    | otherwise -> newValue

spinControlDoSlide :: Ref MenuListS -> Int -> Quake ()
spinControlDoSlide menuListRef dir = do
    menuList <- readRef menuListRef
    modifyRef menuListRef (\v -> v & mlCurValue %~ (updateCurValue menuList))
    
    case menuList^.mlGeneric.mcCallback of
      Nothing -> return ()
      Just callback -> callback
    
  where updateCurValue :: MenuListS -> Int -> Int
        updateCurValue menuList curValue =
          let newValue = curValue + dir
          in if | newValue < 0 -> 0
                | newValue >= V.length (menuList^.mlItemNames) -> curValue
                | otherwise -> newValue

menuSelectItem :: Ref MenuFrameworkS -> Quake Bool
menuSelectItem menuRef = do
    menuItemRef <- menuItemAtCursor menuRef
    
    mItem <- case menuItemRef of
               Nothing -> return Nothing
               Just itemRef -> menuItemCommon itemRef >>= return . Just
    
    case mItem of
      Nothing ->
        return False
      
      Just item -> do
        let Just itemRef = menuItemRef
        
        if | (item^.mcType) == Constants.mtypeField -> do
               let MenuFieldRef fieldRef = itemRef
               fieldDoEnter fieldRef
               
           | (item^.mcType) == Constants.mtypeAction -> do
               let MenuActionRef actionRef = itemRef
               actionDoEnter actionRef >> return True
               
           | otherwise -> 
               return False

fieldDoEnter :: Ref MenuFieldS -> Quake Bool
fieldDoEnter fieldRef = do
    field <- readRef fieldRef
    
    case field^.mflGeneric.mcCallback of
      Nothing -> return False
      Just callback -> callback >> return True
    
actionDoEnter :: Ref MenuActionS -> Quake ()
actionDoEnter actionRef = do
    action <- readRef actionRef
    
    case action^.maGeneric.mcCallback of
      Nothing -> return ()
      Just callback -> callback

menuDrawStatusBar :: Maybe B.ByteString -> Quake ()
menuDrawStatusBar mStatusBar = do
    Just renderer <- use $ globals.gRenderer
    vidDef' <- use $ globals.gVidDef

    case mStatusBar of
      Just statusBar -> do
        let len = B.length statusBar
            maxRow = (vidDef'^.vdHeight) `div` 8
            maxCol = (vidDef'^.vdWidth) `div` 8
            col = maxCol `div` 2 - len `div` 2

        (renderer^.rRefExport.reDrawFill) 0 ((vidDef'^.vdHeight) - 8) (vidDef'^.vdWidth) 8 4
        menuDrawString (col * 8) ((vidDef'^.vdHeight) - 8) mStatusBar

      Nothing ->
        (renderer^.rRefExport.reDrawFill) 0 ((vidDef'^.vdHeight) - 8) (vidDef'^.vdWidth) 8 0
