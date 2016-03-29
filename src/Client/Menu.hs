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
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.XCommandT
import           QuakeRef
import           QuakeState
import           Render.Renderer
import qualified Sound.S as S
import           Types
import           Util.Binary (encode)

import           Control.Applicative (liftA2)
import           Control.Lens (use, ix, (^.), (.=), (%=), (-=), (&), (.~), (%~), (+~))
import           Control.Monad (void, join, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (ord)
import qualified Data.Vector as V

mainItems :: Int
mainItems = 5

menuInSound :: B.ByteString
menuInSound = "misc/menu1.wav"

menuMoveSound :: B.ByteString
menuMoveSound = "misc/menu2.wav"

menuOutSound :: B.ByteString
menuOutSound = "misc/menu3.wav"

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
menuCreditsF = error "Menu.menuCreditsF" -- TODO

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
pushMenu = error "Menu.pushMenu" -- TODO

mainDrawF :: XCommandT
mainDrawF = error "Menu.mainDrawF" -- TODO

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
gameMenuDrawF = error "Menu.gameMenuDrawF" -- TODO

gameMenuKeyF :: KeyFuncT
gameMenuKeyF = KeyFuncT "Menu.gameMenuKeyF" (defaultMenuKey gameMenuRef)

optionsMenuInit :: Quake ()
optionsMenuInit = error "Menu.optionsMenuInit" -- TODO

optionsMenuDrawF :: XCommandT
optionsMenuDrawF = XCommandT "Menu.optionsMenuDrawF" $
  do banner "m_banner_options"
     menuAdjustCursor optionsMenuRef 1
     menuDraw optionsMenuRef

optionsMenuKey :: KeyFuncT
optionsMenuKey = KeyFuncT "Menu.optionsMenuKey" (defaultMenuKey optionsMenuRef)

loadGameMenuInit :: Quake ()
loadGameMenuInit = error "Menu.loadGameMenuInit" -- TODO

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
saveGameMenuInit = error "Menu.saveGameMenuInit" -- TODO

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
joinServerMenuInit = error "Menu.joinServerMenuInit" -- TODO

joinServerMenuDrawF :: XCommandT
joinServerMenuDrawF = XCommandT "Menu.joinServerMenuDrawF" $
  do banner "m_banner_join_server"
     menuDraw joinServerMenuRef

joinServerMenuKeyF :: KeyFuncT
joinServerMenuKeyF = KeyFuncT "Menu.joinServerMenuKeyF" (defaultMenuKey joinServerMenuRef)

addressBookMenuInit :: Quake ()
addressBookMenuInit = error "Menu.addressBookMenuInit" -- TODO

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
startServerMenuInit = error "Menu.startServerMenuInit" -- TODO

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
dmOptionsMenuInit = error "Menu.dmOptionsMenuInit" -- TODO

dmOptionsMenuDrawF :: XCommandT
dmOptionsMenuDrawF = XCommandT "Menu.dmOptionsMenuDrawF" (menuDraw dmOptionsMenuRef)
  
dmOptionsMenuKeyF :: KeyFuncT
dmOptionsMenuKeyF = KeyFuncT "Menu.dmOptionsMenuKey" (defaultMenuKey dmOptionsMenuRef)

multiplayerMenuInit :: Quake ()
multiplayerMenuInit = error "Menu.multiplayerMenuInit" -- TODO

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
keysMenuInit = error "Menu.keysMenuInit" -- TODO

downloadOptionsMenuInit :: Quake ()
downloadOptionsMenuInit = error "Menu.downloadOptionsMenuInit" -- TODO

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