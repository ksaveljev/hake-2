{-# LANGUAGE OverloadedStrings #-}
module Client.Menu where

import Control.Lens (zoom, use, preuse, ix, (.=), (+=), (^.), (%=), (&), (.~), (%~), (+~))
import Control.Monad (when, void)
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import qualified Sound.S as S

maxMenuDepth :: Int
maxMenuDepth = 8

menuInSound :: B.ByteString
menuInSound = "misc/menu1.wav"

menuMoveSound :: B.ByteString
menuMoveSound = "misc/menu2.wav"

menuOutSound :: B.ByteString
menuOutSound = "misc/menu3.wav"

init :: Quake ()
init = do
    Cmd.addCommand "menu_main" (Just menuMainF)
    Cmd.addCommand "menu_game" (Just menuGame)
    Cmd.addCommand "menu_loadgame" (Just menuLoadGame)
    Cmd.addCommand "menu_savegame" (Just menuSaveGame)
    Cmd.addCommand "menu_joinserver" (Just menuJoinServer)
    Cmd.addCommand "menu_addressbook" (Just menuAddressBook)
    Cmd.addCommand "menu_startserver" (Just menuStartServer)
    Cmd.addCommand "menu_dmoptions" (Just menuDMOptions)
    Cmd.addCommand "menu_playerconfig" (Just menuPlayerConfig)
    Cmd.addCommand "menu_downloadoptions" (Just menuDownloadOptions)
    Cmd.addCommand "menu_credits" (Just menuCredits)
    Cmd.addCommand "menu_multiplayer" (Just menuMultiplayer)
    Cmd.addCommand "menu_video" (Just menuVideo)
    Cmd.addCommand "menu_options" (Just menuOptions)
    Cmd.addCommand "menu_keys" (Just menuKeys)
    Cmd.addCommand "menu_quit" (Just menuQuit)

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

      menuGlobals.mgLayers %= (`V.snoc` (MenuLayerT (Just draw) (Just key)))

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
    io (putStrLn "Menu.mainDrawF") >> undefined -- TODO
  )

mainKeyF :: KeyFuncT
mainKeyF =
  KeyFuncT "Menu.mainKeyF" (\key -> do
    io (putStrLn "Menu.mainKeyF") >> undefined -- TODO
  )

menuGame :: XCommandT
menuGame =
  XCommandT "Menu.menuGame" (do
    io (putStrLn "Menu.menuGame") >> undefined -- TODO
  )

menuLoadGame :: XCommandT
menuLoadGame =
  XCommandT "Menu.menuLoadGame" (do
    io (putStrLn "Menu.menuLoadGame") >> undefined -- TODO
  )

menuSaveGame :: XCommandT
menuSaveGame =
  XCommandT "Menu.menuSaveGame" (do
    io (putStrLn "Menu.menuSaveGame") >> undefined -- TODO
  )

menuJoinServer :: XCommandT
menuJoinServer =
  XCommandT "Menu.menuJoinServer" (do
    io (putStrLn "Menu.menuJoinServer") >> undefined -- TODO
  )

menuAddressBook :: XCommandT
menuAddressBook =
  XCommandT "Menu.menuAddressBook" (do
    io (putStrLn "Menu.menuAddressBook") >> undefined -- TODO
  )

menuStartServer :: XCommandT
menuStartServer =
  XCommandT "Menu.menuStartServer" (do
    io (putStrLn "Menu.menuStartServer") >> undefined -- TODO
  )

menuDMOptions :: XCommandT
menuDMOptions =
  XCommandT "Menu.menuDMOptions" (do
    io (putStrLn "Menu.menuDMOptions") >> undefined -- TODO
  )

menuPlayerConfig :: XCommandT
menuPlayerConfig =
  XCommandT "Menu.menuPlayerConfig" (do
    io (putStrLn "Menu.menuPlayerConfig") >> undefined -- TODO
  )

menuDownloadOptions :: XCommandT
menuDownloadOptions =
  XCommandT "Menu.menuDownloadOptions" (do
    io (putStrLn "Menu.menuDownloadOptions") >> undefined -- TODO
  )

menuCredits :: XCommandT
menuCredits =
  XCommandT "Menu.menuCredits" (do
    io (putStrLn "Menu.menuCredits") >> undefined -- TODO
  )

menuMultiplayer :: XCommandT
menuMultiplayer =
  XCommandT "Menu.menuMultiplayer" (do
    io (putStrLn "Menu.menuMultiplayer") >> undefined -- TODO
  )

menuVideo :: XCommandT
menuVideo =
  XCommandT "Menu.menuVideo" (do
    io (putStrLn "Menu.menuVideo") >> undefined -- TODO
  )

menuOptions :: XCommandT
menuOptions =
  XCommandT "Menu.menuOptions" (do
    io (putStrLn "Menu.menuOptions") >> undefined -- TODO
  )

menuKeys :: XCommandT
menuKeys =
  XCommandT "Menu.menuKeys" (do
    io (putStrLn "Menu.menuKeys") >> undefined -- TODO
  )

menuQuit :: XCommandT
menuQuit =
  XCommandT "Menu.menuQuit" (do
    io (putStrLn "Menu.menuQuit") >> undefined -- TODO
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
menuKeyDown _ = do
    io (putStrLn "Menu.menuKeyDown") >> undefined -- TODO

forceMenuOff :: Quake ()
forceMenuOff = do
    io (putStrLn "Menu.forceMenuOff") >> undefined -- TODO

addToServerList :: NetAdrT -> B.ByteString -> Quake ()
addToServerList _ _ = do
    io (putStrLn "Menu.addToServerList") >> undefined -- TODO
