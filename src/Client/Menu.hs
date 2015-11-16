{-# LANGUAGE OverloadedStrings #-}
module Client.Menu where

import Control.Lens (use, preuse, ix, (.=), (+=), (^.), (%=), (&), (.~), (%~), (+~))
import Control.Monad (when)
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Game.Cmd as Cmd
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

menuMainF :: XCommandT
menuMainF = io (putStrLn "Menu.menuMainF") >> undefined -- TODO

menuGame :: XCommandT
menuGame = io (putStrLn "Menu.menuGame") >> undefined -- TODO

menuLoadGame :: XCommandT
menuLoadGame = io (putStrLn "Menu.menuLoadGame") >> undefined -- TODO

menuSaveGame :: XCommandT
menuSaveGame = io (putStrLn "Menu.menuSaveGame") >> undefined -- TODO

menuJoinServer :: XCommandT
menuJoinServer = io (putStrLn "Menu.menuJoinServer") >> undefined -- TODO

menuAddressBook :: XCommandT
menuAddressBook = io (putStrLn "Menu.menuAddressBook") >> undefined -- TODO

menuStartServer :: XCommandT
menuStartServer = io (putStrLn "Menu.menuStartServer") >> undefined -- TODO

menuDMOptions :: XCommandT
menuDMOptions = io (putStrLn "Menu.menuDMOptions") >> undefined -- TODO

menuPlayerConfig :: XCommandT
menuPlayerConfig = io (putStrLn "Menu.menuPlayerConfig") >> undefined -- TODO

menuDownloadOptions :: XCommandT
menuDownloadOptions = io (putStrLn "Menu.menuDownloadOptions") >> undefined -- TODO

menuCredits :: XCommandT
menuCredits = io (putStrLn "Menu.menuCredits") >> undefined -- TODO

menuMultiplayer :: XCommandT
menuMultiplayer = io (putStrLn "Menu.menuMultiplayer") >> undefined -- TODO

menuVideo :: XCommandT
menuVideo = io (putStrLn "Menu.menuVideo") >> undefined -- TODO

menuOptions :: XCommandT
menuOptions = io (putStrLn "Menu.menuOptions") >> undefined -- TODO

menuKeys :: XCommandT
menuKeys = io (putStrLn "Menu.menuKeys") >> undefined -- TODO

menuQuit :: XCommandT
menuQuit = io (putStrLn "Menu.menuQuit") >> undefined -- TODO

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
      drawFunc

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
