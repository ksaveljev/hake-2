{-# LANGUAGE OverloadedStrings #-}
module Client.Menu where

import Control.Lens (use, preuse, ix, (.=), (+=), (^.))
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
    Cmd.addCommand "menu_main" (Just menuMain)
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

-- IMPROVE: instead of Int use "Reference" newtype ?
addItem :: Int -> Int -> Quake ()
addItem menuIdx menuItemIdx = do
    Just nItems <- preuse $ menuGlobals.mgMenuFrameworks.ix menuIdx.mfNItems

    when (nItems == 0) $
      menuGlobals.mgMenuFrameworks.ix menuIdx.mfNSlots .= 0

    when (nItems < Constants.maxMenuItems) $ do
      menuGlobals.mgMenuFrameworks.ix menuIdx.mfItems.ix nItems .= Just (MenuItemReference menuItemIdx)
      Just item <- preuse $ menuGlobals.mgMenuItems.ix menuItemIdx
      case item of
        MenuListS _ _ _ -> menuGlobals.mgMenuItems.ix menuItemIdx.mlGeneric.mcParent .= Just (MenuFrameworkSReference menuIdx)
        MenuSliderS _ _ _ _ _ -> menuGlobals.mgMenuItems.ix menuItemIdx.msGeneric.mcParent .= Just (MenuFrameworkSReference menuIdx)
        MenuActionS _ -> menuGlobals.mgMenuItems.ix menuItemIdx.maGeneric.mcParent .= Just (MenuFrameworkSReference menuIdx)
      menuGlobals.mgMenuFrameworks.ix menuIdx.mfNItems += 1

    tallySlots menuIdx >>= \v ->
      menuGlobals.mgMenuFrameworks.ix menuIdx.mfNSlots .= v

-- IMPROVE: instead of Int use "Reference" newtype ?
center :: Int -> Quake ()
center menuIdx = do
    Just height <- preuse $ menuGlobals.mgMenuFrameworks.ix menuIdx.mfY
    h <- use $ globals.vidDef.vdHeight

    menuGlobals.mgMenuFrameworks.ix menuIdx.mfY .= (h - (height + 10)) `div` 2

-- IMPROVE: instead of Int use "Reference" newtype ?
tallySlots :: Int -> Quake Int
tallySlots menuIdx = do
    Just menu <- preuse $ menuGlobals.mgMenuFrameworks.ix menuIdx

    itemsNum <- V.mapM numberOfItems (menu^.mfItems)
    return $ V.foldl' (+) 0 itemsNum

  where numberOfItems :: Maybe MenuItemReference -> Quake Int
        numberOfItems Nothing = return 0
        numberOfItems (Just (MenuItemReference menuItemIdx)) = do
          Just menuItem <- preuse $ menuGlobals.mgMenuItems.ix menuItemIdx

          case menuItem of
            MenuListS _ _ _ -> case menuItem^.mlItemNames of
                                 Nothing -> return 0
                                 _ -> return (V.length $ fromJust (menuItem^.mlItemNames))
            MenuSliderS _ _ _ _ _ -> return 1
            MenuActionS _ -> return 1

menuMain :: XCommandT
menuMain = io (putStrLn "Menu.menuMain") >> undefined -- TODO

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
