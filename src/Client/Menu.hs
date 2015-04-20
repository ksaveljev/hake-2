module Client.Menu where

import Control.Lens (use, preuse, ix, (.=), (+=), (^.))
import Control.Monad (when)
import Data.Maybe (fromJust)
import qualified Data.Vector as V

import Quake
import QuakeState
import qualified Constants

init :: Quake ()
init = io (putStrLn "Menu.init") >> undefined -- TODO

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
