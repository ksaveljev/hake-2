module Game.GItem where

import qualified Data.ByteString as B

import Game.GItemArmor

data GItem =
  GItem { gItemId              :: Int
        , gItemClassName       :: B.ByteString
        , gItemPickup          :: IO () -- TODO: ???
        , gItemUse             :: IO () -- TODO: ???
        , gItemDrop            :: IO () -- TODO: ???
        , gItemWeaponThink     :: IO () -- TODO: ???
        , gItemPickupSound     :: B.ByteString
        , gItemWorldModel      :: B.ByteString
        , gItemWorldModelFlags :: Int
        , gItemViewModel       :: B.ByteString
        , gItemIcon            :: B.ByteString
        , gItemPickupName      :: B.ByteString
        , gItemCountWidth      :: Int
        , gItemQuantity        :: Int
        , gItemAmmo            :: B.ByteString
        , gItemFlags           :: Int
        , gItemWeaponModel     :: Int
        , gItemInfo            :: GItemArmor
        , gItemTag             :: Int
        , gItemPrecaches       :: B.ByteString
        , gItemIndex           :: Int
        }
