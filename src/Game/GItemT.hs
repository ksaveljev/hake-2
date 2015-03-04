module Game.GItemT where

import qualified Data.ByteString as B

import Game.GItemArmorT

data GItemT =
  GItemT { giId              :: Int
         , giClassName       :: B.ByteString
         , giPickup          :: IO () -- TODO: ???
         , giUse             :: IO () -- TODO: ???
         , giDrop            :: IO () -- TODO: ???
         , giWeaponThink     :: IO () -- TODO: ???
         , giPickupSound     :: B.ByteString
         , giWorldModel      :: B.ByteString
         , giWorldModelFlags :: Int
         , giViewModel       :: B.ByteString
         , giIcon            :: B.ByteString
         , giPickupName      :: B.ByteString
         , giCountWidth      :: Int
         , giQuantity        :: Int
         , giAmmo            :: B.ByteString
         , giFlags           :: Int
         , giWeaponModel     :: Int
         , giInfo            :: GItemArmorT
         , giTag             :: Int
         , giPrecaches       :: B.ByteString
         , giIndex           :: Int
         }
