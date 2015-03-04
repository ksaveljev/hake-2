{-# LANGUAGE TemplateHaskell #-}
module Game.GItemT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Game.GItemArmorT

data GItemT =
  GItemT { _giId              :: Int
         , _giClassName       :: B.ByteString
         , _giPickup          :: IO () -- TODO: ???
         , _giUse             :: IO () -- TODO: ???
         , _giDrop            :: IO () -- TODO: ???
         , _giWeaponThink     :: IO () -- TODO: ???
         , _giPickupSound     :: B.ByteString
         , _giWorldModel      :: B.ByteString
         , _giWorldModelFlags :: Int
         , _giViewModel       :: B.ByteString
         , _giIcon            :: B.ByteString
         , _giPickupName      :: B.ByteString
         , _giCountWidth      :: Int
         , _giQuantity        :: Int
         , _giAmmo            :: B.ByteString
         , _giFlags           :: Int
         , _giWeaponModel     :: Int
         , _giInfo            :: GItemArmorT
         , _giTag             :: Int
         , _giPrecaches       :: B.ByteString
         , _giIndex           :: Int
         }

makeLenses ''GItemT
