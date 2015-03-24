{-# LANGUAGE OverloadedStrings #-}
module Game.GameItemList where

import Data.Sequence (Seq, fromList)

import Game.GItemT
import qualified Constants
import qualified Game.GameItems as GameItems

itemList :: Seq GItemT
itemList =
    fromList [
               -- leave index 0 alone
               GItemT "" Nothing Nothing Nothing Nothing
                      "" "" 0 Nothing "" "" 0 0 Nothing 0 0 Nothing 0 "" 0

             --
             -- ARMOR
             --

             {-
              - QUAKED item_armor_body (.3 .3 1) (-16 -16 -16) (16 16 16)
              -}
             , GItemT "item_armor_body" (Just GameItems.pickupArmor) Nothing Nothing Nothing
                      "misc/ar1_pkup.wav" "models/items/armor/body/tris.md2"
                      Constants.efRotate Nothing
                      -- icon
                      "i_bodyarmor"
                      -- pickup
                      "Body Armor"
                      -- width
                      3 0 Nothing Constants.itArmor 0 (Just GameItems.bodyArmorInfo)
                      Constants.armorBody
                      -- precache
                      ""
                      -- index
                      1

             {-
              - QUAKED item_armor_combat (.3 .3 1) (-16 -16 -16) (16 16 16)
              -}
             , GItemT "item_armor_combat" (Just GameItems.pickupArmor) Nothing Nothing Nothing
                      "misc/ar1_pkup.wav" "models/items/armor/combat/tris.md2"
                      Constants.efRotate Nothing
                      -- icon
                      "i_combatarmor"
                      -- pickup
                      "Combat Armor"
                      -- width
                      3 0 Nothing Constants.itArmor 0 (Just GameItems.combatArmorInfo)
                      Constants.armorCombat
                      -- precache
                      ""
                      -- index
                      2

             {-
              - QUAKED item_armor_jacket (.3 .3 1) (-16 -16 -16) (16 16 16)
              -}
             , GItemT "item_armor_jacket" (Just GameItems.pickupArmor) Nothing Nothing Nothing
                      "misc/ar1_pkup.wav" "models/items/armor/jacket/tris.md2"
                      Constants.efRotate Nothing
                      -- icon
                      "i_jacketarmor"
                      -- pickup
                      "Jacket Armor"
                      -- width
                      3 0 Nothing Constants.itArmor 0 (Just GameItems.jacketArmorInfo)
                      Constants.armorJacket
                      -- precache
                      ""
                      -- index
                      3

             {-
              - QUAKED item_armor_shard (.3 .3 1) (-16 -16 -16) (16 16 16)
              -}
             , GItemT "item_armor_shard" (Just GameItems.pickupArmor) Nothing Nothing Nothing
                      "misc/ar2_pkup.wav" "models/items/armor/shard/tris.md2"
                      Constants.efRotate Nothing
                      -- icon
                      "i_jacketarmor"
                      -- pickup
                      "Armor Shard"
                      -- width
                      3 0 Nothing Constants.itArmor 0 Nothing Constants.armorShard
                      -- precache
                      ""
                      -- index
                      4

             {-
              - QUAKED item_power_screen (.3 .3 1) (-16 -16 -16) (16 16 16)
              -}
             , GItemT "item_power_screen" (Just GameItems.pickupPowerArmor) (Just GameItems.usePowerArmor)
                      (Just GameItems.dropPowerArmor) Nothing "misc/ar3_pkup.wav"
                      "models/items/armor/screen/tris.md2" Constants.efRotate
                      Nothing
                      -- icon
                      "i_powerscreen"
                      -- pickup
                      "Power Screen"
                      -- width
                      0 60 Nothing Constants.itArmor 0 Nothing 0
                      -- precache
                      ""
                      -- index
                      5

             {-
              - QUAKED item_power_shield (.3 .3 1) (-16 -16 -16) (16 16 16)
              -}
             , GItemT "item_power_shield" (Just GameItems.pickupPowerArmor) (Just GameItems.usePowerArmor)
                      (Just GameItems.dropPowerArmor) Nothing "misc/ar3_pkup.wav"
                      "models/items/armor/shield/tris.md2" Constants.efRotate
                      Nothing
                      -- icon
                      "i_powershield"
                      -- pickup
                      "Power Shield"
                      -- width
                      0 60 Nothing Constants.itArmor 0 Nothing 0
                      -- precache
                      "misc/power2.wav misc/power1.wav"
                      -- index
                      6

             --
             -- WEAPONS
             --
             -- TODO
             ]
