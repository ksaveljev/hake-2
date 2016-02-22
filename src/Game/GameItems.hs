module Game.GameItems
  ( itemList
  ) where

import qualified Constants
import           Game.GItemT
import qualified Game.PlayerWeapon as PlayerWeapon
import           Types

import           Data.Bits ((.|.))
import qualified Data.Vector as V

jacketArmorInfo :: GItemArmorT
jacketArmorInfo =
  GItemArmorT { _giaBaseCount        = 25
              , _giaMaxCount         = 50
              , _giaNormalProtection = 0.3
              , _giaEnergyProtection = 0
              , _giaArmor            = Constants.armorJacket
              }

combatArmorInfo :: GItemArmorT
combatArmorInfo =
  GItemArmorT { _giaBaseCount        = 50
              , _giaMaxCount         = 100
              , _giaNormalProtection = 0.6
              , _giaEnergyProtection = 0.3
              , _giaArmor            = Constants.armorCombat
              }

bodyArmorInfo :: GItemArmorT
bodyArmorInfo =
  GItemArmorT { _giaBaseCount        = 100
              , _giaMaxCount         = 200
              , _giaNormalProtection = 0.8
              , _giaEnergyProtection = 0.6
              , _giaArmor            = Constants.armorBody
              }

pickupArmor :: EntInteract
pickupArmor = error "GameItems.pickupArmor" -- TODO

pickupPowerArmor :: EntInteract
pickupPowerArmor = error "GameItems.pickupPowerArmor" -- TODO

usePowerArmor :: ItemUse
usePowerArmor = error "GameItems.usePowerArmor" -- TODO

dropPowerArmor :: ItemDrop
dropPowerArmor = error "GameItems.dropPowerArmor" -- TODO

pickupAmmo :: EntInteract
pickupAmmo = error "GameItems.pickupAmmo" -- TODO

dropAmmo :: ItemDrop
dropAmmo = error "GameItems.dropAmmo" -- TODO

useQuad :: ItemUse
useQuad = error "GameItems.useQuad" -- TODO

pickupPowerup :: EntInteract
pickupPowerup = error "GameItems.pickupPowerup" -- TODO

dropGeneral :: ItemDrop
dropGeneral = error "GameItems.dropGeneral" -- TODO

useInvulnerability :: ItemUse
useInvulnerability = error "GameItems.useInvulnerability" -- TODO

useSilencer :: ItemUse
useSilencer = error "GameItems.useSilencer" -- TODO

useBreather :: ItemUse
useBreather = error "GameItems.useBreather" -- TODO

useEnviroSuit :: ItemUse
useEnviroSuit = error "GameItems.useEnviroSuit" -- TODO

pickupAncientHead :: EntInteract
pickupAncientHead = error "GameItems.pickupAncientHead" -- TODO

pickupAdrenaline :: EntInteract
pickupAdrenaline = error "GameItems.pickupAdrenaline" -- TODO

pickupBandolier :: EntInteract
pickupBandolier = error "GameItems.pickupBandolier" -- TODO

pickupPack :: EntInteract
pickupPack = error "GameItems.pickupPack" -- TODO

pickupKey :: EntInteract
pickupKey = error "GameItems.pickupKey" -- TODO

pickupHealth :: EntInteract
pickupHealth = error "GameItems.pickupHealth" -- TODO

itemList :: V.Vector GItemT
itemList = V.fromList
  [
    -- leave index 0 alone
    GItemT "" Nothing Nothing Nothing Nothing
           Nothing Nothing 0 Nothing Nothing Nothing
           0 0 Nothing 0 0 Nothing 0 "" 0
 
  --
  -- ARMOR
  --
 
  {-
   - QUAKED item_armor_body (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_armor_body" (Just pickupArmor) Nothing Nothing Nothing
           (Just "misc/ar1_pkup.wav") (Just "models/items/armor/body/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "i_bodyarmor")
           -- pickup
           (Just "Body Armor")
           -- width
           3 0 Nothing Constants.itArmor 0 (Just bodyArmorInfo)
           Constants.armorBody
           -- precache
           ""
           -- index
           1
 
  {-
   - QUAKED item_armor_combat (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_armor_combat" (Just pickupArmor) Nothing Nothing Nothing
           (Just "misc/ar1_pkup.wav") (Just "models/items/armor/combat/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "i_combatarmor")
           -- pickup
           (Just "Combat Armor")
           -- width
           3 0 Nothing Constants.itArmor 0 (Just combatArmorInfo)
           Constants.armorCombat
           -- precache
           ""
           -- index
           2
 
  {-
   - QUAKED item_armor_jacket (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_armor_jacket" (Just pickupArmor) Nothing Nothing Nothing
           (Just "misc/ar1_pkup.wav") (Just "models/items/armor/jacket/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "i_jacketarmor")
           -- pickup
           (Just "Jacket Armor")
           -- width
           3 0 Nothing Constants.itArmor 0 (Just jacketArmorInfo)
           Constants.armorJacket
           -- precache
           ""
           -- index
           3
 
  {-
   - QUAKED item_armor_shard (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_armor_shard" (Just pickupArmor) Nothing Nothing Nothing
           (Just "misc/ar2_pkup.wav") (Just "models/items/armor/shard/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "i_jacketarmor")
           -- pickup
           (Just "Armor Shard")
           -- width
           3 0 Nothing Constants.itArmor 0 Nothing Constants.armorShard
           -- precache
           ""
           -- index
           4
 
  {-
   - QUAKED item_power_screen (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_power_screen" (Just pickupPowerArmor) (Just usePowerArmor)
           (Just dropPowerArmor) Nothing (Just "misc/ar3_pkup.wav")
           (Just "models/items/armor/screen/tris.md2") Constants.efRotate
           Nothing
           -- icon
           (Just "i_powerscreen")
           -- pickup
           (Just "Power Screen")
           -- width
           0 60 Nothing Constants.itArmor 0 Nothing 0
           -- precache
           ""
           -- index
           5
 
  {-
   - QUAKED item_power_shield (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_power_shield" (Just pickupPowerArmor) (Just usePowerArmor)
           (Just dropPowerArmor) Nothing (Just "misc/ar3_pkup.wav")
           (Just "models/items/armor/shield/tris.md2") Constants.efRotate
           Nothing
           -- icon
           (Just "i_powershield")
           -- pickup
           (Just "Power Shield")
           -- width
           0 60 Nothing Constants.itArmor 0 Nothing 0
           -- precache
           "misc/power2.wav misc/power1.wav"
           -- index
           6
 
  --
  -- WEAPONS
  --
 
  {-
   - weapon_blaster (.3 .3 1) (-16 -16 -16) (16 16 16) always owned,
   - never in the world
   -}
  , GItemT "weapon_blaster" Nothing (Just PlayerWeapon.useWeapon) Nothing
           (Just PlayerWeapon.weaponBlaster) (Just "misc/w_pkup.wav") Nothing 0
           (Just "models/weapons/v_blast/tris.md2")
           -- icon
           (Just "w_blaster")
           -- pickup
           (Just "Blaster")
           0 0 Nothing (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapBlaster Nothing 0
           -- precache
           "weapons/blastf1a.wav misc/lasfly.wav"
           -- index
           7
 
  {-
   - QUAKED weapon_shotgun (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_shotgun" (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon) (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponShotgun) (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_shotg/tris.md2") Constants.efRotate
           (Just "models/weapons/v_shotg/tris.md2")
           -- icon
           (Just "w_shotgun")
           -- pickup
           (Just "Shotgun")
           0 1 (Just "Shells") (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapShotgun Nothing 0
           -- precache
           "weapons/shotgf1b.wav weapons/shotgr1b.wav"
           -- index
           8
 
  {-
   - QUAKED weapon_supershotgun (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_supershotgun" (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon) (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponSuperShotgun) (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_shotg2/tris.md2") Constants.efRotate
           (Just "models/weapons/v_shotg2/tris.md2")
           -- icon
           (Just "w_sshotgun")
           -- pickup
           (Just "Super Shotgun")
           0 2 (Just "Shells") (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapSuperShotgun Nothing 0
           -- precache
           "weapons/sshotf1b.wav"
           -- index
           9
 
  {-
   - QUAKED weapon_machinegun (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_machinegun"
           (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon)
           (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponMachinegun)
           (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_machn/tris.md2")
           Constants.efRotate
           (Just "models/weapons/v_machn/tris.md2")
           -- icon
           (Just "w_machinegun")
           -- pickup
           (Just "Machinegun")
           0
           1
           (Just "Bullets")
           (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapMachinegun
           Nothing
           0
           -- precache
           "weapons/machgf1b.wav weapons/machgf2b.wav weapons/machgf3b.wav weapons/machgf4b.wav weapons/machgf5b.wav"
           -- index
           10
 
  {-
   - QUAKED weapon_chaingun (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_chaingun"
           (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon)
           (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponChaingun)
           (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_chain/tris.md2")
           Constants.efRotate
           (Just "models/weapons/v_chain/tris.md2")
           -- icon
           (Just "w_chaingun")
           -- pickup
           (Just "Chaingun")
           0
           1
           (Just "Bullets")
           (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapChaingun
           Nothing
           0
           -- precache
           "weapons/chngnu1a.wav weapons/chngnl1a.wav weapons/machgf3b.wav` weapons/chngnd1a.wav"
           -- index
           11
 
  {-
   - QUAKED ammo_grenades (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "ammo_grenades"
           (Just pickupAmmo)
           (Just PlayerWeapon.useWeapon)
           (Just dropAmmo)
           (Just PlayerWeapon.weaponGrenade)
           (Just "misc/am_pkup.wav")
           (Just "models/items/ammo/grenades/medium/tris.md2")
           0
           (Just "models/weapons/v_handgr/tris.md2")
           -- icon
           (Just "a_grenades")
           -- pickup
           (Just "Grenades")
           -- width
           3
           5
           (Just "grenades")
           (Constants.itAmmo .|. Constants.itWeapon)
           Constants.weapGrenades
           Nothing
           Constants.ammoGrenades
           -- precache
           "weapons/hgrent1a.wav weapons/hgrena1b.wav weapons/hgrenc1b.wav weapons/hgrenb1a.wav weapons/hgrenb2a.wav"
           -- index
           12
 
  {-
   - QUAKED weapon_grenadelauncher (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_grenadelauncher"
           (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon)
           (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponGrenadeLauncher)
           (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_launch/tris.md2")
           Constants.efRotate
           (Just "models/weapons/v_launch/tris.md2")
           -- icon
           (Just "w_glauncher")
           -- pickup
           (Just "Grenade Launcher")
           0
           1
           (Just "Grenades")
           (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapGrenadeLauncher
           Nothing
           0
           -- precache
           "models/objects/grenade/tris.md2 weapons/grenlf1a.wav weapons/grenlr1b.wav weapons/grenlb1b.wav"
           -- index
           13
 
  {-
   - QUAKED weapon_rocketlauncher (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_rocketlauncher"
           (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon)
           (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponRocketLauncher)
           (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_rocket/tris.md2")
           Constants.efRotate
           (Just "models/weapons/v_rocket/tris.md2")
           -- icon
           (Just "w_rlauncher")
           -- pickup
           (Just "Rocket Launcher")
           0
           1
           (Just "Rockets")
           (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapRocketLauncher
           Nothing
           0
           -- precache
           "models/objects/rocket/tris.md2 weapons/rockfly.wav weapons/rocklf1a.wav weapons/rocklr1b.wav models/objects/debris2/tris.md2"
           -- index
           14
 
  {-
   - QUAKED weapon_hyperblaster (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_hyperblaster"
           (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon)
           (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponHyperBlaster)
           (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_hyperb/tris.md2")
           Constants.efRotate
           (Just "models/weapons/v_hyperb/tris.md2")
           -- icon
           (Just "w_hyperblaster")
           -- pickup
           (Just "HyperBlaster")
           0
           1
           (Just "Cells")
           (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapHyperBlaster
           Nothing
           0
           -- precache
           "weapons/hyprbu1a.wav weapons/hyprbl1a.wav weapons/hyprbf1a.wav weapons/hyprbd1a.wav misc/lasfly.wav"
           -- index
           15
 
  {-
   - QUAKED weapon_railgun (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_railgun" (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon) (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponRailgun) (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_rail/tris.md2") Constants.efRotate
           (Just "models/weapons/v_rail/tris.md2")
           -- icon
           (Just "w_railgun")
           -- pickup
           (Just "Railgun")
           0 1 (Just "Slugs") (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapRailgun Nothing 0
           -- precache
           "weapons/rg_hum.wav"
           -- index
           16
 
  {-
   - QUAKED weapon_bfg (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "weapon_bfg"
           (Just PlayerWeapon.pickupWeapon)
           (Just PlayerWeapon.useWeapon)
           (Just PlayerWeapon.dropWeapon)
           (Just PlayerWeapon.weaponBFG)
           (Just "misc/w_pkup.wav")
           (Just "models/weapons/g_bfg/tris.md2")
           Constants.efRotate
           (Just "models/weapons/v_bfg/tris.md2")
           -- icon
           (Just "w_bfg")
           -- pickup
           (Just "BFG10K")
           0
           50
           (Just "Cells")
           (Constants.itWeapon .|. Constants.itStayCoop)
           Constants.weapBFG
           Nothing
           0
           -- precache
           "sprites/s_bfg1.sp2 sprites/s_bfg2.sp2 sprites/s_bfg3.sp2 weapons/bfg__f1y.wav weapons/bfg__l1a.wav weapons/bfg__x1b.wav weapons/bfg_hum.wav"
           -- index
           17
 
  --
  -- AMMO ITEMS
  --
 
  {-
   - QUAKED ammo_shells (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "ammo_shells" (Just pickupAmmo) Nothing (Just dropAmmo) Nothing
           (Just "misc/am_pkup.wav")
           (Just "models/items/ammo/shells/medium/tris.md2") 0 Nothing
           -- icon
           (Just "a_shells")
           -- pickup
           (Just "Shells")
           -- width
           3 10 Nothing Constants.itAmmo 0 Nothing Constants.ammoShells
           -- precache
           ""
           -- index
           18
 
  {-
   - QUAKED ammo_bullets (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "ammo_bullets" (Just pickupAmmo) Nothing (Just dropAmmo) Nothing
           (Just "misc/am_pkup.wav")
           (Just "models/items/ammo/bullets/medium/tris.md2") 0 Nothing
           -- icon
           (Just "a_bullets")
           -- pickup
           (Just "Bullets")
           -- width
           3 50 Nothing Constants.itAmmo 0 Nothing
           Constants.ammoBullets
           -- precache
           ""
           -- index
           19
 
  {-
   - QUAKED ammo_cells (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "ammo_cells" (Just pickupAmmo) Nothing (Just dropAmmo) Nothing
           (Just "misc/am_pkup.wav")
           (Just "models/items/ammo/cells/medium/tris.md2") 0 Nothing
           -- icon
           (Just "a_cells")
           -- pickup
           (Just "Cells")
           -- width
           3 50 Nothing Constants.itAmmo 0 Nothing Constants.ammoCells
           -- precache
           ""
           -- index
           20
 
  {-
   - QUAKED ammo_rockets (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "ammo_rockets" (Just pickupAmmo) Nothing (Just dropAmmo) Nothing
           (Just "misc/am_pkup.wav")
           (Just "models/items/ammo/rockets/medium/tris.md2") 0 Nothing
           -- icon
           (Just "a_rockets")
           -- pickup
           (Just "Rockets")
           -- width
           3 5 Nothing Constants.itAmmo 0 Nothing Constants.ammoRockets
           -- precache
           ""
           -- index
           21
 
  {-
   - QUAKED ammo_slugs (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "ammo_slugs" (Just pickupAmmo) Nothing (Just dropAmmo) Nothing
           (Just "misc/am_pkup.wav")
           (Just "models/items/ammo/slugs/medium/tris.md2") 0 Nothing
           -- icon
           (Just "a_slugs")
           -- pickup
           (Just "Slugs")
           -- width
           3 10 Nothing Constants.itAmmo 0 Nothing Constants.ammoSlugs
           -- precache
           ""
           -- index
           22
 
  --
  -- POWERUP ITEMS
  --
 
  {-
   - QUAKED item_quad (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_quad" (Just pickupPowerup) (Just useQuad)
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/quaddama/tris.md2") Constants.efRotate Nothing
           -- icon
           (Just "p_quad")
           -- pickup
           (Just "Quad Damage")
           -- width
           2 60 Nothing Constants.itPowerup 0 Nothing 0
           -- precache
           "items/damage.wav items/damage2.wav items/damage3.wav"
           -- index
           23
 
  {-
   - QUAKED item_invulnerability (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_invulnerability" (Just pickupPowerup)
           (Just useInvulnerability) (Just dropGeneral) Nothing
           (Just "items/pkup.wav") (Just "models/items/invulner/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "p_invulnerability")
           -- pickup
           (Just "Invulnerability")
           -- width
           2 300 Nothing Constants.itPowerup 0 Nothing 0
           -- precache
           "items/protect.wav items/protect2.wav items/protect4.wav"
           -- index
           24
 
  {-
   - QUAKED item_silencer (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_silencer" (Just pickupPowerup) (Just useSilencer)
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/silencer/tris.md2") Constants.efRotate Nothing
           -- icon
           (Just "p_silencer")
           -- pickup
           (Just "Silencer")
           -- width
           2 60 Nothing Constants.itPowerup 0 Nothing 0
           -- precache
           ""
           -- index
           25
 
  {-
   - QUAKED item_breather (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_breather" (Just pickupPowerup) (Just useBreather)
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/breather/tris.md2") Constants.efRotate Nothing
           -- icon
           (Just "p_rebreather")
           -- pickup
           (Just "Rebreather")
           -- width
           2 60 Nothing (Constants.itStayCoop .|. Constants.itPowerup) 0
           Nothing 0
           -- precache
           "items/airout.wav"
           -- index
           26
 
  {-
   - QUAKED item_enviro (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_enviro" (Just pickupPowerup) (Just useEnviroSuit)
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/enviro/tris.md2") Constants.efRotate Nothing
           -- icon
           (Just "p_envirosuit")
           -- pickup
           (Just "Environment Suit")
           -- width
           2 60 Nothing (Constants.itStayCoop .|. Constants.itPowerup) 0
           Nothing 0
           -- precache
           "items/airout.wav"
           -- index
           27
 
  {-
   - QUAKED item_ancient_head (.3 .3 1) (-16 -16 -16) (16 16 16)
   - Special item that gives +2 to maximum health
   -}
  , GItemT "item_ancient_head" (Just pickupAncientHead) Nothing Nothing
           Nothing (Just "items/pkup.wav") (Just "models/items/c_head/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "i_fixme")
           -- pickup
           (Just "Ancient Head")
           -- width
           2 60 Nothing 0 0 Nothing 0
           -- precache
           ""
           -- index
           28
 
  {-
   - QUAKED item_adrenaline (.3 .3 1) (-16 -16 -16) (16 16 16) gives
   - +1 to maximum health
   -}
  , GItemT "item_adrenaline" (Just pickupAdrenaline) Nothing Nothing Nothing
           (Just "items/pkup.wav") (Just "models/items/adrenal/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "p_adrenaline")
           -- pickup
           (Just "Adrenaline")
           -- width
           2 60 Nothing 0 0 Nothing 0
           -- precache
           ""
           -- index
           29
 
  {-
   - QUAKED item_bandolier (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_bandolier" (Just pickupBandolier) Nothing Nothing Nothing
           (Just "items/pkup.wav") (Just "models/items/band/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "p_bandolier")
           -- pickup
           (Just "Bandolier")
           -- width
           2 60 Nothing 0 0 Nothing 0
           -- precache
           ""
           -- index
           30
 
  {-
   - QUAKED item_pack (.3 .3 1) (-16 -16 -16) (16 16 16)
   -}
  , GItemT "item_pack" (Just pickupPack) Nothing Nothing Nothing
           (Just "items/pkup.wav") (Just "models/items/pack/tris.md2")
           Constants.efRotate Nothing
           -- icon
           (Just "i_pack")
           -- pickup
           (Just "Ammo Pack")
           -- width
           2 180 Nothing 0 0 Nothing 0
           -- precache
           ""
           -- index
           31
 
  --
  -- KEYS
  --
 
  {-
   - QUAKED key_data_cd (0 .5 .8) (-16 -16 -16) (16 16 16) key for
   - computer centers
   -}
  , GItemT "key_data_cd" (Just pickupKey) Nothing (Just dropGeneral)
           Nothing (Just "items/pkup.wav")
           (Just "models/items/keys/data_cd/tris.md2") Constants.efRotate
           Nothing (Just "k_datacd") (Just "Data CD") 2 0 Nothing
           (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           32
 
  {-
   - QUAKED key_power_cube (0 .5 .8) (-16 -16 -16) (16 16 16)
   - TRIGGER_SPAWN NO_TOUCH warehouse circuits
   -}
  , GItemT "key_power_cube" (Just pickupKey) Nothing
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/keys/power/tris.md2") Constants.efRotate
           Nothing (Just "k_powercube") (Just "Power Cube") 2 0 Nothing
           (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           33
 
  {-
   - QUAKED key_pyramid (0 .5 .8) (-16 -16 -16) (16 16 16) key for the
   - entrance of jail3
   -}
  , GItemT "key_pyramid" (Just pickupKey) Nothing (Just dropGeneral)
           Nothing (Just "items/pkup.wav")
           (Just "models/items/keys/pyramid/tris.md2") Constants.efRotate
           Nothing (Just "k_pyramid") (Just "Pyramid Key") 2 0 Nothing
           (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           34
 
  {-
   - QUAKED key_data_spinner (0 .5 .8) (-16 -16 -16) (16 16 16) key
   - for the city computer
   -}
  , GItemT "key_data_spinner" (Just pickupKey) Nothing
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/keys/spinner/tris.md2") Constants.efRotate
           Nothing (Just "k_dataspin") (Just "Data Spinner") 2 0 Nothing
           (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           35
 
  {-
   - QUAKED key_pass (0 .5 .8) (-16 -16 -16) (16 16 16) security pass
   - for the security level
   -}
  , GItemT "key_pass" (Just pickupKey) Nothing (Just dropGeneral)
           Nothing (Just "items/pkup.wav") (Just "models/items/keys/pass/tris.md2")
           Constants.efRotate Nothing (Just "k_security") (Just "Security Pass") 2
           0 Nothing (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           36
 
  {-
   - QUAKED key_blue_key (0 .5 .8) (-16 -16 -16) (16 16 16) normal
   - door key - blue
   -}
  , GItemT "key_blue_key" (Just pickupKey) Nothing
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/keys/key/tris.md2") Constants.efRotate Nothing
           (Just "k_bluekey") (Just "Blue Key") 2 0 Nothing
           (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           37
 
  {-
   - QUAKED key_red_key (0 .5 .8) (-16 -16 -16) (16 16 16) normal door
   - key - red
   -}
  , GItemT "key_red_key" (Just pickupKey) Nothing (Just dropGeneral)
           Nothing (Just "items/pkup.wav")
           (Just "models/items/keys/red_key/tris.md2") Constants.efRotate
           Nothing (Just "k_redkey") (Just "Red Key") 2 0 Nothing
           (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           38
 
  {-
   - QUAKED key_commander_head (0 .5 .8) (-16 -16 -16) (16 16 16) tank
   - commander's head
   -}
  , GItemT "key_commander_head" (Just pickupKey) Nothing
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/monsters/commandr/head/tris.md2") Constants.efGib
           Nothing
           -- icon
           (Just "k_comhead")
           -- pickup
           (Just "Commander's Head")
           -- width
           2 0 Nothing (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           39
 
  {-
   - QUAKED key_airstrike_target (0 .5 .8) (-16 -16 -16) (16 16 16)
   - tank commander's head
   -}
  , GItemT "key_airstrike_target" (Just pickupKey) Nothing
           (Just dropGeneral) Nothing (Just "items/pkup.wav")
           (Just "models/items/keys/target/tris.md2") Constants.efRotate
           Nothing
           -- icon
           (Just "i_airstrike")
           -- pickup
           (Just "Airstrike Marker")
           -- width
           2 0 Nothing (Constants.itStayCoop .|. Constants.itKey) 0 Nothing 0
           -- precache
           ""
           -- index
           40
 
  , GItemT "" (Just pickupHealth) Nothing Nothing Nothing
           (Just "items/pkup.wav") Nothing 0 Nothing
           -- icon
           (Just "i_health")
           -- pickup
           (Just "Health")
           -- width
           3 0 Nothing 0 0 Nothing 0
           -- precache
           "items/s_health.wav items/n_health.wav items/l_health.wav items/m_health.wav"
           -- index
           41
  ]