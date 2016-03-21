{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Constants where

import           Data.Bits ((.|.),shiftL)
import qualified Data.ByteString as B
import           Data.Char (ord)
import           Data.Int (Int8)
import qualified Data.Vector as V
import           Linear (V3(..))

version = 3.21 :: Float

__date__ = "2003" :: B.ByteString

baseDirName = "baseq2" :: B.ByteString

weaponReady      = 0 :: Int
weaponActivating = 1 :: Int
weaponDropping   = 2 :: Int
weaponFiring     = 3 :: Int

grenadeTimer    = 3.0 :: Float
grenadeMinSpeed = 400 :: Int
grenadeMaxSpeed = 800 :: Int

-- -----------------
-- client/q_shared.h

-- can accelerate and turn
pmNormal    = 0 :: Int
pmSpectator = 1 :: Int
-- no acceleration or turning
pmDead      = 2 :: Int
pmGib       = 3 :: Int -- different bounding box
pmFreeze    = 4 :: Int

evNone           = 0 :: Int
evItemRespawn    = 1 :: Int
evFootstep       = 2 :: Int
evFallShort      = 3 :: Int
evFall           = 4 :: Int
evFallFar        = 5 :: Int
evPlayerTeleport = 6 :: Int
evOtherTeleport  = 7 :: Int

-- angle indexes
pitch = 0 :: Int -- up / down
yaw   = 1 :: Int -- left / right
roll  = 2 :: Int -- fall over

maxStringChars  = 1024 :: Int -- max length of a string passed to Cmd_TokenizeString
maxStringTokens =   80 :: Int -- max tokens resulting from Cmd_TokenizeString
maxTokenChars   = 1024 :: Int  -- max length of an individual token

maxQPath  = 64 :: Int -- max length of a quake game pathname
maxOsPath = 128 :: Int -- max length of a filesystem pathname

-- per-level limits
maxClients     =  256 :: Int -- absolute limit
maxEdicts      = 1024 :: Int -- must change protocol to increase more
maxLightStyles =  256 :: Int
maxModels      =  256 :: Int -- these are sent over the net as bytes
maxSounds      =  256 :: Int -- so they cannot be blindly increased
maxImages      =  256 :: Int
maxItems       =  256 :: Int
maxGeneral     = maxClients * 2 -- general config strings
maxLinks       = maxEdicts + areaNodes * 2

-- game print flags
printLow    = 0 :: Int -- pickup messages
printMedium = 1 :: Int -- death messages
printHigh   = 2 :: Int -- critical messages
printChat   = 3 :: Int -- chat messages

errFatal      = 0 :: Int -- exit the entire game with a popup window
errDrop       = 1 :: Int -- print to console and disconnect from game
errDisconnect = 2 :: Int -- don't kill server

printAll       = 0 :: Int
printDeveloper = 1 :: Int -- only print when "developer 1"
printAlert     = 2 :: Int

-- key / value info strings
maxInfoKey    =  64 :: Int
maxInfoValue  =  64 :: Int
maxInfoString = 512 :: Int

-- directory searching
sffArch   = 0x01 :: Int
sffHidden = 0x02 :: Int
sffRdOnly = 0x04 :: Int
sffSubDir = 0x08 :: Int
sffSystem = 0x10 :: Int

cvarArchive    =  1 :: Int -- set to cause it to be saved to vars.rc
cvarUserInfo   =  2 :: Int -- added to userinfo when changed
cvarServerInfo =  4 :: Int -- added to serverinfo when changed
cvarNoSet      =  8 :: Int -- don't allow change from console at all, but can be set from the command line
cvarLatch      = 16 :: Int -- save changes until server restart

-- lower bits are stronger, and will eat weaker brushes completely
contentsSolid       =  1 :: Int -- an eye is never valid in a solid
contentsWindow      =  2 :: Int -- translucent, but not watery
contentsAux         =  4 :: Int
contentsLava        =  8 :: Int
contentsSlime       = 16 :: Int
contentsWater       = 32 :: Int
contentsMist        = 64 :: Int
lastVisibleContents = 64 :: Int

-- remaining contents are non-visible, and don't eat brushes
contentsAreaPortal = 0x8000 :: Int

contentsPlayerClip  = 0x10000 :: Int
contentsMonsterClip = 0x20000 :: Int

-- currents can be added to any other contents, and may be mixed
contentsCurrent0    =  0x40000 :: Int
contentsCurrent90   =  0x80000 :: Int
contentsCurrent180  = 0x100000 :: Int
contentsCurrent270  = 0x200000 :: Int
contentsCurrentUp   = 0x400000 :: Int
contentsCurrentDown = 0x800000 :: Int

contentsOrigin = 0x1000000 :: Int -- removed before bsping an entity

contentsMonster     =  0x2000000 :: Int -- should never be on a brush, only in game
contentsDeadMonster =  0x4000000 :: Int
contentsDetail      =  0x8000000 :: Int -- brushes to be added after vis leafs
contentsTranslucent = 0x10000000 :: Int -- auto set if any surface has trans
contentsLadder      = 0x20000000 :: Int

surfLight = 0x1 :: Int -- value will hold the light strength
surfSlick = 0x2 :: Int -- effects game physics

surfSky     =  0x4 :: Int -- don't draw, but add to skybox
surfWarp    =  0x8 :: Int -- turbulent water warp
surfTrans33 = 0x10 :: Int
surfTrans66 = 0x20 :: Int
surfFlowing = 0x40 :: Int -- scroll towards angle
surfNoDraw  = 0x80 :: Int -- don't bother referencing the texture

--
-- button bits
--
buttonAttack =   1 :: Int
buttonUse    =   2 :: Int
buttonAny    = 128 :: Int

maxTouch = 32 :: Int

-- entity_state_t->effects
-- Effects are things handled on the client side (lights, particles, frame animations)
-- that happen constantly on the given entity.
-- An entity that has effects will be sent to the client
-- even if it has a zero index model.
efRotate       = 0x00000001 :: Int -- rotate (bonus items) 
efGib          = 0x00000002 :: Int -- leave a trail 
efBlaster      = 0x00000008 :: Int -- redlight + trail 
efRocket       = 0x00000010 :: Int -- redlight + trail 
efGrenade      = 0x00000020 :: Int
efHyperblaster = 0x00000040 :: Int
efBFG          = 0x00000080 :: Int
efColorShell   = 0x00000100 :: Int
efPowerScreen  = 0x00000200 :: Int
efAnim01       = 0x00000400 :: Int -- automatically cycle between frames 0 and 1 at 2 hz 
efAnim23       = 0x00000800 :: Int --  automatically cycle between frames 2 and 3 at 2 hz 
efAnimAll      = 0x00001000 :: Int -- automatically cycle through all frames at 2hz 
efAnimAllFast  = 0x00002000 :: Int -- automatically cycle through all frames at 10hz 
efFlies        = 0x00004000 :: Int
efQuad         = 0x00008000 :: Int
efPent         = 0x00010000 :: Int
efTeleporter   = 0x00020000 :: Int -- particle fountain 
efFlag1        = 0x00040000 :: Int
efFlag2        = 0x00080000 :: Int
-- RAFAEL
efIonRipper        = 0x00100000 :: Int
efGreenGib         = 0x00200000 :: Int
efBlueHyperblaster = 0x00400000 :: Int
efSpinningLights   = 0x00800000 :: Int
efPlasma           = 0x01000000 :: Int
efTrap             = 0x02000000 :: Int

-- ROGUE
efTracker      = 0x04000000 :: Int
efDouble       = 0x08000000 :: Int
efSphereTrans  = 0x10000000 :: Int
efTagTrail     = 0x20000000 :: Int
efHalfDamage   = 0x40000000 :: Int
efTrackerTrail = 0x80000000 :: Int
-- ROGUE

-- entity_state_t->renderfx flags
rfMinLight    =    1 :: Int -- allways have some light (viewmodel) 
rfViewerModel =    2 :: Int -- don't draw through eyes, only mirrors 
rfWeaponModel =    4 :: Int -- only draw through eyes 
rfFullBright  =    8 :: Int -- allways draw full intensity 
rfDepthHack   =   16 :: Int -- for view weapon Z crunching 
rfTranslucent =   32 :: Int
rfFrameLerp   =   64 :: Int
rfBeam        =  128 :: Int
rfCustomSkin  =  256 :: Int -- skin is an index in image_precache 
rfGlow        =  512 :: Int -- pulse lighting for bonus items 
rfShellRed    = 1024 :: Int
rfShellGreen  = 2048 :: Int
rfShellBlue   = 4096 :: Int

-- ROGUE
rfIrVisible    = 0x00008000 :: Int -- 32768 
rfShellDouble  = 0x00010000 :: Int -- 65536 
rfShellHalfDam = 0x00020000 :: Int
rfUseDisguise  = 0x00040000 :: Int
-- ROGUE

-- player_state_t->refdef flags
rdfUnderwater   = 1 :: Int -- warp the screen as apropriate 
rdfNoWorldModel = 2 :: Int -- used for player configuration screen 

-- ROGUE
rdfIrGoggles = 4 :: Int
rdfUvGoggles = 8 :: Int
-- ROGUE

-- muzzle flashes / player effects
mzBlaster          =   0 :: Int
mzMachinegun       =   1 :: Int
mzShotgun          =   2 :: Int
mzChaingun1        =   3 :: Int
mzChaingun2        =   4 :: Int
mzChaingun3        =   5 :: Int
mzRailgun          =   6 :: Int
mzRocket           =   7 :: Int
mzGrenade          =   8 :: Int
mzLogin            =   9 :: Int
mzLogout           =  10 :: Int
mzRespawn          =  11 :: Int
mzBFG              =  12 :: Int
mzSShotgun         =  13 :: Int
mzHyperblaster     =  14 :: Int
mzItemRespawn      =  15 :: Int
-- RAFAEL
mzIonRipper        =  16 :: Int
mzBlueHyperblaster =  17 :: Int
mzPhalanx          =  18 :: Int
mzSilenced         = 128 :: Int -- bit flag ORed with one of the above numbers 

-- ROGUE
mzEtfRifle = 30 :: Int
mzUnused   = 31 :: Int
mzShotgun2 = 32 :: Int
mzHeatBeam = 33 :: Int
mzBlaster2 = 34 :: Int
mzTracker  = 35 :: Int
mzNuke1    = 36 :: Int
mzNuke2    = 37 :: Int
mzNuke4    = 38 :: Int
mzNuke8    = 39 :: Int
-- ROGUE

--
-- monster muzzle flashes
--
mz2TankBlaster1     =  1 :: Int
mz2TankBlaster2     =  2 :: Int
mz2TankBlaster3     =  3 :: Int
mz2TankMachinegun1  =  4 :: Int
mz2TankMachinegun2  =  5 :: Int
mz2TankMachinegun3  =  6 :: Int
mz2TankMachinegun4  =  7 :: Int
mz2TankMachinegun5  =  8 :: Int
mz2TankMachinegun6  =  9 :: Int
mz2TankMachinegun7  = 10 :: Int
mz2TankMachinegun8  = 11 :: Int
mz2TankMachinegun9  = 12 :: Int
mz2TankMachinegun10 = 13 :: Int
mz2TankMachinegun11 = 14 :: Int
mz2TankMachinegun12 = 15 :: Int
mz2TankMachinegun13 = 16 :: Int
mz2TankMachinegun14 = 17 :: Int
mz2TankMachinegun15 = 18 :: Int
mz2TankMachinegun16 = 19 :: Int
mz2TankMachinegun17 = 20 :: Int
mz2TankMachinegun18 = 21 :: Int
mz2TankMachinegun19 = 22 :: Int
mz2TankRocket1      = 23 :: Int
mz2TankRocket2      = 24 :: Int
mz2TankRocket3      = 25 :: Int

mz2InfantryMachinegun1  = 26 :: Int
mz2InfantryMachinegun2  = 27 :: Int
mz2InfantryMachinegun3  = 28 :: Int
mz2InfantryMachinegun4  = 29 :: Int
mz2InfantryMachinegun5  = 30 :: Int
mz2InfantryMachinegun6  = 31 :: Int
mz2InfantryMachinegun7  = 32 :: Int
mz2InfantryMachinegun8  = 33 :: Int
mz2InfantryMachinegun9  = 34 :: Int
mz2InfantryMachinegun10 = 35 :: Int
mz2InfantryMachinegun11 = 36 :: Int
mz2InfantryMachinegun12 = 37 :: Int
mz2InfantryMachinegun13 = 38 :: Int

mz2SoldierBlaster1     = 39 :: Int
mz2SoldierBlaster2     = 40 :: Int
mz2SoldierShotgun1    = 41 :: Int
mz2SoldierShotgun2    = 42 :: Int
mz2SoldierMachinegun1 = 43 :: Int
mz2SoldierMachinegun2 = 44 :: Int

mz2GunnerMachinegun1 = 45 :: Int
mz2GunnerMachinegun2 = 46 :: Int
mz2GunnerMachinegun3 = 47 :: Int
mz2GunnerMachinegun4 = 48 :: Int
mz2GunnerMachinegun5 = 49 :: Int
mz2GunnerMachinegun6 = 50 :: Int
mz2GunnerMachinegun7 = 51 :: Int
mz2GunnerMachinegun8 = 52 :: Int
mz2GunnerGrenade1    = 53 :: Int
mz2GunnerGrenade2    = 54 :: Int
mz2GunnerGrenade3    = 55 :: Int
mz2GunnerGrenade4    = 56 :: Int

mz2ChickRocket1 = 57 :: Int

mz2FlyerBlaster1 = 58 :: Int
mz2FlyerBlaster2 = 59 :: Int

mz2MedicBlaster1 = 60 :: Int

mz2GladiatorRailgun1 = 61 :: Int

mz2HoverBlaster1 = 62 :: Int

mz2ActorMachinegun1 = 63 :: Int

mz2SupertankMachinegun1 = 64 :: Int
mz2SupertankMachinegun2 = 65 :: Int
mz2SupertankMachinegun3 = 66 :: Int
mz2SupertankMachinegun4 = 67 :: Int
mz2SupertankMachinegun5 = 68 :: Int
mz2SupertankMachinegun6 = 69 :: Int
mz2SupertankRocket1     = 70 :: Int
mz2SupertankRocket2     = 71 :: Int
mz2SupertankRocket3     = 72 :: Int

mz2Boss2MachinegunL1 = 73 :: Int
mz2Boss2MachinegunL2 = 74 :: Int
mz2Boss2MachinegunL3 = 75 :: Int
mz2Boss2MachinegunL4 = 76 :: Int
mz2Boss2MachinegunL5 = 77 :: Int
mz2Boss2Rocket1      = 78 :: Int
mz2Boss2Rocket2      = 79 :: Int
mz2Boss2Rocket3      = 80 :: Int
mz2Boss2Rocket4      = 81 :: Int

mz2FloatBlaster1 = 82 :: Int

mz2SoldierBlaster3    =  83 :: Int
mz2SoldierShotgun3    =  84 :: Int
mz2SoldierMachinegun3 =  85 :: Int
mz2SoldierBlaster4    =  86 :: Int
mz2SoldierShotgun4    =  87 :: Int
mz2SoldierMachinegun4 =  88 :: Int
mz2SoldierBlaster5    =  89 :: Int
mz2SoldierShotgun5    =  90 :: Int
mz2SoldierMachinegun5 =  91 :: Int
mz2SoldierBlaster6    =  92 :: Int
mz2SoldierShotgun6    =  93 :: Int
mz2SoldierMachinegun6 =  94 :: Int
mz2SoldierBlaster7    =  95 :: Int
mz2SoldierShotgun7    =  96 :: Int
mz2SoldierMachinegun7 =  97 :: Int
mz2SoldierBlaster8    =  98 :: Int
mz2SoldierShotgun8    =  99 :: Int
mz2SoldierMachinegun8 = 100 :: Int

--- Xian shit below ---
mz2MakronBfg         = 101 :: Int
mz2MakronBlaster1    = 102 :: Int
mz2MakronBlaster2    = 103 :: Int
mz2MakronBlaster3    = 104 :: Int
mz2MakronBlaster4    = 105 :: Int
mz2MakronBlaster5    = 106 :: Int
mz2MakronBlaster6    = 107 :: Int
mz2MakronBlaster7    = 108 :: Int
mz2MakronBlaster8    = 109 :: Int
mz2MakronBlaster9    = 110 :: Int
mz2MakronBlaster10   = 111 :: Int
mz2MakronBlaster11   = 112 :: Int
mz2MakronBlaster12   = 113 :: Int
mz2MakronBlaster13   = 114 :: Int
mz2MakronBlaster14   = 115 :: Int
mz2MakronBlaster15   = 116 :: Int
mz2MakronBlaster16   = 117 :: Int
mz2MakronBlaster17   = 118 :: Int
mz2MakronRailgun1    = 119 :: Int
mz2JorgMachinegunL1  = 120 :: Int
mz2JorgMachinegunL2  = 121 :: Int
mz2JorgMachinegunL3  = 122 :: Int
mz2JorgMachinegunL4  = 123 :: Int
mz2JorgMachinegunL5  = 124 :: Int
mz2JorgMachinegunL6  = 125 :: Int
mz2JorgMachinegunR1  = 126 :: Int
mz2JorgMachinegunR2  = 127 :: Int
mz2JorgMachinegunR3  = 128 :: Int
mz2JorgMachinegunR4  = 129 :: Int
mz2JorgMachinegunR5  = 130 :: Int
mz2JorgMachinegunR6  = 131 :: Int
mz2JorgBfg1          = 132 :: Int
mz2Boss2MachinegunR1 = 133 :: Int
mz2Boss2MachinegunR2 = 134 :: Int
mz2Boss2MachinegunR3 = 135 :: Int
mz2Boss2MachinegunR4 = 136 :: Int
mz2Boss2MachinegunR5 = 137 :: Int

-- ROGUE
mz2CarrierMachinegunL1 = 138 :: Int
mz2CarrierMachinegunR1 = 139 :: Int
mz2CarrierGrenade      = 140 :: Int
mz2TurretMachinegun    = 141 :: Int
mz2TurretRocket        = 142 :: Int
mz2TurretBlaster       = 143 :: Int
mz2StalkerBlaster      = 144 :: Int
mz2DaedalusBlaster     = 145 :: Int
mz2MedicBlaster2       = 146 :: Int
mz2CarrierRailgun      = 147 :: Int
mz2WidowDisruptor      = 148 :: Int
mz2WidowBlaster        = 149 :: Int
mz2WidowRail           = 150 :: Int
mz2WidowPlasmaBeam     = 151 :: Int -- PMM - not used 
mz2CarrierMachinegunL2 = 152 :: Int
mz2CarrierMachinegunR2 = 153 :: Int
mz2WidowRailLeft       = 154 :: Int
mz2WidowRailRight      = 155 :: Int
mz2WidowBlasterSweep1  = 156 :: Int
mz2WidowBlasterSweep2  = 157 :: Int
mz2WidowBlasterSweep3  = 158 :: Int
mz2WidowBlasterSweep4  = 159 :: Int
mz2WidowBlasterSweep5  = 160 :: Int
mz2WidowBlasterSweep6  = 161 :: Int
mz2WidowBlasterSweep7  = 162 :: Int
mz2WidowBlasterSweep8  = 163 :: Int
mz2WidowBlasterSweep9  = 164 :: Int
mz2WidowBlaster100     = 165 :: Int
mz2WidowBlaster90      = 166 :: Int
mz2WidowBlaster80      = 167 :: Int
mz2WidowBlaster70      = 168 :: Int
mz2WidowBlaster60      = 169 :: Int
mz2WidowBlaster50      = 170 :: Int
mz2WidowBlaster40      = 171 :: Int
mz2WidowBlaster30      = 172 :: Int
mz2WidowBlaster20      = 173 :: Int
mz2WidowBlaster10      = 174 :: Int
mz2WidowBlaster0       = 175 :: Int
mz2WidowBlaster10L     = 176 :: Int
mz2WidowBlaster20L     = 177 :: Int
mz2WidowBlaster30L     = 178 :: Int
mz2WidowBlaster40L     = 179 :: Int
mz2WidowBlaster50L     = 180 :: Int
mz2WidowBlaster60L     = 181 :: Int
mz2WidowBlaster70L     = 182 :: Int
mz2WidowRun1           = 183 :: Int
mz2WidowRun2           = 184 :: Int
mz2WidowRun3           = 185 :: Int
mz2WidowRun4           = 186 :: Int
mz2WidowRun5           = 187 :: Int
mz2WidowRun6           = 188 :: Int
mz2WidowRun7           = 189 :: Int
mz2WidowRun8           = 190 :: Int
mz2CarrierRocket1      = 191 :: Int
mz2CarrierRocket2      = 192 :: Int
mz2CarrierRocket3      = 193 :: Int
mz2CarrierRocket4      = 194 :: Int
mz2Widow2Beamer1       = 195 :: Int
mz2Widow2Beamer2       = 196 :: Int
mz2Widow2Beamer3       = 197 :: Int
mz2Widow2Beamer4       = 198 :: Int
mz2Widow2Beamer5       = 199 :: Int
mz2Widow2BeamSweep1    = 200 :: Int
mz2Widow2BeamSweep2    = 201 :: Int
mz2Widow2BeamSweep3    = 202 :: Int
mz2Widow2BeamSweep4    = 203 :: Int
mz2Widow2BeamSweep5    = 204 :: Int
mz2Widow2BeamSweep6    = 205 :: Int
mz2Widow2BeamSweep7    = 206 :: Int
mz2Widow2BeamSweep8    = 207 :: Int
mz2Widow2BeamSweep9    = 208 :: Int
mz2Widow2BeamSweep10   = 209 :: Int
mz2Widow2BeamSweep11   = 210 :: Int

splashUnknown    = 0 :: Int
splashSparks     = 1 :: Int
splashBlueWater  = 2 :: Int
splashBrownWater = 3 :: Int
splashSlime      = 4 :: Int
splashLava       = 5 :: Int
splashBlood      = 6 :: Int

-- sound channels
-- channel 0 never willingly overrides
-- other channels (1-7) allways override a playing sound on that channel
chanAuto     =  0 :: Int
chanWeapon   =  1 :: Int
chanVoice    =  2 :: Int
chanItem     =  3 :: Int
chanBody     =  4 :: Int
-- modifier flags
chanNoPhsAdd =  8 :: Int
-- send to all clients, not just ones in PHS (ATTN 0 will also do this)
chanReliable = 16 :: Int -- send by reliable message, not datagram 

-- sound attenuation values
attnNone   = 0 :: Float -- full volume the entire level 
attnNorm   = 1 :: Float 
attnIdle   = 2 :: Float
attnStatic = 3 :: Float -- diminish very rapidly with distance 

-- player_state->stats[] indexes
statHealthIcon   =  0 :: Int
statHealth       =  1 :: Int
statAmmoIcon     =  2 :: Int
statAmmo         =  3 :: Int
statArmorIcon    =  4 :: Int
statArmor        =  5 :: Int
statSelectedIcon =  6 :: Int
statPickupIcon   =  7 :: Int
statPickupString =  8 :: Int
statTimerIcon    =  9 :: Int
statTimer        = 10 :: Int
statHelpIcon     = 11 :: Int
statSelectedItem = 12 :: Int
statLayouts      = 13 :: Int
statFrags        = 14 :: Int
statFlashes      = 15 :: Int -- cleared each frame, 1 = health, 2 = armor 
statChase        = 16 :: Int
statSpectator    = 17 :: Int

maxStats = 32 :: Int

-- dmflags->value flags
dfNoHealth       = 0x00000001 :: Int -- 1 
dfNoItems        = 0x00000002 :: Int -- 2 
dfWeaponsStay    = 0x00000004 :: Int -- 4 
dfNoFalling      = 0x00000008 :: Int -- 8 
dfInstantItems   = 0x00000010 :: Int -- 16 
dfSameLevel      = 0x00000020 :: Int -- 32 
dfSkinTeams      = 0x00000040 :: Int -- 64 
dfModelTeams     = 0x00000080 :: Int -- 128 
dfNoFriendlyFire = 0x00000100 :: Int -- 256 
dfSpawnFarthest  = 0x00000200 :: Int -- 512 
dfForceRespawn   = 0x00000400 :: Int -- 1024 
dfNoArmor        = 0x00000800 :: Int -- 2048 
dfAllowExit      = 0x00001000 :: Int -- 4096 
dfInfiniteAmmo   = 0x00002000 :: Int -- 8192 
dfQuadDrop       = 0x00004000 :: Int -- 16384 
dfFixedFov       = 0x00008000 :: Int -- 32768 

-- RAFAEL
dfQuadfireDrop = 0x00010000 :: Int -- 65536 

-- ROGUE
dfNoMines       = 0x00020000 :: Int
dfNoStackDouble = 0x00040000 :: Int
dfNoNukes       = 0x00080000 :: Int
dfNoSpheres     = 0x00100000 :: Int
-- ROGUE

--
-- config strings are a general means of communication from
-- the server to all connected clients.
-- Each config string can be at most MAX_QPATH characters.
--
csName      = 0 :: Int
csCdTrack   = 1 :: Int
csSky       = 2 :: Int
csSkyAxis   = 3 :: Int -- %f %f %f format 
csSkyRotate = 4 :: Int
csStatusBar = 5 :: Int --display program string 

csAirAccel    = 29 :: Int -- air acceleration control 
csMaxClients  = 30 :: Int
csMapChecksum = 31 :: Int -- for catching cheater maps 

csModels         = 32 :: Int
csSounds         = csModels + maxModels
csImages         = csSounds + maxSounds
csLights         = csImages + maxImages
csItems          = csLights + maxLightStyles
csPlayerSkins    = csItems + maxItems
csGeneral        = csPlayerSkins + maxClients
maxConfigStrings = csGeneral + maxGeneral

healthIgnoreMax = 1 :: Int
healthTimed     = 2 :: Int

-- gi.BoxEdicts() can return a list of either solid or trigger entities
-- FIXME: eliminate AREA_ distinction?
areaSolid    = 1 :: Int
areaTriggers = 2 :: Int

teGunshot               =  0 :: Int
teBlood                 =  1 :: Int
teBlaster               =  2 :: Int
teRailTrail             =  3 :: Int
teShotgun               =  4 :: Int
teExplosion1            =  5 :: Int
teExplosion2            =  6 :: Int
teRocketExplosion       =  7 :: Int
teGrenadeExplosion      =  8 :: Int
teSparks                =  9 :: Int
teSplash                = 10 :: Int
teBubbleTrail           = 11 :: Int
teScreenSparks          = 12 :: Int
teShieldSparks          = 13 :: Int
teBulletSparks          = 14 :: Int
teLaserSparks           = 15 :: Int
teParasiteAttack        = 16 :: Int
teRocketExplosionWater  = 17 :: Int
teGrenadeExplosionWater = 18 :: Int
teMedicCableAttack      = 19 :: Int
teBfgExplosion          = 20 :: Int
teBfgBigExplosion       = 21 :: Int
teBossTPort             = 22 :: Int -- used as '22' in a map, so DON'T RENUMBER!!! 
teBfgLaser              = 23 :: Int
teGrappleCable          = 24 :: Int
teWeldingSparks         = 25 :: Int
teGreenBlood            = 26 :: Int
teBlueHyperblaster      = 27 :: Int
tePlasmaExplosion       = 28 :: Int
teTunnelSparks          = 29 :: Int
-- ROGUE 
teBlaster2         = 30 :: Int
teRailTrail2       = 31 :: Int
teFlame            = 32 :: Int
teLightning        = 33 :: Int
teDebugTrail       = 34 :: Int
tePlainExplosion   = 35 :: Int
teFlashlight       = 36 :: Int
teForceWall        = 37 :: Int
teHeatBeam         = 38 :: Int
teMonsterHeatBeam  = 39 :: Int
teSteam            = 40 :: Int
teBubbleTrail2     = 41 :: Int
teMoreBlood        = 42 :: Int
teHeatBeamSparks   = 43 :: Int
teHeatBeamSteam    = 44 :: Int
teChainFistSmoke   = 45 :: Int
teElectricSparks   = 46 :: Int
teTrackerExplosion = 47 :: Int
teTeleportEffect   = 48 :: Int
teDBallGoal        = 49 :: Int
teWidowBeamOut     = 50 :: Int
teNukeBlast        = 51 :: Int
teWidowSplash      = 52 :: Int
teExplosion1Big    = 53 :: Int
teExplosion1Np     = 54 :: Int
teFlechette        = 55 :: Int

-- content masks
maskAll          = -1 :: Int
maskSolid        = contentsSolid .|. contentsWindow
maskPlayerSolid  = contentsSolid .|. contentsPlayerClip .|. contentsWindow .|. contentsMonster
maskDeadSolid    = contentsSolid .|. contentsPlayerClip .|. contentsWindow
maskMonsterSolid = contentsSolid .|. contentsMonsterClip .|. contentsWindow .|. contentsMonster
maskWater        = contentsWater .|. contentsLava .|. contentsSlime
maskOpaque       = contentsSolid .|. contentsSlime .|. contentsLava
maskShot         = contentsSolid .|. contentsMonster .|. contentsWindow .|. contentsDeadMonster
maskCurrent      = contentsCurrent0 .|. contentsCurrent90 .|. contentsCurrent180 .|. contentsCurrent270 .|. contentsCurrentUp .|. contentsCurrentDown

-- item spawnflags
itemTriggerSpawn  = 0x00000001 :: Int
itemNoTouch       = 0x00000002 :: Int
-- 6 bits reserved for editor flags
-- 8 bits used as power cube id bits for coop games
droppedItem       = 0x00010000 :: Int
droppedPlayerItem = 0x0002000 :: Int
itemTargetsUsed   = 0x00040000 :: Int

-- (machen nur GL)
vidrefGl    = 1 :: Int
vidrefSoft  = 2 :: Int
vidrefOther = 3 :: Int

-- --------------
-- game/g_local.h

fflSpawntemp = 1 :: Int
fflNospawn   = 2 :: Int

-- enum fieldtype_t
fInt       =  0 :: Int
fFloat     =  1 :: Int
fLstring   =  2 :: Int -- string on disk, pointer in memory, TAG_LEVEL
fGstring   =  3 :: Int -- string on disk, pointer in memory, TAG_GAME
fVector    =  4 :: Int
fAnglehack =  5 :: Int
fEdict     =  6 :: Int -- index on disk, pointer in memory
fItem      =  7 :: Int -- index on disk, pointer in memory
fClient    =  8 :: Int -- index on disk, pointer in memory
fFunction  =  9 :: Int
fMmove     = 10 :: Int
fIgnore    = 11 :: Int

defaultBulletHspread          =  300 :: Int
defaultBulletVspread          =  500 :: Int
defaultShotgunHspread         = 1000 :: Int
defaultShotgunVspread         =  500 :: Int
defaultDeathmatchShotgunCount =   12 :: Int
defaultShotgunCount           =   12 :: Int
defaultSshotgunCount          =   20 :: Int

animBasic   = 0 :: Int -- stand / run 
animWave    = 1 :: Int
animJump    = 2 :: Int
animPain    = 3 :: Int
animAttack  = 4 :: Int
animDeath   = 5 :: Int
animReverse = 6 :: Int

ammoBullets  = 0 :: Int
ammoShells   = 1 :: Int
ammoRockets  = 2 :: Int
ammoGrenades = 3 :: Int
ammoCells    = 4 :: Int
ammoSlugs    = 5 :: Int

-- view pitching times
damageTime = 0.5 :: Float
fallTime   = 0.3 :: Float

-- damage flags
damageRadius       = 0x00000001 :: Int -- damage was indirect 
damageNoArmor      = 0x00000002 :: Int -- armour does not protect from this damage 
damageEnergy       = 0x00000004 :: Int -- damage is from an energy based weapon 
damageNoKnockback  = 0x00000008 :: Int -- do not affect velocity, just view angles 
damageBullet       = 0x00000010 :: Int -- damage is from a bullet (used for ricochets) 
damageNoProtection = 0x00000020 :: Int
-- armor, shields, invulnerability, and godmode have no effect

damageNo  = 0 :: Int
damageYes = 1 :: Int -- will take damage if hit 
damageAim = 2 :: Int -- auto targeting recognizes this 

-- means of death
modUnknown       =         0 :: Int
modBlaster       =         1 :: Int
modShotgun       =         2 :: Int
modSshotgun      =         3 :: Int
modMachinegun    =         4 :: Int
modChaingun      =         5 :: Int
modGrenade       =         6 :: Int
modGSplash       =         7 :: Int
modRocket        =         8 :: Int
modRSplash       =         9 :: Int
modHyperblaster  =        10 :: Int
modRailgun       =        11 :: Int
modBFGLaser      =        12 :: Int
modBFGBlast      =        13 :: Int
modBFGEffect     =        14 :: Int
modHandgrenade   =        15 :: Int
modHgSplash      =        16 :: Int
modWater         =        17 :: Int
modSlime         =        18 :: Int
modLava          =        19 :: Int
modCrush         =        20 :: Int
modTelefrag      =        21 :: Int
modFalling       =        22 :: Int
modSuicide       =        23 :: Int
modHeldGrenade   =        24 :: Int
modExplosive     =        25 :: Int
modBarrel        =        26 :: Int
modBomb          =        27 :: Int
modExit          =        28 :: Int
modSplash        =        29 :: Int
modTargetLaser   =        30 :: Int
modTriggerHurt   =        31 :: Int
modHit           =        32 :: Int
modTargetBlaster =        33 :: Int
modFriendlyFire  = 0x8000000 :: Int

-- edict->spawnflags
-- these are set with checkboxes on each entity in the map editor
spawnFlagNotEasy       = 0x00000100 :: Int
spawnFlagNotMedium     = 0x00000200 :: Int
spawnFlagNotHard       = 0x00000400 :: Int
spawnFlagNotDeathmatch = 0x00000800 :: Int
spawnFlagNotCoop       = 0x00001000 :: Int

-- edict->flags
flFly           = 0x00000001 :: Int
flSwim          = 0x00000002 :: Int -- implied immunity to drowining 
flImmuneLaser   = 0x00000004 :: Int
flInWater       = 0x00000008 :: Int
flGodMode       = 0x00000010 :: Int
flNoTarget      = 0x00000020 :: Int
flImmuneSlime   = 0x00000040 :: Int
flImmuneLava    = 0x00000080 :: Int
flPartialGround = 0x00000100 :: Int -- not all corners are valid 
flWaterJump     = 0x00000200 :: Int --  player jumping out of water 
flTeamSlave     = 0x00000400 :: Int -- not the first on the team 
flNoKnockback   = 0x00000800 :: Int
flPowerArmor    = 0x00001000 :: Int -- power armor (if any) is active 
flRespawn       = 0x80000000 :: Int -- used for item respawning 

frameTime = 0.1 :: Float

-- memory tags to allow dynamic memory to be cleaned up
tagGame  = 765 :: Int -- clear when unloading the dll 
tagLevel = 766 :: Int -- clear when loading a new level 

meleeDistance = 80 :: Int

bodyQueueSize = 8 :: Int

-- deadflag
deadNo          = 0 :: Int
deadDying       = 1 :: Int
deadDead        = 2 :: Int
deadRespawnable = 3 :: Int

-- range
rangeMelee = 0 :: Int
rangeNear  = 1 :: Int
rangeMid   = 2 :: Int
rangeFar   = 3 :: Int

-- gib types
gibOrganic  = 0 :: Int
gibMetallic = 1 :: Int

-- monster ai flags
aiStandGround     = 0x00000001 :: Int
aiTempStandGround = 0x00000002 :: Int
aiSoundTarget     = 0x00000004 :: Int
aiLostSight       = 0x00000008 :: Int
aiPursuitLastSeen = 0x00000010 :: Int
aiPursueNext      = 0x00000020 :: Int
aiPursueTemp      = 0x00000040 :: Int
aiHoldFrame       = 0x00000080 :: Int
aiGoodGuy         = 0x00000100 :: Int
aiBrutal          = 0x00000200 :: Int
aiNoStep          = 0x00000400 :: Int
aiDucked          = 0x00000800 :: Int
aiCombatPoint     = 0x00001000 :: Int
aiMedic           = 0x00002000 :: Int
aiResurrecting    = 0x00004000 :: Int

-- monster attack state
asStraight = 1 :: Int
asSliding  = 2 :: Int
asMelee    = 3 :: Int
asMissile  = 4 :: Int

-- armor types
armorNone   = 0 :: Int
armorJacket = 1 :: Int
armorCombat = 2 :: Int
armorBody   = 3 :: Int
armorShard  = 4 :: Int

-- power armor types
powerArmorNone   = 0 :: Int
powerArmorScreen = 1 :: Int
powerArmorShield = 2 :: Int

-- handedness values
rightHanded  = 0 :: Int
leftHanded   = 1 :: Int
centerHanded = 2 :: Int

-- game.serverflags values
sflCrossTrigger1    = 0x00000001 :: Int
sflCrossTrigger2    = 0x00000002 :: Int
sflCrossTrigger3    = 0x00000004 :: Int
sflCrossTrigger4    = 0x00000008 :: Int
sflCrossTrigger5    = 0x00000010 :: Int
sflCrossTrigger6    = 0x00000020 :: Int
sflCrossTrigger7    = 0x00000040 :: Int
sflCrossTrigger8    = 0x00000080 :: Int
sflCrossTriggerMask = 0x000000ff :: Int

-- noise types for PlayerNoise
pNoiseSelf   = 0 :: Int
pNoiseWeapon = 1 :: Int
pNoiseImpact = 2 :: Int

-- gitem_t->flags
itWeapon    =  1 :: Int -- use makes active weapon 
itAmmo      =  2 :: Int
itArmor     =  4 :: Int
itStayCoop  =  8 :: Int
itKey       = 16 :: Int
itPowerup   = 32 :: Int

-- gitem_t->weapmodel for weapons indicates model index
weapBlaster         = 1 :: Int
weapShotgun         = 2 :: Int
weapSuperShotgun    = 3 :: Int
weapMachinegun      = 4 :: Int
weapChaingun        = 5 :: Int
weapGrenades        = 6 :: Int
weapGrenadeLauncher = 7 :: Int
weapRocketLauncher  = 8 :: Int
weapHyperBlaster    = 9 :: Int
weapRailgun         = 10 :: Int
weapBFG             = 11 :: Int

-- edict->movetype values
moveTypeNone   = 0 :: Int -- never moves 
moveTypeNoClip = 1 :: Int -- origin and angles change with no interaction 
moveTypePush   = 2 :: Int -- no clip to world, push on box contact 
moveTypeStop   = 3 :: Int -- no clip to world, stops on box contact 

moveTypeWalk       = 4 :: Int -- gravity 
moveTypeStep       = 5 :: Int -- gravity, special edge handling 
moveTypeFly        = 6 :: Int
moveTypeToss       = 7 :: Int -- gravity 
moveTypeFlyMissile = 8 :: Int -- extra size to monsters 
moveTypeBounce     = 9 :: Int

multicastAll  = 0 :: Int
multicastPhs  = 1 :: Int
multicastPvs  = 2 :: Int
multicastAllR = 3 :: Int
multicastPhsR = 4 :: Int
multicastPvsR = 5 :: Int

-- -------------
-- client/game.h

solidNot     = 0 :: Int -- no interaction with other objects
solidTrigger = 1 :: Int -- only touch when inside, after moving
solidBbox    = 2 :: Int -- touch on edge
solidBsp     = 3 :: Int -- bsp clip, touch on edge

gameApiVersion = 3 :: Int

-- edict->svflags
svfNoClient    = 0x00000001 :: Int -- don't send entity to clients, even if it has effects 
svfDeadMonster = 0x00000002 :: Int -- treat as CONTENTS_DEADMONSTER for collision 
svfMonster     = 0x00000004 :: Int -- treat as CONTENTS_MONSTER for collision 

maxEntClusters = 16 :: Int

svStopSpeed     = 100 :: Float
svFriction      =   6 :: Float
svWaterFriction =   1 :: Float

platLowTrigger = 1 :: Int

stateTop    = 0 :: Int
stateBottom = 1 :: Int
stateUp     = 2 :: Int
stateDown   = 3 :: Int

doorStartOpen =   1 :: Int
doorReverse   =   2 :: Int
doorCrusher   =   4 :: Int
doorNoMonster =   8 :: Int
doorToggle    =  32 :: Int
doorXaxis     =  64 :: Int
doorYaxis     = 128 :: Int

-- R E N D E R E R 
------------------
maxDLights   =   32 :: Int
maxEntities  =  128 :: Int
maxParticles = 4096 :: Int

-- gl_model.h
surfPlaneBack      =    2 :: Int
surfDrawSky        =    4 :: Int
surfDrawTurb       = 0x10 :: Int
surfDrawBackground = 0x40 :: Int
surfUnderwater     = 0x80 :: Int

powersuitScale = 4.0 :: Float

shellRedColor   = 0xF2 :: Int
shellGreenColor = 0xD0 :: Int
shellBlueColor  = 0xF3 :: Int

shellRgColor = 0xDC :: Int

shellRbColor = 0x68 :: Int -- 0x86
shellBgColor = 0x78 :: Int

-- ROGUE
shellDoubleColor  = 0xDF :: Int -- 223
shellHalfDamColor = 0x90 :: Int
shellCyanColor    = 0x72 :: Int

-- ---------
-- qcommon.h

svcBad = 0 :: Int

-- these ops are known to the game dll
-- protocol bytes that can be directly added to messages

svcMuzzleFlash  = 1 :: Int
svcMuzzleFlash2 = 2 :: Int
svcTempEntity   = 3 :: Int
svcLayout       = 4 :: Int
svcInventory    = 5 :: Int

-- the rest are private to the client and server
svcNop                 =  6 :: Int
svcDisconnect          =  7 :: Int
svcReconnect           =  8 :: Int
svcSound               =  9 :: Int -- <see code> 
svcPrint               = 10 :: Int -- [byte] id [string] null terminated string 
svcStuffText           = 11 :: Int
-- [string] stuffed into client's console buffer, should be \n terminated
svcServerData          = 12 :: Int -- [long] protocol ... 
svcConfigString        = 13 :: Int -- [short] [string] 
svcSpawnBaseline       = 14 :: Int
svcCenterPrint         = 15 :: Int -- [string] to put in center of the screen 
svcDownload            = 16 :: Int -- [short] size [size bytes] 
svcPlayerInfo          = 17 :: Int -- variable 
svcPacketEntities      = 18 :: Int -- [...] 
svcDeltaPacketEntities = 19 :: Int -- [...] 
svcFrame               = 20 :: Int

numVertexNormals =   162 :: Int
protocolVersion  =    34 :: Int
portMaster       = 27900 :: Int
portClient       = 27901 :: Int
portServer       = 27910 :: Int
portAny          =    -1 :: Int

psMType        = 1 `shiftL` 0 :: Int
psMOrigin      = 1 `shiftL` 1 :: Int
psMVelocity    = 1 `shiftL` 2 :: Int
psMTime        = 1 `shiftL` 3 :: Int
psMFlags       = 1 `shiftL` 4 :: Int
psMGravity     = 1 `shiftL` 5 :: Int
psMDeltaAngles = 1 `shiftL` 6 :: Int

updateBackup = 16 :: Int -- copies of entity_state_t to keep buffered 
-- must be power of two
updateMask = updateBackup - 1

psViewOffset  = 1 `shiftL`  7 :: Int
psViewAngles  = 1 `shiftL`  8 :: Int
psKickAngles  = 1 `shiftL`  9 :: Int
psBlend       = 1 `shiftL` 10 :: Int
psFov         = 1 `shiftL` 11 :: Int
psWeaponIndex = 1 `shiftL` 12 :: Int
psWeaponFrame = 1 `shiftL` 13 :: Int
psRdFlags     = 1 `shiftL` 14 :: Int

cmAngle1  = 1 `shiftL` 0 :: Int
cmAngle2  = 1 `shiftL` 1 :: Int
cmAngle3  = 1 `shiftL` 2 :: Int
cmForward = 1 `shiftL` 3 :: Int
cmSide    = 1 `shiftL` 4 :: Int
cmUp      = 1 `shiftL` 5 :: Int
cmButtons = 1 `shiftL` 6 :: Int
cmImpulse = 1 `shiftL` 7 :: Int

-- try to pack the common update flags into the first byte
uOrigin1   = 1 `shiftL` 0 :: Int
uOrigin2   = 1 `shiftL` 1 :: Int
uAngle2    = 1 `shiftL` 2 :: Int
uAngle3    = 1 `shiftL` 3 :: Int
uFrame8    = 1 `shiftL` 4 :: Int -- frame is a byte 
uEvent     = 1 `shiftL` 5 :: Int
uRemove    = 1 `shiftL` 6 :: Int -- REMOVE this entity, don't add it 
uMoreBits1 = 1 `shiftL` 7 :: Int -- read one additional byte 

-- second byte
uNumber16  = 1 `shiftL`  8 :: Int -- NUMBER8 is implicit if not set 
uOrigin3   = 1 `shiftL`  9 :: Int
uAngle1    = 1 `shiftL` 10 :: Int
uModel     = 1 `shiftL` 11 :: Int
uRenderFx8 = 1 `shiftL` 12 :: Int -- fullbright, etc 
uEffects8  = 1 `shiftL` 14 :: Int -- autorotate, trails, etc 
uMoreBits2 = 1 `shiftL` 15 :: Int -- read one additional byte 

-- third byte
uSkin8      = 1 `shiftL` 16 :: Int
uFrame16    = 1 `shiftL` 17 :: Int -- frame is a short 
uRenderFx16 = 1 `shiftL` 18 :: Int -- 8 + 16 = 32 
uEffects16  = 1 `shiftL` 19 :: Int -- 8 + 16 = 32 
uModel2     = 1 `shiftL` 20 :: Int -- weapons, flags, etc 
uModel3     = 1 `shiftL` 21 :: Int
uModel4     = 1 `shiftL` 22 :: Int
uMoreBits3  = 1 `shiftL` 23 :: Int -- read one additional byte 

-- fourth byte
uOldOrigin = 1 `shiftL` 24 :: Int -- FIXME: get rid of this 
uSkin16    = 1 `shiftL` 25 :: Int
uSound     = 1 `shiftL` 26 :: Int
uSolid     = 1 `shiftL` 27 :: Int

shellWhiteColor = 0xD7 :: Int

maxTriangles = 4096 :: Int
maxVerts     = 2048 :: Int
maxFrames    =  512 :: Int
maxMd2Skins  =   32 :: Int
maxSkinName  =   64 :: Int

maxLightMaps = 4 :: Int
mipLevels    = 4 :: Int

clcBad       = 0 :: Int
clcNop       = 1 :: Int
clcMove      = 2 :: Int -- [[usercmd_t]
clcUserInfo  = 3 :: Int -- [[userinfo string]
clcStringCmd = 4 :: Int -- [string] message

nsClient = 0 :: Int
nsServer = 1 :: Int

naLoopback      = 0 :: Int
naBroadcast     = 1 :: Int
naIp            = 2 :: Int
naIpx           = 3 :: Int
naBroadcastIpx  = 4 :: Int

sndVolume      = 1 `shiftL` 0 :: Int -- a byte 
sndAttenuation = 1 `shiftL` 1 :: Int -- a byte 
sndPos         = 1 `shiftL` 2 :: Int -- three coordinates 
sndEnt         = 1 `shiftL` 3 :: Int -- a short 0-2: channel, 3-12: entity 
sndOffset      = 1 `shiftL` 4 :: Int -- a byte, msec offset from frame start 

defaultSoundPacketVolume      = 1.0 :: Float
defaultSoundPacketAttenuation = 1.0 :: Float

-- --------
-- client.h
maxParseEntities      = 1024 :: Int
maxClientWeaponModels =   20 :: Int

cmdBackup = 64 :: Int -- allow a lot of command backups for very fast systems	

caUninitialized = 0 :: Int
caDisconnected  = 1 :: Int
caConnecting    = 2 :: Int
caConnected     = 3 :: Int
caActive        = 4 :: Int

maxAliasName = 32 :: Int
maxNumArgvs  = 50 :: Int

maxMsgLen = 1400 :: Int

-- ---------
-- console.h

numConTimes =     4 :: Int
conTextSize = 32768 :: Int

bspVersion = 38 :: Int

-- --------
-- qfiles.h 

-- upper design bounds
-- leaffaces, leafbrushes, planes, and verts are still bounded by
-- 16 bit short limits
maxMapModels    =    1024 :: Int
maxMapBrushes   =    8192 :: Int
maxMapEntities  =    2048 :: Int
maxMapEntString = 0x40000 :: Int
maxMapTexInfo   =    8192 :: Int

maxMapAreas       =      256 :: Int
maxMapAreaPortals =     1024 :: Int
maxMapPlanes      =    65536 :: Int
maxMapNodes       =    65536 :: Int
maxMapBrushSides  =    65536 :: Int
maxMapLeafs       =    65536 :: Int
maxMapVerts       =    65536 :: Int
maxMapFaces       =    65536 :: Int
maxMapLeafFaces   =    65536 :: Int
maxMapLeafBrushes =    65536 :: Int
maxMapPortals     =    65536 :: Int
maxMapEdges       =   128000 :: Int
maxMapSurfEdges   =   256000 :: Int
maxMapLighting    = 0x200000 :: Int
maxMapVisibility  = 0x100000 :: Int

-- key / value pair sizes
maxKey   =   32 :: Int
maxValue = 1024 :: Int

-- 0-2 are axial planes
planeX = 0 :: Int8
planeY = 1 :: Int8
planeZ = 2 :: Int8

-- 3-5 are non-axial planes snapped to the nearest
planeAnyX = 3 :: Int8
planeAnyY = 4 :: Int8
planeAnyZ = 5 :: Int8

lumpEntities    =  0 :: Int
lumpPlanes      =  1 :: Int
lumpVertexes    =  2 :: Int
lumpVisibility  =  3 :: Int
lumpNodes       =  4 :: Int
lumpTexInfo     =  5 :: Int
lumpFaces       =  6 :: Int
lumpLighting    =  7 :: Int
lumpLeafs       =  8 :: Int
lumpLeafFaces   =  9 :: Int
lumpLeafBrushes = 10 :: Int
lumpEdges       = 11 :: Int
lumpSurfEdges   = 12 :: Int
lumpModels      = 13 :: Int
lumpBrushes     = 14 :: Int
lumpBrushSides  = 15 :: Int
lumpPop         = 16 :: Int
lumpAreas       = 17 :: Int
lumpAreaPortals = 18 :: Int
headerLumps     = 19 :: Int

dtrivertxV0   = 0 :: Int
dtrivertxV1   = 1 :: Int
dtrivertxV2   = 2 :: Int
dtrivertxLni  = 3 :: Int
dtrivertxSize = 4 :: Int

aliasVersion = 8 :: Int
apiVersion   = 3 :: Int -- ref_library (refexport_t)

gameVersion  = "baseq2" :: B.ByteString

dvisPvs = 0 :: Int
dvisPhs = 1 :: Int

-- ----------------
-- client/keydest_t

keyGame    = 0 :: Int
keyConsole = 1 :: Int
keyMessage = 2 :: Int
keyMenu    = 3 :: Int

-- ---------------
-- server/server.h

csFree      = 0 :: Int -- can be reused for a new connection
csZombie    = 1 :: Int -- client has been disconnected, but don't reuse
-- connection for a couple seconds
csConnected = 2 :: Int -- has been assigned to a client_t, but not in game yet
csSpawned   = 3 :: Int

maxChallenges = 1024 :: Int

ssDead      = 0 :: Int -- no map loaded
ssLoading   = 1 :: Int -- spawning level edicts
ssGame      = 2 :: Int -- actively running
ssCinematic = 3 :: Int
ssDemo      = 4 :: Int
ssPic       = 5 :: Int

svOutputbufLength = maxMsgLen - 16

rdNone   = 0 :: Int
rdClient = 1 :: Int
rdPacket = 2 :: Int

rateMessages = 10 :: Int

latencyCounts = 16 :: Int

maxCmdLine = 256 :: Int

maxMasters = 8 :: Int

-- -----------------
-- server/sv_world.h

areaDepth =  4 :: Int
areaNodes = 32 :: Int

execNow    = 0 :: Int
execInsert = 1 :: Int
execAppend = 2 :: Int

-- --------------
-- client/qmenu.h

maxMenuItems = 64 :: Int

mtypeSlider      = 0 :: Int
mtypeList        = 1 :: Int
mtypeAction      = 2 :: Int
mtypeSpinControl = 3 :: Int
mtypeSeparator   = 4 :: Int
mtypeField       = 5 :: Int

kTab    =  9 :: Int
kEnter  = 13 :: Int
kEscape = 27 :: Int
kSpace  = 32 :: Int

-- normal keys should be passed as lowercased ascii

kBackspace  = 127 :: Int
kUpArrow    = 128 :: Int
kDownArrow  = 129 :: Int
kLeftArrow  = 130 :: Int
kRightArrow = 131 :: Int

qmfLeftJustify = 0x00000001 :: Int
qmfGrayed      = 0x00000002 :: Int
qmfNumbersOnly = 0x00000004 :: Int

rColumnOffset =  16 :: Int
lColumnOffset = -16 :: Int

maxDisplayName  =   16 :: Int
maxPlayerModels = 1024 :: Int

maxLocalServers       = 8 :: Int
numAddressBookEntries = 9 :: Int

noServerString = "<no server>" :: B.ByteString

stepSize = 18 :: Int


moveStopEpsilon = 0.1 :: Float

minStepNormal = 0.7 :: Float -- can't step up onto very steep slopes


-- used by filefinders in Sys
fileIsReadable  = 1 :: Int
fileIsWritable  = 2 :: Int
fileIsFile      = 4 :: Int
fileIsDirectory = 8 :: Int


{-
	// datentyp konstanten
	// groesse in bytes
    public final static boolean LITTLE_ENDIAN = (ByteOrder.nativeOrder() == ByteOrder.LITTLE_ENDIAN);
    -}

sizeOfShort  = 2 :: Int
sizeOfInt    = 4 :: Int
sizeOfLong   = 8 :: Int
sizeOfFloat  = 4 :: Int
sizeOfDouble = 8 :: Int

-- move from Server/SV.hs
maxClipPlanes = 5 :: Int

-- move from Client/CLTEnt
maxExplosions = 32 :: Int
maxBeams      = 32 :: Int
maxLasers     = 32 :: Int
maxSustains   = 32 :: Int

-- move from QCommon/FS
maxRead        = 0x10000 :: Int
idPakHeader    = (ord 'K' `shiftL` 24) + (ord 'C' `shiftL` 16) + (ord 'A' `shiftL` 8) + ord 'P' :: Int
maxFilesInPack = 4096 :: Int

-- move from Render/RenderAPI
glColorIndex8Ext = glColorIndex :: Int
refVersion = "GL 0.01" :: B.ByteString

{-
 - skins will be outline flood filled and mip mapped pics and sprites with
 - alpha will be outline flood filled pic won't be mip mapped
 - 
 - model skin sprite frame wall texture pic
 -}
-- enum imagetype_t
itSkin   = 0 :: Int
itSprite = 1 :: Int
itWall   = 2 :: Int
itPic    = 3 :: Int
itSky    = 4 :: Int

-- enum modtype_t
modBad    = 0 :: Int
modBrush  = 1 :: Int
modSprite = 2 :: Int
modAlias  = 3 :: Int
texNumLightmaps = 1024 :: Int
texNumScraps    = 1152 :: Int
texNumImages    = 1153 :: Int
maxGLTextures   = 1024 :: Int
maxLBMHeight    =  480 :: Int
backfaceEpsilon = 0.01 :: Float

-- GL config stuff
glRendererVoodoo     = 0x00000001 :: Int
glRendererVoodoo2    = 0x00000002 :: Int
glRendererVoodooRush = 0x00000004 :: Int
glRendererBanshee    = 0x00000008 :: Int
glRenderer3DFX       = 0x0000000F :: Int
glRendererPCX1       = 0x00000010 :: Int
glRendererPCX2       = 0x00000020 :: Int
glRendererPMX        = 0x00000040 :: Int
glRendererPowerVR    = 0x00000070 :: Int
glRendererPerMedia2  = 0x00000100 :: Int
glRendererGlintMX    = 0x00000200 :: Int
glRendererGlintTX    = 0x00000400 :: Int
glRenderer3DLabsMisc = 0x00000800 :: Int
glRenderer3DLabs     = 0x00000F00 :: Int
glRendererRealizm    = 0x00001000 :: Int
glRendererRealizm2   = 0x00002000 :: Int
glRendererIntergraph = 0x00003000 :: Int
glRenderer3DPro      = 0x00004000 :: Int
glRendererReal3D     = 0x00008000 :: Int
glRendererRiva128    = 0x00010000 :: Int
glRendererDYPIC      = 0x00020000 :: Int
glRendererV1000      = 0x00040000 :: Int
glRendererV2100      = 0x00080000 :: Int
glRendererV2200      = 0x00100000 :: Int
glRendererRendition  = 0x001C0000 :: Int
glRendererO2         = 0x00100000 :: Int
glRendererImpact     = 0x00200000 :: Int
glRendererRE         = 0x00400000 :: Int
glRendererIR         = 0x00800000 :: Int
glRendererSGI        = 0x00F00000 :: Int
glRendererMCD        = 0x01000000 :: Int
glRendererOther      = 0x80000000 :: Int

-- enum rserr_t
rsErrOk                = 0 :: Int
rsErrInvalidFullscreen = 1 :: Int
rsErrInvalidMode       = 2 :: Int
rsErrUnknown           = 3 :: Int

-- move from Render/OpenGL/QGL
-- alpha functions
glNever    = 0x0200 :: Int
glLess     = 0x0201 :: Int
glEqual    = 0x0202 :: Int
glLequal   = 0x0203 :: Int
glGreater  = 0x0204 :: Int
glNotEqual = 0x0205 :: Int
glGEqual   = 0x0206 :: Int
glAlways   = 0x0207 :: Int

-- attribute masks
glDepthBufferBit   = 0x00000100 :: Int
glStencilBufferBit = 0x00000400 :: Int
glColorBufferBit   = 0x00004000 :: Int

-- begin modes
glPoints         = 0x0000 :: Int
glLines          = 0x0001 :: Int
glLineLoop       = 0x0002 :: Int
glLineStrip      = 0x0003 :: Int
glTriangles      = 0x0004 :: Int
glTriangleStrip  = 0x0005 :: Int
glTriangleFan    = 0x0006 :: Int
glQuads          = 0x0007 :: Int
glQuadStrip      = 0x0008 :: Int
glPolygon        = 0x0009 :: Int

-- blending factors
glZero             =      0 :: Int
glOne              =      1 :: Int
glSrcColor         = 0x0300 :: Int
glOneMinusSrcColor = 0x0301 :: Int
glSrcAlpha         = 0x0302 :: Int
glOneMinusSrcAlpha = 0x0303 :: Int
glDstAlpha         = 0x0304 :: Int
glOneMinusDstAlpha = 0x0305 :: Int

-- boolean
glTrue  = 1 :: Int
glFalse = 0 :: Int

-- data types
glByte           = 0x1400 :: Int
glUnsignedByte   = 0x1401 :: Int
glShort          = 0x1402 :: Int
glUnsignedShort  = 0x1403 :: Int
glInt            = 0x1404 :: Int
glUnsignedInt    = 0x1405 :: Int
glFloat          = 0x1406 :: Int

-- draw buffer modes
glFront        = 0x0404 :: Int
glBack         = 0x0405 :: Int
glFrontAndBack = 0x0408 :: Int

-- errors
glNoError         =      0 :: Int
glPointSmooth     = 0x0B10 :: Int
glCullFace        = 0x0B44 :: Int
glDepthTest       = 0x0B71 :: Int
glModelViewMatrix = 0x0BA6 :: Int
glAlphaTest       = 0x0BC0 :: Int
glBlend           = 0x0BE2 :: Int
glScissorTest     = 0x0C11 :: Int
glPackAlignment   = 0x0D05 :: Int
glTexture2D       = 0x0DE1 :: Int

-- hints
glPerspectiveCorrectionHint = 0x0C50 :: Int
glDontCare                  = 0x1100 :: Int
glFastest                   = 0x1101 :: Int
glNicest                    = 0x1102 :: Int

-- matrix modes
glModelView  = 0x1700 :: Int
glProjection = 0x1701 :: Int

-- pixel formats
glColorIndex     = 0x1900 :: Int
glRed            = 0x1903 :: Int
glGreen          = 0x1904 :: Int
glBlue           = 0x1905 :: Int
glAlpha          = 0x1906 :: Int
glRgb            = 0x1907 :: Int
glRgba           = 0x1908 :: Int
glLuminance      = 0x1909 :: Int
glLuminanceAlpha = 0x190A :: Int

-- polygon modes
glPoint = 0x1B00 :: Int
glLine  = 0x1B01 :: Int
glFill  = 0x1B02 :: Int

-- shading models
glFlat    = 0x1D00 :: Int
glSmooth  = 0x1D01 :: Int
glReplace = 0x1E01 :: Int

-- string names
glVendor     = 0x1F00 :: Int
glRenderer   = 0x1F01 :: Int
glVersion    = 0x1F02 :: Int
glExtensions = 0x1F03 :: Int

-- TextureEnvMode
glModulate = 0x2100 :: Int

-- TextureEnvParameter
glTextureEnvMode  = 0x2200 :: Int
glTextureEnvColor = 0x2201 :: Int

-- TextureEnvTarget
glTextureEnv           = 0x2300 :: Int
glNearest              = 0x2600 :: Int
glLinear               = 0x2601 :: Int
glNearestMipmapNearest = 0x2700 :: Int
glLinearMipmapNearest  = 0x2701 :: Int
glNearestMipmapLinear  = 0x2702 :: Int
glLinearMipmapLinear   = 0x2703 :: Int

-- TextureParameterName
glTextureMagFilter = 0x2800 :: Int
glTextureMinFilter = 0x2801 :: Int
glTextureWrapS     = 0x2802 :: Int
glTextureWrapT     = 0x2803 :: Int

-- TextureWrapMode
glClamp  = 0x2900 :: Int
glRepeat = 0x2901 :: Int

-- texture
glLuminance8 = 0x8040 :: Int
glIntensity8 = 0x804B :: Int
glR3G3B2     = 0x2A10 :: Int
glRGB4       = 0x804F :: Int
glRGB5       = 0x8050 :: Int
glRGB8       = 0x8051 :: Int
glRGBA2      = 0x8055 :: Int
glRGBA4      = 0x8056 :: Int
glRGB5A1     = 0x8057 :: Int
glRGBA8      = 0x8058 :: Int

-- vertex arrays
glVertexArray       = 0x8074 :: Int
glColorArray        = 0x8076 :: Int
glTextureCoordArray = 0x8078 :: Int
glT2fV3f            = 0x2A27 :: Int

-- OpenGL 1.2, 1.3 constants
glSharedTexturePaletteExt = 0x81FB :: Int
glTexture0                = 0x84C0 :: Int
glTexture1                = 0x84C1 :: Int
glTexture0ARB             = 0x84C0 :: Int
glTexture1ARB             = 0x84C1 :: Int
glBGR                     = 0x80E0 :: Int
glBGRA                    = 0x80E1 :: Int

-- point parameters
glPointSizeMinExt           = 0x8126 :: Int
glPointSizeMaxExt           = 0x8127 :: Int
glPointFadeThresholdSizeExt = 0x8128 :: Int
glDistanceAttenuationExt    = 0x8129 :: Int

-- move from somewhere?
maxVertices :: Int
maxVertices = 64

-- move from Game.PMoveT
pmfDucked        =  1 :: Int8
pmfJumpHeld      =  2 :: Int8
pmfOnGround      =  4 :: Int8
pmfTimeWaterJump =  8 :: Int8
pmfTimeLand      = 16 :: Int8
pmfTimeTeleport  = 32 :: Int8
pmfNoPrediction  = 64 :: Int8

byteDirs :: V.Vector (V3 Float)
byteDirs =
    V.fromList [ V3 (-0.525731)   0.000000    0.850651
               , V3 (-0.442863)   0.238856    0.864188
               , V3 (-0.295242)   0.000000    0.955423
               , V3 (-0.309017)   0.500000    0.809017
               , V3 (-0.162460)   0.262866    0.951056
               , V3   0.000000    0.000000    1.000000
               , V3   0.000000    0.850651    0.525731
               , V3 (-0.147621)   0.716567    0.681718
               , V3   0.147621    0.716567    0.681718
               , V3   0.000000    0.525731    0.850651
               , V3   0.309017    0.500000    0.809017
               , V3   0.525731    0.000000    0.850651
               , V3   0.295242    0.000000    0.955423
               , V3   0.442863    0.238856    0.864188
               , V3   0.162460    0.262866    0.951056
               , V3 (-0.681718)   0.147621    0.716567
               , V3 (-0.809017)   0.309017    0.500000
               , V3 (-0.587785)   0.425325    0.688191
               , V3 (-0.850651)   0.525731    0.000000
               , V3 (-0.864188)   0.442863    0.238856
               , V3 (-0.716567)   0.681718    0.147621
               , V3 (-0.688191)   0.587785    0.425325
               , V3 (-0.500000)   0.809017    0.309017
               , V3 (-0.238856)   0.864188    0.442863
               , V3 (-0.425325)   0.688191    0.587785
               , V3 (-0.716567)   0.681718  (-0.147621)
               , V3 (-0.500000)   0.809017  (-0.309017)
               , V3 (-0.525731)   0.850651    0.000000
               , V3   0.000000    0.850651  (-0.525731)
               , V3 (-0.238856)   0.864188  (-0.442863)
               , V3   0.000000    0.955423  (-0.295242)
               , V3 (-0.262866)   0.951056  (-0.162460)
               , V3   0.000000    1.000000    0.000000
               , V3   0.000000    0.955423    0.295242
               , V3 (-0.262866)   0.951056    0.162460
               , V3   0.238856    0.864188    0.442863
               , V3   0.262866    0.951056    0.162460
               , V3   0.500000    0.809017    0.309017
               , V3   0.238856    0.864188  (-0.442863)
               , V3   0.262866    0.951056  (-0.162460)
               , V3   0.500000    0.809017  (-0.309017)
               , V3   0.850651    0.525731    0.000000
               , V3   0.716567    0.681718    0.147621
               , V3   0.716567    0.681718  (-0.147621)
               , V3   0.525731    0.850651    0.000000
               , V3   0.425325    0.688191    0.587785
               , V3   0.864188    0.442863    0.238856
               , V3   0.688191    0.587785    0.425325
               , V3   0.809017    0.309017    0.500000
               , V3   0.681718    0.147621    0.716567
               , V3   0.587785    0.425325    0.688191
               , V3   0.955423    0.295242    0.000000
               , V3   1.000000    0.000000    0.000000
               , V3   0.951056    0.162460    0.262866
               , V3   0.850651  (-0.525731)   0.000000
               , V3   0.955423  (-0.295242)   0.000000
               , V3   0.864188  (-0.442863)   0.238856
               , V3   0.951056  (-0.162460)   0.262866
               , V3   0.809017  (-0.309017)   0.500000
               , V3   0.681718  (-0.147621)   0.716567
               , V3   0.850651    0.000000    0.525731
               , V3   0.864188    0.442863  (-0.238856)
               , V3   0.809017    0.309017  (-0.500000)
               , V3   0.951056    0.162460  (-0.262866)
               , V3   0.525731    0.000000  (-0.850651)
               , V3   0.681718    0.147621  (-0.716567)
               , V3   0.681718  (-0.147621) (-0.716567)
               , V3   0.850651    0.000000  (-0.525731)
               , V3   0.809017  (-0.309017) (-0.500000)
               , V3   0.864188  (-0.442863) (-0.238856)
               , V3   0.951056  (-0.162460) (-0.262866)
               , V3   0.147621    0.716567  (-0.681718)
               , V3   0.309017    0.500000  (-0.809017)
               , V3   0.425325    0.688191  (-0.587785)
               , V3   0.442863    0.238856  (-0.864188)
               , V3   0.587785    0.425325  (-0.688191)
               , V3   0.688191    0.587785  (-0.425325)
               , V3 (-0.147621)   0.716567  (-0.681718)
               , V3 (-0.309017)   0.500000  (-0.809017)
               , V3   0.000000    0.525731  (-0.850651)
               , V3 (-0.525731)   0.000000  (-0.850651)
               , V3 (-0.442863)   0.238856  (-0.864188)
               , V3 (-0.295242)   0.000000  (-0.955423)
               , V3 (-0.162460)   0.262866  (-0.951056)
               , V3   0.000000    0.000000  (-1.000000)
               , V3   0.295242    0.000000  (-0.955423)
               , V3   0.162460    0.262866  (-0.951056)
               , V3 (-0.442863) (-0.238856) (-0.864188)
               , V3 (-0.309017) (-0.500000) (-0.809017)
               , V3 (-0.162460) (-0.262866) (-0.951056)
               , V3   0.000000  (-0.850651) (-0.525731)
               , V3 (-0.147621) (-0.716567) (-0.681718)
               , V3   0.147621  (-0.716567) (-0.681718)
               , V3   0.000000  (-0.525731) (-0.850651)
               , V3   0.309017  (-0.500000) (-0.809017)
               , V3   0.442863  (-0.238856) (-0.864188)
               , V3   0.162460  (-0.262866) (-0.951056)
               , V3   0.238856  (-0.864188) (-0.442863)
               , V3   0.500000  (-0.809017) (-0.309017)
               , V3   0.425325  (-0.688191) (-0.587785)
               , V3   0.716567  (-0.681718) (-0.147621)
               , V3   0.688191  (-0.587785) (-0.425325)
               , V3   0.587785  (-0.425325) (-0.688191)
               , V3   0.000000  (-0.955423) (-0.295242)
               , V3   0.000000  (-1.000000)   0.000000
               , V3   0.262866  (-0.951056) (-0.162460)
               , V3   0.000000  (-0.850651)   0.525731
               , V3   0.000000  (-0.955423)   0.295242
               , V3   0.238856  (-0.864188)   0.442863
               , V3   0.262866  (-0.951056)   0.162460
               , V3   0.500000  (-0.809017)   0.309017
               , V3   0.716567  (-0.681718)   0.147621
               , V3   0.525731  (-0.850651)   0.000000
               , V3 (-0.238856) (-0.864188) (-0.442863)
               , V3 (-0.500000) (-0.809017) (-0.309017)
               , V3 (-0.262866) (-0.951056) (-0.162460)
               , V3 (-0.850651) (-0.525731)   0.000000
               , V3 (-0.716567) (-0.681718) (-0.147621)
               , V3 (-0.716567) (-0.681718)   0.147621
               , V3 (-0.525731) (-0.850651)   0.000000
               , V3 (-0.500000) (-0.809017)   0.309017
               , V3 (-0.238856) (-0.864188)   0.442863
               , V3 (-0.262866) (-0.951056)   0.162460
               , V3 (-0.864188) (-0.442863)   0.238856
               , V3 (-0.809017) (-0.309017)   0.500000
               , V3 (-0.688191) (-0.587785)   0.425325
               , V3 (-0.681718) (-0.147621)   0.716567
               , V3 (-0.442863) (-0.238856)   0.864188
               , V3 (-0.587785) (-0.425325)   0.688191
               , V3 (-0.309017) (-0.500000)   0.809017
               , V3 (-0.147621) (-0.716567)   0.681718
               , V3 (-0.425325) (-0.688191)   0.587785
               , V3 (-0.162460) (-0.262866)   0.951056
               , V3   0.442863  (-0.238856)   0.864188
               , V3   0.162460  (-0.262866)   0.951056
               , V3   0.309017  (-0.500000)   0.809017
               , V3   0.147621  (-0.716567)   0.681718
               , V3   0.000000  (-0.525731)   0.850651
               , V3   0.425325  (-0.688191)   0.587785
               , V3   0.587785  (-0.425325)   0.688191
               , V3   0.688191  (-0.587785)   0.425325
               , V3 (-0.955423)   0.295242    0.000000
               , V3 (-0.951056)   0.162460    0.262866
               , V3 (-1.000000)   0.000000    0.000000
               , V3 (-0.850651)   0.000000    0.525731
               , V3 (-0.955423) (-0.295242)   0.000000
               , V3 (-0.951056) (-0.162460)   0.262866
               , V3 (-0.864188)   0.442863  (-0.238856)
               , V3 (-0.951056)   0.162460  (-0.262866)
               , V3 (-0.809017)   0.309017  (-0.500000)
               , V3 (-0.864188) (-0.442863) (-0.238856)
               , V3 (-0.951056) (-0.162460) (-0.262866)
               , V3 (-0.809017) (-0.309017) (-0.500000)
               , V3 (-0.681718)   0.147621  (-0.716567)
               , V3 (-0.681718) (-0.147621) (-0.716567)
               , V3 (-0.850651)   0.000000  (-0.525731)
               , V3 (-0.688191)   0.587785  (-0.425325)
               , V3 (-0.587785)   0.425325  (-0.688191)
               , V3 (-0.425325)   0.688191  (-0.587785)
               , V3 (-0.425325) (-0.688191) (-0.587785)
               , V3 (-0.587785) (-0.425325) (-0.688191)
               , V3 (-0.688191) (-0.587785) (-0.425325)
               ]
