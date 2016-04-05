module Client.CLTEnt
  ( clearTEnts
  , parseTEnt
  , registerTEntModels
  , registerTEntSounds
  ) where

import           Client.BeamT
import           Client.CLSustainT
import           Client.ExplosionT
import           Client.LaserT
import           Client.RefExportT
import qualified Constants
import qualified QCommon.Com as Com
import           QuakeState
import           Render.Renderer
import qualified Sound.S as S
import           Types

import           Control.Lens (use, (^.), (%=), (&), (.~))
import           Control.Monad (void)
import qualified Data.Vector as V

clearTEnts :: Quake ()
clearTEnts =
  clTEntGlobals %= (\v -> v & clteExplosions .~ V.replicate Constants.maxExplosions newExplosionT
                            & clteBeams .~ V.replicate Constants.maxBeams newBeamT
                            & cltePlayerBeams .~ V.replicate Constants.maxBeams newBeamT
                            & clteLasers .~ V.replicate Constants.maxLasers newLaserT
                            & clteSustains .~ V.replicate Constants.maxSustains newCLSustainT)

parseTEnt :: Quake ()
parseTEnt = error "CLTEnt.parseTEnt" -- TODO

registerTEntSounds :: Quake ()
registerTEntSounds = do
    sfxRic1      <- S.registerSound "world/ric1.wav"
    sfxRic2      <- S.registerSound "world/ric2.wav"
    sfxRic3      <- S.registerSound "world/ric3.wav"
    sfxLashIt    <- S.registerSound "weapons/lashit.wav"
    sfxSpark5    <- S.registerSound "world/spark5.wav"
    sfxSpark6    <- S.registerSound "world/spark6.wav"
    sfxSpark7    <- S.registerSound "world/spark7.wav"
    sfxRailg     <- S.registerSound "weapons/railgf1a.wav"
    sfxRockExp   <- S.registerSound "weapons/rocklx1a.wav"
    sfxGrenExp   <- S.registerSound "weapons/grenlx1a.wav"
    sfxWatrExp   <- S.registerSound "weapons/xpld_wat.wav"
    sfxFootStep1 <- S.registerSound "player/step1.wav"
    sfxFootStep2 <- S.registerSound "player/step2.wav"
    sfxFootStep3 <- S.registerSound "player/step3.wav"
    sfxFootStep4 <- S.registerSound "player/step4.wav"
    sfxLightning <- S.registerSound "weapons/tesla.wav"
    sfxDisrExp   <- S.registerSound "weapons/disrupthit.wav"
    void (S.registerSound "player/land1.wav")
    void (S.registerSound "player/fall2.wav")
    void (S.registerSound "player/fall1.wav")
    clTEntGlobals %= (\v -> v & clteSfxRic1 .~ sfxRic1
                              & clteSfxRic2 .~ sfxRic2
                              & clteSfxRic3 .~ sfxRic3
                              & clteSfxLashIt .~ sfxLashIt
                              & clteSfxSpark5 .~ sfxSpark5
                              & clteSfxSpark6 .~ sfxSpark6
                              & clteSfxSpark7 .~ sfxSpark7
                              & clteSfxRailg .~ sfxRailg
                              & clteSfxRockExp .~ sfxRockExp
                              & clteSfxGrenExp .~ sfxGrenExp
                              & clteSfxWatrExp .~ sfxWatrExp
                              & clteSfxFootsteps .~ V.fromList [sfxFootStep1, sfxFootStep2, sfxFootStep3, sfxFootStep4]
                              & clteSfxLightning .~ sfxLightning
                              & clteSfxDisrExp .~ sfxDisrExp)

registerTEntModels :: Quake ()
registerTEntModels =
  do renderer <- use (globals.gRenderer)
     maybe rendererError proceedRegisterTEntModels renderer
  where rendererError = Com.fatalError "CLTEnt.registerTEntModels renderer is Nothing"

proceedRegisterTEntModels :: Renderer -> Quake ()
proceedRegisterTEntModels renderer =
  do modExplode         <- registerModel "models/objects/explode/tris.md2"
     modSmoke           <- registerModel "models/objects/smoke/tris.md2"
     modFlash           <- registerModel "models/objects/flash/tris.md2"
     modParasiteSegment <- registerModel "models/monsters/parasite/segment/tris.md2"
     modGrappleCable    <- registerModel "models/ctf/segment/tris.md2"
     modParasiteTip     <- registerModel "models/monsters/parasite/tip/tris.md2"
     modExplo4          <- registerModel "models/objects/r_explode/tris.md2"
     modBfgExplo        <- registerModel "sprites/s_bfg2.sp2"
     modPowerScreen     <- registerModel "models/items/armor/effect/tris.md2"
     void (registerModel "models/objects/laser/tris.md2")
     void (registerModel "models/objects/grenade2/tris.md2")
     void (registerModel "models/weapons/v_machn/tris.md2")
     void (registerModel "models/weapons/v_handgr/tris.md2")
     void (registerModel "models/weapons/v_shotg2/tris.md2")
     void (registerModel "models/objects/gibs/bone/tris.md2")
     void (registerModel "models/objects/gibs/sm_meat/tris.md2")
     void (registerModel "models/objects/gibs/bone2/tris.md2")
     -- void (registerModel "models/objects/blaser/tris.md2")
     void (registerPic "w_machinegun")
     void (registerPic "a_bullets")
     void (registerPic "i_health")
     void (registerPic "a_grenades")
     modExplo4Big       <- registerModel "models/objects/r_explode2/tris.md2"
     modLightning       <- registerModel "models/proj/lightning/tris.md2"
     modHeatBeam        <- registerModel "models/proj/beam/tris.md2"
     modMonsterHeatBeam <- registerModel "models/proj/widowbeam/tris.md2"
     clTEntGlobals %= (\v -> v & clteModExplode .~ modExplode
                               & clteModSmoke .~ modSmoke
                               & clteModFlash .~ modFlash
                               & clteModParasiteSegment .~ modParasiteSegment
                               & clteModGrappleCable .~ modGrappleCable
                               & clteModParasiteTip .~ modParasiteTip
                               & clteModExplo4 .~ modExplo4
                               & clteModBfgExplo .~ modBfgExplo
                               & clteModPowerScreen .~ modPowerScreen
                               & clteModLightning .~ modLightning
                               & clteModHeatBeam .~ modHeatBeam
                               & clteModMonsterHeatBeam .~ modMonsterHeatBeam
                               & clteModExplo4Big .~ modExplo4Big)
  where registerModel = renderer^.rRefExport.reRegisterModel
        registerPic = renderer^.rRefExport.reRegisterPic