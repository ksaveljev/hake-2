{-# LANGUAGE OverloadedStrings #-}
module Client.CLTEnt where

import Control.Lens (zoom, (.=), use, (^.))
import Control.Monad (void)
import qualified Data.Vector as V

import Quake
import QuakeState
import qualified Constants

clearTEnts :: Quake ()
clearTEnts = do
    zoom clTEntGlobals $ do
      clteExplosions  .= V.replicate Constants.maxExplosions newExplosionT
      clteBeams       .= V.replicate Constants.maxBeams newBeamT
      cltePlayerBeams .= V.replicate Constants.maxBeams newBeamT
      clteLasers      .= V.replicate Constants.maxLasers newLaserT
      clteSustains    .= V.replicate Constants.maxSustains newCLSustainT

registerTEntModels :: Quake ()
registerTEntModels = do
    Just renderer <- use $ globals.re
    let registerModel = renderer^.rRefExport.reRegisterModel
        registerPic = renderer^.rRefExport.reRegisterPic

    modExplode         <- registerModel "models/objects/explode/tris.md2"
    modSmoke           <- registerModel "models/objects/smoke/tris.md2"
    modFlash           <- registerModel "models/objects/flash/tris.md2"
    modParasiteSegment <- registerModel "models/monsters/parasite/segment/tris.md2"
    modGrappleCable    <- registerModel "models/ctf/segment/tris.md2"
    modParasiteTip     <- registerModel "models/monsters/parasite/tip/tris.md2"
    modExplo4          <- registerModel "models/objects/r_explode/tris.md2"
    modBfgExplo        <- registerModel "sprites/s_bfg2.sp2"
    modPowerScreen     <- registerModel "models/items/armor/effect/tris.md2"

    void $ registerModel "models/objects/laser/tris.md2"
    void $ registerModel "models/objects/grenade2/tris.md2"
    void $ registerModel "models/weapons/v_machn/tris.md2"
    void $ registerModel "models/weapons/v_handgr/tris.md2"
    void $ registerModel "models/weapons/v_shotg2/tris.md2"
    void $ registerModel "models/objects/gibs/bone/tris.md2"
    void $ registerModel "models/objects/gibs/sm_meat/tris.md2"
    void $ registerModel "models/objects/gibs/bone2/tris.md2"
    -- void $ registerModel "models/objects/blaser/tris.md2"

    void $ registerPic "w_machinegun"
    void $ registerPic "a_bullets"
    void $ registerPic "i_health"
    void $ registerPic "a_grenades"

    modExplo4Big       <- registerModel "models/objects/r_explode2/tris.md2"
    modLightning       <- registerModel "models/proj/lightning/tris.md2"
    modHeatBeam        <- registerModel "models/proj/beam/tris.md2"
    modMonsterHeatBeam <- registerModel "models/proj/widowbeam/tris.md2"

    zoom clTEntGlobals $ do
      clteModExplode         .= modExplode
      clteModSmoke           .= modSmoke
      clteModFlash           .= modFlash
      clteModParasiteSegment .= modParasiteSegment
      clteModGrappleCable    .= modGrappleCable
      clteModParasiteTip     .= modParasiteTip
      clteModExplo4          .= modExplo4
      clteModBfgExplo        .= modBfgExplo
      clteModPowerScreen     .= modPowerScreen
      clteModLightning       .= modLightning
      clteModHeatBeam        .= modHeatBeam
      clteModMonsterHeatBeam .= modMonsterHeatBeam
      clteModExplo4Big       .= modExplo4Big
