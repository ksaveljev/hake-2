module Game.Monsters.MInfantry
    ( spMonsterInfantry
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~))
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import qualified Game.GameAI           as GameAI
import qualified Game.GameUtil         as GameUtil
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

spMonsterInfantry :: Ref EdictT -> Quake ()
spMonsterInfantry selfRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnMonsterInfantry deathmatch
  where
    doSpawnMonsterInfantry deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let soundIndex = gameImport^.giSoundIndex
                modelIndex = gameImport^.giModelIndex
                linkEntity = gameImport^.giLinkEntity
            soundIndex (Just "infantry/infpain1.wav") >>= (mInfantryGlobals.miSoundPain1 .=)
            soundIndex (Just "infantry/infpain2.wav") >>= (mInfantryGlobals.miSoundPain2 .=)
            soundIndex (Just "infantry/infdeth1.wav") >>= (mInfantryGlobals.miSoundDie1 .=)
            soundIndex (Just "infantry/infdeth2.wav") >>= (mInfantryGlobals.miSoundDie2 .=)
            soundIndex (Just "infantry/infatck1.wav") >>= (mInfantryGlobals.miSoundGunShot .=)
            soundIndex (Just "infantry/infatck3.wav") >>= (mInfantryGlobals.miSoundWeaponCock .=)
            soundIndex (Just "infantry/infatck2.wav") >>= (mInfantryGlobals.miSoundPunchSwing .=)
            soundIndex (Just "infantry/melee2.wav") >>= (mInfantryGlobals.miSoundPunchHit .=)
            soundIndex (Just "infantry/infsght1.wav") >>= (mInfantryGlobals.miSoundSight .=)
            soundIndex (Just "infantry/infsrch1.wav") >>= (mInfantryGlobals.miSoundSearch .=)
            soundIndex (Just "infantry/infidle1.wav") >>= (mInfantryGlobals.miSoundIdle .=)
            modelIdx <- modelIndex (Just "models/monsters/infantry/tris.md2")
            modifyRef selfRef (\v -> v & eMoveType                 .~ Constants.moveTypeStep
                                       & eSolid                    .~ Constants.solidBbox
                                       & eEntityState.esModelIndex .~ modelIdx
                                       & eMins                     .~ V3 (-16) (-16) (-24)
                                       & eMaxs                     .~ V3 16 16 32
                                       & eHealth                   .~ 100
                                       & eGibHealth                .~ -40
                                       & eMass                     .~ 200
                                       & ePain                     .~ Just infantryPain
                                       & eDie                      .~ Just infantryDie
                                       & eMonsterInfo.miStand      .~ Just infantryStand
                                       & eMonsterInfo.miWalk       .~ Just infantryWalk
                                       & eMonsterInfo.miRun        .~ Just infantryRun
                                       & eMonsterInfo.miDodge      .~ Just infantryDodge
                                       & eMonsterInfo.miAttack     .~ Just infantryAttack
                                       & eMonsterInfo.miMelee      .~ Nothing
                                       & eMonsterInfo.miSight      .~ Just infantrySight
                                       & eMonsterInfo.miIdle       .~ Just infantryFidget)
            linkEntity selfRef
            modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just infantryMoveStand
                                       & eMonsterInfo.miScale       .~ modelScale)
            void (entThink GameAI.walkMonsterStart selfRef)
