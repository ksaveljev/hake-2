module Game.Monsters.MSoldier
  ( spMonsterSoldier
  , spMonsterSoldierLight
  , spMonsterSoldierSS
  ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~))
import           Control.Monad         (void)
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameUtil         as GameUtil
import           Game.MonsterInfoT
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

import {-# SOURCE #-} Game.GameImportT

modelScale :: Float
modelScale = 1.20000

spMonsterSoldier :: EntThink
spMonsterSoldier = error "MSoldier.spMonsterSoldier" -- TODO

spMonsterSoldierLight :: EntThink
spMonsterSoldierLight = EntThink "SP_monster_soldier_light" $ \selfRef -> do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    monsterSoldierLight selfRef deathmatch
    return True
  where
    monsterSoldierLight selfRef deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            void (entThink spMonsterSoldierX selfRef)
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let soundIndex = gameImport^.giSoundIndex
                modelIndex = gameImport^.giModelIndex
            soundIndex (Just "soldier/solpain2.wav") >>= (mSoldierGlobals.msSoundPainLight .=)
            soundIndex (Just "soldier/soldeth2.wav") >>= (mSoldierGlobals.msSoundDeathLight .=)
            void (modelIndex (Just "models/objects/laser/tris.md2"))
            void (soundIndex (Just "misc/lasfly.wav"))
            void (soundIndex (Just "soldier/solatck2.wav"))
            modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 0
                                       & eHealth .~ 20
                                       & eGibHealth .~ (-30))

spMonsterSoldierSS :: EntThink
spMonsterSoldierSS = error "MSoldier.spMonsterSoldierSS" -- TODO

spMonsterSoldierX :: EntThink
spMonsterSoldierX = EntThink "SP_monster_soldier_x" $ \selfRef -> do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity
    soundIndex (Just "soldier/solidle1.wav") >>= (mSoldierGlobals.msSoundIdle .=)
    soundIndex (Just "soldier/solsght1.wav") >>= (mSoldierGlobals.msSoundSight1 .=)
    soundIndex (Just "soldier/solsrch1.wav") >>= (mSoldierGlobals.msSoundSight2 .=)
    soundIndex (Just "infantry/infatck3.wav") >>= (mSoldierGlobals.msSoundCock .=)
    modelIdx <- modelIndex (Just "models/monsters/soldier/tris.md2")
    modifyRef selfRef (\v -> v & eEntityState.esModelIndex .~ modelIdx
                               & eMonsterInfo.miScale .~ modelScale
                               & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 32
                               & eMoveType .~ Constants.moveTypeStep
                               & eSolid .~ Constants.solidBbox
                               & eMass .~ 100
                               & ePain .~ Just soldierPain
                               & eDie  .~ Just soldierDie
                               & eMonsterInfo.miStand  .~ Just soldierStand
                               & eMonsterInfo.miWalk   .~ Just soldierWalk
                               & eMonsterInfo.miRun    .~ Just soldierRun
                               & eMonsterInfo.miDodge  .~ Just soldierDodge
                               & eMonsterInfo.miAttack .~ Just soldierAttack
                               & eMonsterInfo.miMelee  .~ Nothing
                               & eMonsterInfo.miSight  .~ Just soldierSight)
    linkEntity selfRef
    void (entThink soldierStand selfRef)
    void (entThink GameAI.walkMonsterStart selfRef)
    return True

soldierPain :: EntPain
soldierPain = error "MSoldier.soldierPain" -- TODO

soldierDie :: EntDie
soldierDie = error "MSoldier.soldierDie" -- TODO

soldierStand :: EntThink
soldierStand = error "MSoldier.soldierStand" -- TODO

soldierWalk :: EntThink
soldierWalk = error "MSoldier.soldierWalk" -- TODO

soldierRun :: EntThink
soldierRun = error "MSoldier.soldierRun" -- TODO

soldierDodge :: EntDodge
soldierDodge = error "MSoldier.soldierDodge" -- TODO

soldierAttack :: EntThink
soldierAttack = error "MSoldier.soldierAttack" -- TODO

soldierSight :: EntInteract
soldierSight = error "MSoldier.soldierSight" -- TODO
