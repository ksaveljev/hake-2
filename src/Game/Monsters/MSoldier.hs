{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MSoldier where

import Control.Lens ((^.), (.=), use, ix, zoom, preuse)
import Control.Monad (liftM, void)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil
import qualified QCommon.Com as Com

modelScale :: Float
modelScale = 1.20000

soldierPain :: EntPain
soldierPain =
  GenericEntPain "soldier_pain" $ \_ _ _ _ -> do
    io (putStrLn "MSoldier.soldierPain") >> undefined -- TODO

soldierDie :: EntDie
soldierDie =
  GenericEntDie "soldier_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MSoldier.soldierDie") >> undefined -- TODO

soldierStand :: EntThink
soldierStand =
  GenericEntThink "soldier_stand" $ \_ -> do
    io (putStrLn "MSoldier.soldierStand") >> undefined -- TODO

soldierWalk :: EntThink
soldierWalk =
  GenericEntThink "soldier_walk" $ \_ -> do
    io (putStrLn "MSoldier.soldierWalk") >> undefined -- TODO

soldierRun :: EntThink
soldierRun =
  GenericEntThink "soldier_run" $ \_ -> do
    io (putStrLn "MSoldier.soldierRun") >> undefined -- TODO

soldierDodge :: EntDodge
soldierDodge =
  GenericEntDodge "soldier_dodge" $ \_ _ _ -> do
    io (putStrLn "MSoldier.soldierDodge") >> undefined -- TODO

soldierAttack :: EntThink
soldierAttack =
  GenericEntThink "soldier_attack" $ \_ -> do
    io (putStrLn "MSoldier.soldierAttack") >> undefined -- TODO

soldierSight :: EntInteract
soldierSight =
  GenericEntInteract "soldier_sight" $ \_ _ -> do
    io (putStrLn "MSoldier.soldierSight") >> undefined -- TODO

spMonsterSoldierX :: EntThink
spMonsterSoldierX =
  GenericEntThink "SP_monster_soldier_x" $ \er@(EdictReference edictIdx) -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    tris <- modelIndex "models/monsters/soldier/tris.md2"

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEntityState.esModelIndex .= tris
      eMonsterInfo.miScale .= modelScale
      eEdictMinMax.eMins .= V3 (-16) (-16) (-24)
      eEdictMinMax.eMaxs .= V3 16 16 32
      eMoveType .= Constants.moveTypeStep
      eSolid .= Constants.solidBbox

    soundIndex "soldier/solidle1.wav" >>= (mSoldierGlobals.msSoundIdle .=)
    soundIndex "soldier/solsght1.wav" >>= (mSoldierGlobals.msSoundSight1 .=)
    soundIndex "soldier/solsrch1.wav" >>= (mSoldierGlobals.msSoundSight2 .=)
    soundIndex "infantry/infatck3.wav" >>= (mSoldierGlobals.msSoundCock .=)

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEdictPhysics.eMass .= 100

      eEdictAction.eaPain .= Just soldierPain
      eEdictAction.eaDie  .= Just soldierDie

      eMonsterInfo.miStand  .= Just soldierStand
      eMonsterInfo.miWalk   .= Just soldierWalk
      eMonsterInfo.miRun    .= Just soldierRun
      eMonsterInfo.miDodge  .= Just soldierDodge
      eMonsterInfo.miAttack .= Just soldierAttack
      eMonsterInfo.miMelee  .= Nothing
      eMonsterInfo.miSight  .= Just soldierSight

    linkEntity er

    void $ think soldierStand er
    void $ think GameAI.walkMonsterStart er

    return True

{-
- QUAKED monster_soldier (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterSoldier :: EntThink
spMonsterSoldier =
  GenericEntThink "SP_monster_soldier" $ \er@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    Com.dprintf $ "Spawning a soldier at " `B.append` BC.pack (show (edict^.eEntityState.esOrigin)) -- IMPROVE

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        void $ think spMonsterSoldierX er

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

        soundIndex "soldier/solpain1.wav" >>= (mSoldierGlobals.msSoundPain .=)
        soundIndex "soldier/soldeth1.wav" >>= (mSoldierGlobals.msSoundDeath .=)
        void $ soundIndex "soldier/solatck1.wav"

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esSkinNum .= 2
          eEdictStatus.eHealth .= 30
          eEdictStatus.eGibHealth .= (-30)

    return True

{-
- QUAKED monster_soldier_ss (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterSoldierSS :: EntThink
spMonsterSoldierSS =
  GenericEntThink "SP_monster_soldier_ss" $ \er@(EdictReference edictIdx) -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        void $ think spMonsterSoldierX er

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

        soundIndex "soldier/solpain3.wav" >>= (mSoldierGlobals.msSoundPainSS .=)
        soundIndex "soldier/soldeth3.wav" >>= (mSoldierGlobals.msSoundDeathSS .=)
        void $ soundIndex "soldier/solatck3.wav"

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esSkinNum .= 4
          eEdictStatus.eHealth .= 40
          eEdictStatus.eGibHealth .= (-30)

    return True

{-
- QUAKED monster_soldier_light (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterSoldierLight :: EntThink
spMonsterSoldierLight =
  GenericEntThink "SP_monster_soldier_light" $ \er@(EdictReference edictIdx) -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        void $ think spMonsterSoldierX er

        gameImport <- use $ gameBaseGlobals.gbGameImport
        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex

        soundIndex "soldier/solpain2.wav" >>= (mSoldierGlobals.msSoundPainLight .=)
        soundIndex "soldier/soldeth2.wav" >>= (mSoldierGlobals.msSoundDeathLight .=)
        void $ modelIndex "models/objects/laser/tris.md2"
        void $ soundIndex "misc/lasfly.wav"
        void $ soundIndex "soldier/solatck2.wav"

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esSkinNum .= 0
          eEdictStatus.eHealth .= 20
          eEdictStatus.eGibHealth .= (-30)

    return True
