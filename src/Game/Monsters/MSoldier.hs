{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MSoldier where

import Control.Lens ((^.), (.=), use, ix, zoom, preuse)
import Control.Monad (liftM, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import Game.EntThink
import qualified Game.GameUtil as GameUtil
import qualified QCommon.Com as Com

spMonsterSoldierX :: EntThink
spMonsterSoldierX =
  GenericEntThink "SP_monster_soldier_x" $ \_ -> do
    io (putStrLn "MSoldier.spMonsterSoldierX") >> undefined -- TODO

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

spMonsterSoldierSS :: EntThink
spMonsterSoldierSS =
  GenericEntThink "SP_monster_soldier_ss" $ \_ -> do
    io (putStrLn "MSoldier.spMonsterSoldierSS") >> undefined -- TODO

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
