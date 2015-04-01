{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MSoldier where

import Control.Lens ((^.), (.=), use, zoom, ix)
import Control.Monad (liftM, void)

import Quake
import QuakeState
import CVarVariables
import Game.EntThink
import qualified Game.GameUtil as GameUtil

spMonsterSoldierX :: EntThink
spMonsterSoldierX =
  GenericEntThink "SP_monster_soldier_x" $ \_ -> do
    io (putStrLn "MSoldier.spMonsterSoldierX") >> undefined -- TODO

spMonsterSoldier :: EntThink
spMonsterSoldier =
  GenericEntThink "SP_monster_soldier" $ \_ -> do
    io (putStrLn "MSoldier.spMonsterSoldier") >> undefined -- TODO

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

        io (putStrLn "spMonsterSoldierLight") >> undefined -- TODO
        {- ZOOM DOESN'T WORK FOR ME RIGHT NOW
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esSkinNum .= 0
          eEdictStatus.eHealth .= 20
          eEdictStatus.eGibHealth .= (-30)
          -}

    return True
