{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MSoldier where

import Control.Lens ((^.), (.=), (%=), use, ix, zoom, preuse)
import Control.Monad (liftM, void, when)
import Data.Bits ((.|.))
import Data.Maybe (isNothing)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import Game.MMoveT
import Game.MFrameT
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil
import qualified QCommon.Com as Com
import qualified Util.Lib as Lib

modelScale :: Float
modelScale = 1.20000

frameAttak101 :: Int
frameAttak101 = 0

frameAttak112 :: Int
frameAttak112 = 11

frameAttak201 :: Int
frameAttak201 = 12

frameAttak218 :: Int
frameAttak218 = 29

frameAttak301 :: Int
frameAttak301 = 30

frameAttak309 :: Int
frameAttak309 = 38

frameAttak401 :: Int
frameAttak401 = 39

frameAttak406 :: Int
frameAttak406 = 44

frameDuck01 :: Int
frameDuck01 = 45

frameDuck05 :: Int
frameDuck05 = 49

frameRuns01 :: Int
frameRuns01 = 109

frameRuns14 :: Int
frameRuns14 = 122

frameStand322 :: Int
frameStand322 = 197

soldierDead :: EntThink
soldierDead =
  GenericEntThink "soldier_dead" $ \self@(EdictReference selfIdx) -> do
    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEdictMinMax.eMins .= V3 (-16) (-16) (-24)
      eEdictMinMax.eMaxs .= V3 16 16 (-8)
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfDeadMonster)
      eEdictAction.eaNextThink .= 0

    linkEntity self

    return True

soldierDie :: EntDie
soldierDie =
  GenericEntDie "soldier_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MSoldier.soldierDie") >> undefined -- TODO

soldierPain :: EntPain
soldierPain =
  GenericEntPain "soldier_pain" $ \_ _ _ _ -> do
    io (putStrLn "MSoldier.soldierPain") >> undefined -- TODO

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
  GenericEntDodge "soldier_dodge" $ \(EdictReference selfIdx) attacker eta -> do
    r <- Lib.randomF

    when (r > 0.25) $ do
      Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoEnemy

      when (isNothing enemy) $
        gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoEnemy .= Just attacker

      skillValue <- liftM (^.cvValue) skillCVar

      nextMove <- if skillValue == 0
                    then return soldierMoveDuck
                    else do
                      time <- use $ gameBaseGlobals.gbLevel.llTime
                      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miPauseTime .= time + eta + 0.3
                      r' <- Lib.randomF

                      return $ if | skillValue == 1 ->
                                      if r' > 0.33
                                        then soldierMoveDuck
                                        else soldierMoveAttack3
                                  | skillValue >= 2 ->
                                      if r' > 0.66
                                        then soldierMoveDuck
                                        else soldierMoveAttack3
                                  | otherwise -> soldierMoveAttack3

      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just nextMove

soldierCock :: EntThink
soldierCock = 
  GenericEntThink "soldier_cock" $ \self@(EdictReference selfIdx) -> do
    Just frame <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esFrame
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundCock <- use $ mSoldierGlobals.msSoundCock

    if frame == frameStand322
      then sound self Constants.chanWeapon soundCock 1 (fromIntegral Constants.attnIdle) 0
      else sound self Constants.chanWeapon soundCock 1 (fromIntegral Constants.attnNorm) 0

    return True

soldierFire1 :: EntThink
soldierFire1 =
  GenericEntThink "soldier_fire1" $ \self -> do
    soldierFire self 0
    return True

soldierFire2 :: EntThink
soldierFire2 =
  GenericEntThink "soldier_fire2" $ \self -> do
    soldierFire self 1
    return True

soldierFire3 :: EntThink
soldierFire3 =
  GenericEntThink "soldier_fire3" $ \self -> do
    void $ think soldierDuckDown self
    soldierFire self 2
    return True

soldierFire4 :: EntThink
soldierFire4 =
  GenericEntThink "soldier_fire4" $ \self -> do
    soldierFire self 3
    return True

soldierFire6 :: EntThink
soldierFire6 =
  GenericEntThink "soldier_fire6" $ \self -> do
    soldierFire self 5
    return True

soldierFire7 :: EntThink
soldierFire7 =
  GenericEntThink "soldier_fire7" $ \self -> do
    soldierFire self 6
    return True

soldierFire8 :: EntThink
soldierFire8 =
  GenericEntThink "soldier_fire8" $ \self -> do
    soldierFire self 7
    return True

soldierFire :: EdictReference -> Int -> Quake ()
soldierFire _ _ = io (putStrLn "MSoldier.soldierFire") >> undefined -- TODO

soldierAttack1Refire1 :: EntThink
soldierAttack1Refire1 =
  GenericEntThink "soldier_attack1_refire1" $ \_ -> do
    io (putStrLn "MSoldier.soldierAttack1Refire1") >> undefined -- TODO

soldierAttack1Refire2 :: EntThink
soldierAttack1Refire2 =
  GenericEntThink "soldier_attack1_refire2" $ \_ -> do
    io (putStrLn "MSoldier.soldierAttack1Refire2") >> undefined -- TODO

soldierAttack2Refire1 :: EntThink
soldierAttack2Refire1 =
  GenericEntThink "soldier_attack2_refire1" $ \_ -> do
    io (putStrLn "MSoldier.soldierAttack2Refire1") >> undefined -- TODO

soldierAttack2Refire2 :: EntThink
soldierAttack2Refire2 =
  GenericEntThink "soldier_attack2_refire2" $ \_ -> do
    io (putStrLn "MSoldier.soldierAttack2Refire2") >> undefined -- TODO

soldierAttack3Refire :: EntThink
soldierAttack3Refire =
  GenericEntThink "soldier_attack3_refire" $ \_ -> do
    io (putStrLn "MSoldier.soldierAttack3Refire") >> undefined -- TODO

soldierAttack6Refire :: EntThink
soldierAttack6Refire =
  GenericEntThink "soldier_attack6_refire" $ \_ -> do
    io (putStrLn "MSoldier.soldierAttack6Refire") >> undefined -- TODO

soldierDuckUp :: EntThink
soldierDuckUp =
  GenericEntThink "soldier_duck_up" $ \_ -> do
    io (putStrLn "MSoldier.soldierDuckUp") >> undefined -- TODO

soldierDuckDown :: EntThink
soldierDuckDown =
  GenericEntThink "soldier_duck_down" $ \_ -> do
    io (putStrLn "MSoldier.soldierDuckDown") >> undefined -- TODO

soldierDuckHold :: EntThink
soldierDuckHold =
  GenericEntThink "soldier_duck_hold" $ \_ -> do
    io (putStrLn "MSoldier.soldierDuckHold") >> undefined -- TODO

soldierFramesAttack1 :: V.Vector MFrameT
soldierFramesAttack1 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire1)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack1Refire1)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierCock)
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack1Refire2)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack1 :: MMoveT
soldierMoveAttack1 = MMoveT frameAttak101 frameAttak112 soldierFramesAttack1 (Just soldierRun)

soldierFramesAttack2 :: V.Vector MFrameT
soldierFramesAttack2 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire2)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack2Refire1)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierCock)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack2Refire2)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack2 :: MMoveT
soldierMoveAttack2 = MMoveT frameAttak201 frameAttak218 soldierFramesAttack2 (Just soldierRun)

soldierFramesAttack3 :: V.Vector MFrameT
soldierFramesAttack3 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire3)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierAttack3Refire)
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierDuckUp)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack3 :: MMoveT
soldierMoveAttack3 = MMoveT frameAttak301 frameAttak309 soldierFramesAttack3 (Just soldierRun)

soldierFramesAttack4 :: V.Vector MFrameT
soldierFramesAttack4 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 (Just soldierFire4)
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               , MFrameT (Just GameAI.aiCharge) 0 Nothing
               ]

soldierMoveAttack4 :: MMoveT
soldierMoveAttack4 = MMoveT frameAttak401 frameAttak406 soldierFramesAttack4 (Just soldierRun)

soldierFramesAttack6 :: V.Vector MFrameT
soldierFramesAttack6 =
    V.fromList [ MFrameT (Just GameAI.aiCharge) 10 Nothing
               , MFrameT (Just GameAI.aiCharge)  4 Nothing
               , MFrameT (Just GameAI.aiCharge) 12 Nothing
               , MFrameT (Just GameAI.aiCharge) 11 (Just soldierFire8)
               , MFrameT (Just GameAI.aiCharge) 13 Nothing
               , MFrameT (Just GameAI.aiCharge) 18 Nothing
               , MFrameT (Just GameAI.aiCharge) 15 Nothing
               , MFrameT (Just GameAI.aiCharge) 14 Nothing
               , MFrameT (Just GameAI.aiCharge) 11 Nothing
               , MFrameT (Just GameAI.aiCharge)  8 Nothing
               , MFrameT (Just GameAI.aiCharge) 11 Nothing
               , MFrameT (Just GameAI.aiCharge) 12 Nothing
               , MFrameT (Just GameAI.aiCharge) 12 Nothing
               , MFrameT (Just GameAI.aiCharge) 17 (Just soldierAttack6Refire)
               ]

soldierMoveAttack6 :: MMoveT
soldierMoveAttack6 = MMoveT frameRuns01 frameRuns14 soldierFramesAttack6 (Just soldierRun)

soldierFramesDuck :: V.Vector MFrameT
soldierFramesDuck =
    V.fromList [ MFrameT (Just GameAI.aiMove)   5  (Just soldierDuckDown)
               , MFrameT (Just GameAI.aiMove) (-1) (Just soldierDuckHold)
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  (Just soldierDuckUp)
               , MFrameT (Just GameAI.aiMove)   5  Nothing
               ]

soldierMoveDuck :: MMoveT
soldierMoveDuck = MMoveT frameDuck01 frameDuck05 soldierFramesDuck (Just soldierRun)

soldierAttack :: EntThink
soldierAttack =
  GenericEntThink "soldier_attack" $ \er@(EdictReference edictIdx) -> do
    Just skinNum <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSkinNum

    nextMove <- if skinNum < 4
                  then do
                    v <- Lib.randomF
                    return $ if v < 0.5 then soldierMoveAttack1 else soldierMoveAttack2
                  else return soldierMoveAttack4

    gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove .= Just nextMove

    return True

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
