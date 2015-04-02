{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MInfantry where

import Control.Lens ((^.), use, (.=), ix, zoom)
import Control.Monad (liftM, void)
import Linear (V3(..))
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import Game.MFrameT
import Game.MMoveT
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Game.GameUtil as GameUtil

modelScale :: Float
modelScale = 1

frameStand50 :: Int
frameStand50 = 50

frameStand71 :: Int
frameStand71 = 71

infantryFramesStand :: V.Vector MFrameT
infantryFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

infantryPain :: EntPain
infantryPain =
  GenericEntPain "infantry_pain" $ \_ _ _ _ -> do
    io (putStrLn "MInfantry.infantryPain") >> undefined -- TODO

infantryDie :: EntDie
infantryDie =
  GenericEntDie "infantry_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MInfantry.infantryDie") >> undefined -- TODO

infantryStand :: EntThink
infantryStand =
  GenericEntThink "infantry_stand" $ \_ -> do
    io (putStrLn "MInfantry.infantryStand") >> undefined -- TODO

infantryWalk :: EntThink
infantryWalk =
  GenericEntThink "infantry_walk" $ \_ -> do
    io (putStrLn "MInfantry.infantryWalk") >> undefined -- TODO

infantryRun :: EntThink
infantryRun =
  GenericEntThink "infantry_run" $ \_ -> do
    io (putStrLn "MInfantry.infantryRun") >> undefined -- TODO

infantryDodge :: EntDodge
infantryDodge =
  GenericEntDodge "infantry_dodge" $ \_ _ _ -> do
    io (putStrLn "MInfantry.infantryDodge") >> undefined -- TODO

infantryAttack :: EntThink
infantryAttack =
  GenericEntThink "infantry_attack" $ \_ -> do
    io (putStrLn "MInfantry.infantryAttack") >> undefined -- TODO

infantrySight :: EntInteract
infantrySight =
  GenericEntInteract "infantry_sight" $ \_ _ -> do
    io (putStrLn "MInfantry.infantrySight") >> undefined -- TODO

infantryFidget :: EntThink
infantryFidget =
  GenericEntThink "infantry_fidget" $ \_ -> do
    io (putStrLn "MInfantry.infantryFidget") >> undefined -- TODO

infantryMoveStand :: MMoveT
infantryMoveStand = MMoveT frameStand50 frameStand71 infantryFramesStand Nothing

{-
- QUAKED monster_infantry (1 .5 0) (-16 -16 -24) (16 16 32) Ambush
- Trigger_Spawn Sight
-}
spMonsterInfantry :: EdictReference -> Quake ()
spMonsterInfantry er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport
        let soundIndex = gameImport^.giSoundIndex
            modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        soundIndex "infantry/infpain1.wav" >>= (mInfantryGlobals.miSoundPain1 .=)
        soundIndex "infantry/infpain2.wav" >>= (mInfantryGlobals.miSoundPain2 .=)
        soundIndex "infantry/infdeth1.wav" >>= (mInfantryGlobals.miSoundDie1 .=)
        soundIndex "infantry/infdeth2.wav" >>= (mInfantryGlobals.miSoundDie2 .=)

        soundIndex "infantry/infatck1.wav" >>= (mInfantryGlobals.miSoundGunShot .=)
        soundIndex "infantry/infatck3.wav" >>= (mInfantryGlobals.miSoundWeaponCock .=)
        soundIndex "infantry/infatck2.wav" >>= (mInfantryGlobals.miSoundPunchSwing .=)
        soundIndex "infantry/melee2.wav" >>= (mInfantryGlobals.miSoundPunchHit .=)

        soundIndex "infantry/infsght1.wav" >>= (mInfantryGlobals.miSoundSight .=)
        soundIndex "infantry/infsrch1.wav" >>= (mInfantryGlobals.miSoundSearch .=)
        soundIndex "infantry/infidle1.wav" >>= (mInfantryGlobals.miSoundIdle .=)

        tris <- modelIndex "models/monsters/infantry/tris.md2"

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eMoveType                 .= Constants.moveTypeStep
          eSolid                    .= Constants.solidBbox
          eEntityState.esModelIndex .= tris
          eEdictMinMax.eMins        .= V3 (-16) (-16) (-24)
          eEdictMinMax.eMaxs        .= V3 16 16 32

          eEdictStatus.eHealth    .= 100
          eEdictStatus.eGibHealth .= -40
          eEdictPhysics.eMass     .= 200

          eEdictAction.eaPain .= Just infantryPain
          eEdictAction.eaDie  .= Just infantryDie

          eMonsterInfo.miStand  .= Just infantryStand
          eMonsterInfo.miWalk   .= Just infantryWalk
          eMonsterInfo.miRun    .= Just infantryRun
          eMonsterInfo.miDodge  .= Just infantryDodge
          eMonsterInfo.miAttack .= Just infantryAttack
          eMonsterInfo.miMelee  .= Nothing
          eMonsterInfo.miSight  .= Just infantrySight
          eMonsterInfo.miIdle   .= Just infantryFidget

        linkEntity er

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo) $ do
          miCurrentMove .= Just infantryMoveStand
          miScale       .= modelScale

        void $ think GameAI.walkMonsterStart er
