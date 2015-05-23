{-# LANGUAGE OverloadedStrings #-}
module Game.Monster where

import Control.Lens ((^.), preuse, (%=), ix, (+=), use, zoom, (.=))
import Control.Monad (liftM, when, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.ByteString as B

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

monsterStart :: EdictReference -> Quake Bool
monsterStart edictRef@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then do
        GameUtil.freeEdict edictRef
        return False
      else do
        updateSpawnFlags
        updateMonsterCount
        updateSelf
        setItem
        randomizeStartFrame
        return True

  where updateSpawnFlags :: Quake ()
        updateSpawnFlags = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          when ((edict^.eSpawnFlags) .&. 4 /= 0 && (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy == 0) $ do
            gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.&. (complement 4))
            gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. 1)

        updateMonsterCount :: Quake ()
        updateMonsterCount = do
          Just aiFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miAIFlags
          when (aiFlags .&. Constants.aiGoodGuy == 0) $
            gameBaseGlobals.gbLevel.llTotalMonsters += 1

        updateSelf :: Quake ()
        updateSelf = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          time <- use $ gameBaseGlobals.gbLevel.llTime

          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eEdictAction.eaNextThink .= time + Constants.frameTime
            eSvFlags %= (.|. Constants.svfMonster)
            eEntityState.esRenderFx %= (.|. Constants.rfFrameLerp)
            eEdictStatus.eTakeDamage .= Constants.damageAim
            eEdictPhysics.eAirFinished .= time + 12
            eEdictAction.eaUse .= Just (GameUtil.monsterUse)
            eEdictStatus.eMaxHealth .= (edict^.eEdictStatus.eHealth)
            eClipMask .= Constants.maskMonsterSolid

            eEntityState.esSkinNum .= 0
            eEdictStatus.eDeadFlag .= Constants.deadNo
            eSvFlags %= (.&. (complement Constants.svfDeadMonster))

            eEntityState.esOldOrigin .= (edict^.eEntityState.esOrigin)

          when (isNothing (edict^.eMonsterInfo.miCheckAttack)) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCheckAttack .= Just GameUtil.mCheckAttack

        setItem :: Quake ()
        setItem = do
          item <- use $ gameBaseGlobals.gbSpawnTemp.stItem

          when (B.length item > 0) $ do
            foundItem <- GameItems.findItemByClassname item
            gameBaseGlobals.gbGEdicts.ix edictIdx.eItem .= foundItem

            when (isNothing foundItem) $ do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              dprintf $ "monster_start:" `B.append` (edict^.eClassName) `B.append`
                        " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append`
                        " has bad item: " `B.append` item `B.append` "\n"

        randomizeStartFrame :: Quake ()
        randomizeStartFrame = do
          Just currentMove <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove
          when (isJust currentMove) $ do
            r <- Lib.rand
            let Just move = currentMove
                frame = (move^.mmFirstFrame) + ((fromIntegral r) `div` ((move^.mmLastFrame) - (move^.mmFirstFrame) + 1))
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= frame

monsterStartGo :: EdictReference -> Quake ()
monsterStartGo selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    unless ((self^.eEdictStatus.eHealth) <= 0) $ do
      -- check for target to combat_point and change to combattarget
      when (isJust (self^.eEdictInfo.eiTarget)) $
        checkTarget (fromJust $ self^.eEdictInfo.eiTarget)

      -- validate combattarget
      Just combatTarget <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictInfo.eiCombatTarget
      when (isJust combatTarget) $
        validateCombatTarget (fromJust combatTarget) Nothing

      Just self' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
      case self'^.eEdictInfo.eiTarget of
        Just target -> do
          pickedTarget <- GameBase.pickTarget (Just target)

          gameBaseGlobals.gbGEdicts.ix selfIdx.eGoalEntity .= pickedTarget
          gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveTarget .= pickedTarget

          case pickedTarget of
            Nothing -> do
              io (putStrLn "Monster.monsterStartGo") >> undefined -- TODO

            Just (EdictReference targetIdx) -> do
              Just targetEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix targetIdx
              if (targetEdict^.eClassName) == "path_corner"
                then do
                  io (putStrLn "Monster.monsterStartGo") >> undefined -- TODO
                else do
                  io (putStrLn "Monster.monsterStartGo") >> undefined -- TODO

        Nothing -> do
          gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miPauseTime .= 100000000
          think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      zoom (gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictAction) $ do
        eaThink .= Just monsterThink
        eaNextThink .= levelTime + Constants.frameTime

  where checkTarget :: B.ByteString -> Quake ()
        checkTarget targetName = do
          (notCombat, fixup) <- checkTargets targetName False False Nothing
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

          when (notCombat && isJust (self^.eEdictInfo.eiCombatTarget)) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ (self^.eClassName) `B.append` " at " `B.append` (Lib.vtos (self^.eEntityState.esOrigin)) `B.append` " has target with mixed types\n"

          when fixup $
            gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictInfo.eiTarget .= Nothing

        checkTargets :: B.ByteString -> Bool -> Bool -> Maybe EdictReference -> Quake (Bool, Bool)
        checkTargets targetName notCombat fixup ref = do
          foundRef <- GameBase.gFind ref GameBase.findByTarget targetName

          case foundRef of
            Nothing -> return (notCombat, fixup)
            targetRef@(Just (EdictReference targetIdx)) -> do
              Just target <- preuse $ gameBaseGlobals.gbGEdicts.ix targetIdx
              if (target^.eClassName) == "point_combat"
                then do
                  gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictInfo.eiCombatTarget .= Just targetName
                  checkTargets targetName notCombat True targetRef
                else
                  checkTargets targetName True fixup targetRef

        validateCombatTarget :: B.ByteString -> Maybe EdictReference -> Quake ()
        validateCombatTarget combatTarget ref = do
          foundRef <- GameBase.gFind ref GameBase.findByTarget combatTarget

          case foundRef of
            Nothing -> return ()
            Just (EdictReference foundIdx) -> do
              Just foundEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix foundIdx

              when ((foundEdict^.eClassName) /= "point_combat") $ do
                dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
                dprintf "IMPLEMENT ME! bad combattarget" -- TODO

              validateCombatTarget combatTarget foundRef

monsterTriggeredStart :: EntThink
monsterTriggeredStart =
  GenericEntThink "monster_triggered_start" $ \_ -> do
    io (putStrLn "Monster.monsterTriggeredStart") >> undefined -- TODO

monsterThink :: EntThink
monsterThink =
  GenericEntThink "monster_think" $ \_ -> do
    io (putStrLn "Monster.monsterThink") >> undefined -- TODO
