{-# LANGUAGE OverloadedStrings #-}
module Game.Monster where

import Control.Lens ((^.), preuse, (%=), ix, (+=), use, zoom, (.=))
import Control.Monad (liftM, when, unless, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (_x, _y, _z)
import qualified Data.ByteString as B

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified QCommon.Com as Com
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

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
            eNextThink .= time + Constants.frameTime
            eSvFlags %= (.|. Constants.svfMonster)
            eEntityState.esRenderFx %= (.|. Constants.rfFrameLerp)
            eTakeDamage .= Constants.damageAim
            eEdictPhysics.eAirFinished .= time + 12
            eUse .= Just (GameUtil.monsterUse)
            eMaxHealth .= (edict^.eHealth)
            eClipMask .= Constants.maskMonsterSolid

            eEntityState.esSkinNum .= 0
            eDeadFlag .= Constants.deadNo
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

    unless ((self^.eHealth) <= 0) $ do
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
          pickedTargetRef <- GameBase.pickTarget (Just target)

          gameBaseGlobals.gbGEdicts.ix selfIdx.eGoalEntity .= pickedTargetRef
          gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveTarget .= pickedTargetRef

          case pickedTargetRef of
            Nothing -> do
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              dprintf "IMPLEMENT ME! can't find target" -- TODO
              gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictInfo.eiTarget .= Nothing
              gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miPauseTime .= 100000000

              void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

            Just (EdictReference targetIdx) -> do
              Just targetEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix targetIdx
              if (targetEdict^.eClassName) == "path_corner"
                then do
                  let Just (EdictReference goalEntityIdx) = pickedTargetRef
                  Just goalEntity <- preuse $ gameBaseGlobals.gbGEdicts.ix goalEntityIdx
                  let v = (goalEntity^.eEntityState.esOrigin) - (self'^.eEntityState.esOrigin)
                      yaw = Math3D.vectorYaw v
                      access = case Constants.yaw of
                                 0 -> _x
                                 1 -> _y
                                 2 -> _z
                                 _ -> undefined -- shouldn't happen

                  gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esAngles.access .= yaw
                  gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictPhysics.eIdealYaw .= yaw
                  
                  void $ think (fromJust $ self'^.eMonsterInfo.miWalk) selfRef

                  gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictInfo.eiTarget .= Nothing

                else do
                  gameBaseGlobals.gbGEdicts.ix selfIdx.eGoalEntity .= Nothing
                  gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveTarget .= Nothing
                  gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miPauseTime .= 100000000

                  void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

        Nothing -> do
          gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miPauseTime .= 100000000
          void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
        eThink .= Just monsterThink
        eNextThink .= levelTime + Constants.frameTime

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
  GenericEntThink "monster_triggered_start" $ \(EdictReference selfIdx) -> do
    Just index <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eIndex

    when (index == 312) $
      Com.printf "monster_triggered_start\n"

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eSolid .= Constants.solidNot
      eMoveType .= Constants.moveTypeNone
      eSvFlags %= (.|. Constants.svfNoClient)
      eNextThink .= 0
      eUse .= Just monsterTriggeredSpawnUse

    return True

monsterThink :: EntThink
monsterThink =
  GenericEntThink "monster_think" $ \selfRef@(EdictReference selfIdx) -> do
    M.moveFrame selfRef

    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eLinkCount) /= (self^.eMonsterInfo.miLinkCount)) $ do
      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miLinkCount .= self^.eLinkCount
      M.checkGround selfRef

    M.catagorizePosition selfRef
    M.worldEffects selfRef
    M.setEffects selfRef

    return True

monsterTriggeredSpawnUse :: EntUse
monsterTriggeredSpawnUse =
  GenericEntUse "monster_trigger_spawn_use" $ \_ _ _ -> do
    io (putStrLn "Monster.monsterTriggeredSpawnUse") >> undefined -- TODO
