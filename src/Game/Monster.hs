{-# LANGUAGE OverloadedStrings #-}
module Game.Monster where

import Control.Lens ((^.), preuse, (%=), ix, (+=), use, zoom, (.=), (&), (.~), (%~))
import Control.Monad (liftM, when, unless, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3, _x, _y, _z)
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
import qualified Game.GameWeapon as GameWeapon
import qualified QCommon.Com as Com
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

monsterFireBullet :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()
monsterFireBullet _ _ _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireBullet") >> undefined -- TODO

monsterFireShotgun :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
monsterFireShotgun selfRef start aimDir damage kick hspread vspread count flashType = do
    GameWeapon.fireShotgun selfRef start aimDir damage kick hspread vspread count Constants.modUnknown

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    self <- readEdictT selfRef

    writeByte Constants.svcMuzzleFlash2
    writeShort (self^.eIndex)
    writeByte flashType
    multicast start Constants.multicastPvs

monsterFireBlaster :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
monsterFireBlaster selfRef start dir damage speed flashType effect = do
    GameWeapon.fireBlaster selfRef start dir damage speed effect False

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    self <- readEdictT selfRef

    writeByte Constants.svcMuzzleFlash2
    writeShort (self^.eIndex)
    writeByte flashType
    multicast start Constants.multicastPvs

monsterStart :: EdictReference -> Quake Bool
monsterStart edictRef = do
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
          edict <- readEdictT edictRef
          when ((edict^.eSpawnFlags) .&. 4 /= 0 && (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy == 0) $ do
            modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.&. (complement 4))
                                           & eSpawnFlags %~ (.|. 1))

        updateMonsterCount :: Quake ()
        updateMonsterCount = do
          edict <- readEdictT edictRef
          let aiFlags = edict^.eMonsterInfo.miAIFlags
          when (aiFlags .&. Constants.aiGoodGuy == 0) $
            gameBaseGlobals.gbLevel.llTotalMonsters += 1

        updateSelf :: Quake ()
        updateSelf = do
          edict <- readEdictT edictRef
          time <- use $ gameBaseGlobals.gbLevel.llTime

          modifyEdictT edictRef (\v -> v & eNextThink .~ time + Constants.frameTime
                                         & eSvFlags %~ (.|. Constants.svfMonster)
                                         & eEntityState.esRenderFx %~ (.|. Constants.rfFrameLerp)
                                         & eTakeDamage .~ Constants.damageAim
                                         & eAirFinished .~ time + 12
                                         & eUse .~ Just (GameUtil.monsterUse)
                                         & eMaxHealth .~ (edict^.eHealth)
                                         & eClipMask .~ Constants.maskMonsterSolid
                                         & eEntityState.esSkinNum .~ 0
                                         & eDeadFlag .~ Constants.deadNo
                                         & eSvFlags %~ (.&. (complement Constants.svfDeadMonster))
                                         & eEntityState.esOldOrigin .~ (edict^.eEntityState.esOrigin))

          when (isNothing (edict^.eMonsterInfo.miCheckAttack)) $
            modifyEdictT edictRef (\v -> v & eMonsterInfo.miCheckAttack .~ Just GameUtil.mCheckAttack)

        setItem :: Quake ()
        setItem = do
          item <- use $ gameBaseGlobals.gbSpawnTemp.stItem

          when (B.length item > 0) $ do
            foundItem <- GameItems.findItemByClassname item
            modifyEdictT edictRef (\v -> v & eItem .~ foundItem)

            when (isNothing foundItem) $ do
              edict <- readEdictT edictRef
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              dprintf $ "monster_start:" `B.append` (edict^.eClassName) `B.append`
                        " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append`
                        " has bad item: " `B.append` item `B.append` "\n"

        randomizeStartFrame :: Quake ()
        randomizeStartFrame = do
          edict <- readEdictT edictRef
          let currentMove = edict^.eMonsterInfo.miCurrentMove
          when (isJust currentMove) $ do
            r <- Lib.rand
            let Just move = currentMove
                frame = (move^.mmFirstFrame) + ((fromIntegral r) `div` ((move^.mmLastFrame) - (move^.mmFirstFrame) + 1))
            modifyEdictT edictRef (\v -> v & eEntityState.esFrame .~ frame)

monsterStartGo :: EdictReference -> Quake ()
monsterStartGo selfRef = do
    self <- readEdictT selfRef

    unless ((self^.eHealth) <= 0) $ do
      -- check for target to combat_point and change to combattarget
      when (isJust (self^.eTarget)) $
        checkTarget (fromJust $ self^.eTarget)

      -- validate combattarget
      selff <- readEdictT selfRef
      let combatTarget = selff^.eCombatTarget
      when (isJust combatTarget) $
        validateCombatTarget (fromJust combatTarget) Nothing

      self' <- readEdictT selfRef

      case self'^.eTarget of
        Just target -> do
          pickedTargetRef <- GameBase.pickTarget (Just target)

          modifyEdictT selfRef (\v -> v & eGoalEntity .~ pickedTargetRef
                                        & eMoveTarget .~ pickedTargetRef)

          case pickedTargetRef of
            Nothing -> do
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              dprintf "IMPLEMENT ME! can't find target" -- TODO
              modifyEdictT selfRef (\v -> v & eTarget .~ Nothing
                                            & eMonsterInfo.miPauseTime .~ 100000000)

              void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

            Just targetRef -> do
              targetEdict <- readEdictT targetRef

              if (targetEdict^.eClassName) == "path_corner"
                then do
                  let Just goalEntityRef = pickedTargetRef
                  goalEntity <- readEdictT goalEntityRef
                  let v = (goalEntity^.eEntityState.esOrigin) - (self'^.eEntityState.esOrigin)
                      yaw = Math3D.vectorYaw v
                      access = case Constants.yaw of
                                 0 -> _x
                                 1 -> _y
                                 2 -> _z
                                 _ -> undefined -- shouldn't happen

                  modifyEdictT selfRef (\v -> v & eEntityState.esAngles.access .~ yaw
                                                & eIdealYaw .~ yaw)
                  
                  void $ think (fromJust $ self'^.eMonsterInfo.miWalk) selfRef

                  modifyEdictT selfRef (\v -> v & eTarget .~ Nothing)

                else do
                  modifyEdictT selfRef (\v -> v & eGoalEntity .~ Nothing
                                                & eMoveTarget .~ Nothing
                                                & eMonsterInfo.miPauseTime .~ 100000000)

                  void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

        Nothing -> do
          modifyEdictT selfRef (\v -> v & eMonsterInfo.miPauseTime .~ 100000000)
          void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      modifyEdictT selfRef (\v -> v & eThink .~ Just monsterThink
                                    & eNextThink .~ levelTime + Constants.frameTime)

  where checkTarget :: B.ByteString -> Quake ()
        checkTarget targetName = do
          (notCombat, fixup) <- checkTargets targetName False False Nothing
          self <- readEdictT selfRef

          when (notCombat && isJust (self^.eCombatTarget)) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ (self^.eClassName) `B.append` " at " `B.append` (Lib.vtos (self^.eEntityState.esOrigin)) `B.append` " has target with mixed types\n"

          when fixup $
            modifyEdictT selfRef (\v -> v & eTarget .~ Nothing)

        checkTargets :: B.ByteString -> Bool -> Bool -> Maybe EdictReference -> Quake (Bool, Bool)
        checkTargets targetName notCombat fixup ref = do
          foundRef <- GameBase.gFind ref GameBase.findByTarget targetName

          case foundRef of
            Nothing ->
              return (notCombat, fixup)

            Just targetRef -> do
              target <- readEdictT targetRef
              if (target^.eClassName) == "point_combat"
                then do
                  modifyEdictT selfRef (\v -> v & eCombatTarget .~ Just targetName)
                  checkTargets targetName notCombat True foundRef
                else
                  checkTargets targetName True fixup foundRef

        validateCombatTarget :: B.ByteString -> Maybe EdictReference -> Quake ()
        validateCombatTarget combatTarget ref = do
          foundRef <- GameBase.gFind ref GameBase.findByTarget combatTarget

          case foundRef of
            Nothing ->
              return ()

            Just foundEdictRef -> do
              foundEdict <- readEdictT foundEdictRef

              when ((foundEdict^.eClassName) /= "point_combat") $ do
                dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
                dprintf "IMPLEMENT ME! bad combattarget" -- TODO

              validateCombatTarget combatTarget foundRef

monsterTriggeredStart :: EntThink
monsterTriggeredStart =
  GenericEntThink "monster_triggered_start" $ \selfRef -> do
    self <- readEdictT selfRef
    let index = self^.eIndex

    when (index == 312) $
      Com.printf "monster_triggered_start\n"

    modifyEdictT selfRef (\v -> v & eSolid .~ Constants.solidNot
                                  & eMoveType .~ Constants.moveTypeNone
                                  & eSvFlags %~ (.|. Constants.svfNoClient)
                                  & eNextThink .~ 0
                                  & eUse .~ Just monsterTriggeredSpawnUse)

    return True

monsterThink :: EntThink
monsterThink =
  GenericEntThink "monster_think" $ \selfRef -> do
    M.moveFrame selfRef

    self <- readEdictT selfRef

    when ((self^.eLinkCount) /= (self^.eMonsterInfo.miLinkCount)) $ do
      modifyEdictT selfRef (\v -> v & eMonsterInfo.miLinkCount .~ (self^.eLinkCount))
      M.checkGround selfRef

    M.catagorizePosition selfRef
    M.worldEffects selfRef
    M.setEffects selfRef

    return True

monsterTriggeredSpawnUse :: EntUse
monsterTriggeredSpawnUse =
  GenericEntUse "monster_trigger_spawn_use" $ \_ _ _ -> do
    io (putStrLn "Monster.monsterTriggeredSpawnUse") >> undefined -- TODO

{-
- ================ monster_death_use ================
- 
- When a monster dies, it fires all of its targets with the current enemy
- as activator.
-}
monsterDeathUse :: EdictReference -> Quake ()
monsterDeathUse selfRef = do
    modifyEdictT selfRef (\v -> v & eFlags %~ (.&. (complement (Constants.flFly .|. Constants.flSwim)))
                                  & eMonsterInfo.miAIFlags %~ (.&. Constants.aiGoodGuy))


    self <- readEdictT selfRef

    when (isJust (self^.eItem)) $ do
      GameItems.dropItem selfRef (fromJust $ self^.eItem)
      modifyEdictT selfRef (\v -> v & eItem .~ Nothing)

    when (isJust (self^.eDeathTarget)) $
      modifyEdictT selfRef (\v -> v & eTarget .~ (self^.eDeathTarget))

    self' <- readEdictT selfRef

    when (isJust (self'^.eTarget)) $
      GameUtil.useTargets selfRef (self'^.eEnemy)

monsterFireRocket :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
monsterFireRocket _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireRocket") >> undefined -- TODO

monsterFireRailgun :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
monsterFireRailgun _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireRailgun") >> undefined -- TODO

monsterFireGrenade :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
monsterFireGrenade _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireGrenade") >> undefined -- TODO
