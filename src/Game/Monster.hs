{-# LANGUAGE OverloadedStrings #-}
module Game.Monster where

import Control.Lens ((^.), preuse, (%=), ix, (+=), use, zoom, (.=), (&), (.~), (%~))
import Control.Monad (liftM, when, unless, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3, _x, _y, _z)
import qualified Data.ByteString as B

import {-# SOURCE #-} Game.GameImportT
import Game.SpawnTempT
import Game.LevelLocalsT
import Game.CVarT
import Game.EntityStateT
import Game.EdictT
import Game.MMoveT
import Game.MonsterInfoT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Game.GameWeapon as GameWeapon
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

monsterFireBullet :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()
monsterFireBullet _ _ _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireBullet") >> undefined -- TODO

monsterFireShotgun :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
monsterFireShotgun selfRef start aimDir damage kick hspread vspread count flashType = do
    GameWeapon.fireShotgun selfRef start aimDir damage kick hspread vspread count Constants.modUnknown

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    self <- readRef selfRef

    writeByte Constants.svcMuzzleFlash2
    writeShort (self^.eIndex)
    writeByte flashType
    multicast start Constants.multicastPvs

monsterFireBlaster :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
monsterFireBlaster selfRef start dir damage speed flashType effect = do
    GameWeapon.fireBlaster selfRef start dir damage speed effect False

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    self <- readRef selfRef

    writeByte Constants.svcMuzzleFlash2
    writeShort (self^.eIndex)
    writeByte flashType
    multicast start Constants.multicastPvs

monsterStart :: Ref EdictT -> Quake Bool
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
          edict <- readRef edictRef
          when ((edict^.eSpawnFlags) .&. 4 /= 0 && (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy == 0) $ do
            modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. (complement 4))
                                           & eSpawnFlags %~ (.|. 1))

        updateMonsterCount :: Quake ()
        updateMonsterCount = do
          edict <- readRef edictRef
          let aiFlags = edict^.eMonsterInfo.miAIFlags
          when (aiFlags .&. Constants.aiGoodGuy == 0) $
            gameBaseGlobals.gbLevel.llTotalMonsters += 1

        updateSelf :: Quake ()
        updateSelf = do
          edict <- readRef edictRef
          time <- use $ gameBaseGlobals.gbLevel.llTime

          modifyRef edictRef (\v -> v & eNextThink .~ time + Constants.frameTime
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
            modifyRef edictRef (\v -> v & eMonsterInfo.miCheckAttack .~ Just GameUtil.mCheckAttack)

        setItem :: Quake ()
        setItem = do
          mItem <- use $ gameBaseGlobals.gbSpawnTemp.stItem
          
          case mItem of
            Nothing ->
              return ()
              
            Just item -> do
              when (B.length item > 0) $ do
                foundItem <- GameItems.findItemByClassname item
                modifyRef edictRef (\v -> v & eItem .~ foundItem)
    
                when (isNothing foundItem) $ do
                  edict <- readRef edictRef
                  dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
                  dprintf $ "monster_start:" `B.append` (edict^.eClassName) `B.append`
                            " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append`
                            " has bad item: " `B.append` item `B.append` "\n"

        randomizeStartFrame :: Quake ()
        randomizeStartFrame = do
          edict <- readRef edictRef
          let currentMove = edict^.eMonsterInfo.miCurrentMove
          when (isJust currentMove) $ do
            r <- Lib.rand
            let Just move = currentMove
                frame = (move^.mmFirstFrame) + ((fromIntegral r) `div` ((move^.mmLastFrame) - (move^.mmFirstFrame) + 1))
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ frame)

monsterStartGo :: Ref EdictT -> Quake ()
monsterStartGo selfRef = do
    self <- readRef selfRef

    unless ((self^.eHealth) <= 0) $ do
      -- check for target to combat_point and change to combattarget
      when (isJust (self^.eTarget)) $
        checkTarget (fromJust $ self^.eTarget)

      -- validate combattarget
      selff <- readRef selfRef
      let combatTarget = selff^.eCombatTarget
      when (isJust combatTarget) $
        validateCombatTarget (fromJust combatTarget) Nothing

      self' <- readRef selfRef

      case self'^.eTarget of
        Just target -> do
          pickedTargetRef <- GameBase.pickTarget (Just target)

          modifyRef selfRef (\v -> v & eGoalEntity .~ pickedTargetRef
                                        & eMoveTarget .~ pickedTargetRef)

          case pickedTargetRef of
            Nothing -> do
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              dprintf "IMPLEMENT ME! can't find target" -- TODO
              modifyRef selfRef (\v -> v & eTarget .~ Nothing
                                            & eMonsterInfo.miPauseTime .~ 100000000)

              void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

            Just targetRef -> do
              targetEdict <- readRef targetRef

              if (targetEdict^.eClassName) == "path_corner"
                then do
                  let Just goalEntityRef = pickedTargetRef
                  goalEntity <- readRef goalEntityRef
                  let v = (goalEntity^.eEntityState.esOrigin) - (self'^.eEntityState.esOrigin)
                      yaw = Math3D.vectorYaw v
                      access = case Constants.yaw of
                                 0 -> _x
                                 1 -> _y
                                 2 -> _z
                                 _ -> undefined -- shouldn't happen

                  modifyRef selfRef (\v -> v & eEntityState.esAngles.access .~ yaw
                                                & eIdealYaw .~ yaw)
                  
                  void $ think (fromJust $ self'^.eMonsterInfo.miWalk) selfRef

                  modifyRef selfRef (\v -> v & eTarget .~ Nothing)

                else do
                  modifyRef selfRef (\v -> v & eGoalEntity .~ Nothing
                                                & eMoveTarget .~ Nothing
                                                & eMonsterInfo.miPauseTime .~ 100000000)

                  void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

        Nothing -> do
          modifyRef selfRef (\v -> v & eMonsterInfo.miPauseTime .~ 100000000)
          void $ think (fromJust $ self'^.eMonsterInfo.miStand) selfRef

      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      modifyRef selfRef (\v -> v & eThink .~ Just monsterThink
                                    & eNextThink .~ levelTime + Constants.frameTime)

  where checkTarget :: B.ByteString -> Quake ()
        checkTarget targetName = do
          (notCombat, fixup) <- checkTargets targetName False False Nothing
          self <- readRef selfRef

          when (notCombat && isJust (self^.eCombatTarget)) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ (self^.eClassName) `B.append` " at " `B.append` (Lib.vtos (self^.eEntityState.esOrigin)) `B.append` " has target with mixed types\n"

          when fixup $
            modifyRef selfRef (\v -> v & eTarget .~ Nothing)

        checkTargets :: B.ByteString -> Bool -> Bool -> Maybe (Ref EdictT) -> Quake (Bool, Bool)
        checkTargets targetName notCombat fixup ref = do
          foundRef <- GameBase.gFind ref GameBase.findByTarget targetName

          case foundRef of
            Nothing ->
              return (notCombat, fixup)

            Just targetRef -> do
              target <- readRef targetRef
              if (target^.eClassName) == "point_combat"
                then do
                  modifyRef selfRef (\v -> v & eCombatTarget .~ Just targetName)
                  checkTargets targetName notCombat True foundRef
                else
                  checkTargets targetName True fixup foundRef

        validateCombatTarget :: B.ByteString -> Maybe (Ref EdictT) -> Quake ()
        validateCombatTarget combatTarget ref = do
          foundRef <- GameBase.gFind ref GameBase.findByTarget combatTarget

          case foundRef of
            Nothing ->
              return ()

            Just foundEdictRef -> do
              foundEdict <- readRef foundEdictRef

              when ((foundEdict^.eClassName) /= "point_combat") $ do
                dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
                dprintf "IMPLEMENT ME! bad combattarget" -- TODO

              validateCombatTarget combatTarget foundRef

monsterTriggeredStart :: EntThink
monsterTriggeredStart =
  GenericEntThink "monster_triggered_start" $ \selfRef -> do
    self <- readRef selfRef
    let index = self^.eIndex

    when (index == 312) $
      Com.printf "monster_triggered_start\n"

    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidNot
                                  & eMoveType .~ Constants.moveTypeNone
                                  & eSvFlags %~ (.|. Constants.svfNoClient)
                                  & eNextThink .~ 0
                                  & eUse .~ Just monsterTriggeredSpawnUse)

    return True

monsterThink :: EntThink
monsterThink =
  GenericEntThink "monster_think" $ \selfRef -> do
    M.moveFrame selfRef

    self <- readRef selfRef

    when ((self^.eLinkCount) /= (self^.eMonsterInfo.miLinkCount)) $ do
      modifyRef selfRef (\v -> v & eMonsterInfo.miLinkCount .~ (self^.eLinkCount))
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
monsterDeathUse :: Ref EdictT -> Quake ()
monsterDeathUse selfRef = do
    modifyRef selfRef (\v -> v & eFlags %~ (.&. (complement (Constants.flFly .|. Constants.flSwim)))
                                  & eMonsterInfo.miAIFlags %~ (.&. Constants.aiGoodGuy))


    self <- readRef selfRef

    when (isJust (self^.eItem)) $ do
      GameItems.dropItem selfRef (fromJust $ self^.eItem)
      modifyRef selfRef (\v -> v & eItem .~ Nothing)

    when (isJust (self^.eDeathTarget)) $
      modifyRef selfRef (\v -> v & eTarget .~ (self^.eDeathTarget))

    self' <- readRef selfRef

    when (isJust (self'^.eTarget)) $
      GameUtil.useTargets selfRef (self'^.eEnemy)

monsterFireRocket :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
monsterFireRocket _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireRocket") >> undefined -- TODO

monsterFireRailgun :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
monsterFireRailgun _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireRailgun") >> undefined -- TODO

monsterFireGrenade :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
monsterFireGrenade _ _ _ _ _ _ = do
    io (putStrLn "Monster.monsterFireGrenade") >> undefined -- TODO

monsterFireBFG :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Float -> Int -> Quake ()
monsterFireBFG selfRef start aimDir damage speed kick damageRadius flashType = do
    GameWeapon.fireBFG selfRef start aimDir damage speed damageRadius
    
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast
    
    writeByte Constants.svcMuzzleFlash2
    writeShort (self^.eIndex)
    writeByte flashType
    multicast start Constants.multicastPvs
