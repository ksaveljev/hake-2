{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameCombat ( damage
                       , radiusDamage
                       ) where

import Control.Lens (use, preuse, (^.), ix, (.=), (+=), (%=), zoom, (-=), (&), (.~), (%~), (+~), (-~))
import Control.Monad (when, unless, liftM)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (V3(..), norm, normalize, dot)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.GItemArmorT
import Game.GClientT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Types
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Game.Monster as Monster
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified Util.Math3D as Math3D

radiusDamage :: EdictReference -> EdictReference -> Float -> Maybe EdictReference -> Float -> Int -> Quake ()
radiusDamage inflictorRef attackerRef dmg ignoreRef radius mod' = do
    radiusDamage' Nothing

  where radiusDamage' :: Maybe EdictReference -> Quake ()
        radiusDamage' edictRef = do
          inflictor <- readEdictT inflictorRef
          edictit <- GameBase.findRadius edictRef (inflictor^.eEntityState.esOrigin) radius

          case edictit of
            Nothing -> return ()

            Just entRef -> do
              ent <- readEdictT entRef

              if | edictit == ignoreRef -> radiusDamage' edictit
                 | (ent^.eTakeDamage) == 0 -> radiusDamage' edictit
                 | otherwise -> do
                     let v = (ent^.eMins) + (ent^.eMaxs)
                         v' = (ent^.eEntityState.esOrigin) + fmap (* 0.5) v
                         v'' = (inflictor^.eEntityState.esOrigin) - v'
                         points = dmg - 0.5 * (norm v'')
                         points' = if entRef == attackerRef
                                     then points * 0.5
                                     else points

                     when (points' > 0) $ do
                       doDamage <- canDamage entRef inflictorRef
                       when doDamage $ do
                         let dir = (ent^.eEntityState.esOrigin) - (inflictor^.eEntityState.esOrigin)
                         v3o <- use $ globals.gVec3Origin
                         damage entRef inflictorRef attackerRef dir (inflictor^.eEntityState.esOrigin) v3o (truncate points') (truncate points') Constants.damageRadius mod'

                     radiusDamage' edictit

damage :: EdictReference -> EdictReference -> EdictReference -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
damage targRef inflictorRef attackerRef dir point normal damage knockback dflags mod' = do
    targ <- readEdictT targRef

    unless ((targ^.eTakeDamage) == 0) $ do
      -- friendly fire avoidance
      -- if enabled you can't hurt teammates (but you can hurt yourself)
      -- knockback still occurs
      deathmatchValue <- liftM (^.cvValue) deathmatchCVar
      dmFlags <- liftM (truncate . (^.cvValue)) dmFlagsCVar
      skillValue <- liftM (^.cvValue) skillCVar
      coopValue <- liftM (^.cvValue) coopCVar

      attacker <- readEdictT attackerRef

      (damage', mod'') <- updateDamageAndMod targ deathmatchValue dmFlags skillValue coopValue

      let client = targ^.eClient
          sparks = if dflags .&. Constants.damageBullet /= 0
                     then Constants.teBulletSparks
                     else Constants.teSparks
          dir' = normalize dir
          -- bonus damage for suprising a monster
          damage'' = if dflags .&. Constants.damageRadius == 0 && (targ^.eSvFlags) .&. Constants.svfMonster /= 0 && isJust (attacker^.eClient) && isNothing (targ^.eEnemy) && (targ^.eHealth) > 0
                       then damage' * 2
                       else damage'
          knockback' = if (targ^.eFlags) .&. Constants.flNoKnockback /= 0
                         then 0
                         else knockback

      -- figure momentum add
      when (dflags .&. Constants.damageNoKnockback == 0 && knockback' /= 0 && (targ^.eMoveType) /= Constants.moveTypeNone && (targ^.eMoveType) /= Constants.moveTypeBounce && (targ^.eMoveType) /= Constants.moveTypePush && (targ^.eMoveType) /= Constants.moveTypeStop) $ do
        let mass = fromIntegral $ if (targ^.eMass) < 50
                                    then 50
                                    else targ^.eMass
            kvel = if isJust (targ^.eClient) && attackerRef == targRef
                     then fmap (* (1600 * fromIntegral knockback' / mass)) dir'
                     -- the rocket jump hack...
                     else fmap (* (500 * fromIntegral knockback' / mass)) dir'

        modifyEdictT targRef (\v -> v & eVelocity +~ kvel)

      -- check for godmode
      (take, save) <- if (targ^.eFlags) .&. Constants.flGodMode /= 0 && dflags .&. Constants.damageNoProtection == 0
                        then do
                          spawnDamage sparks point normal damage''
                          return (0, damage'')
                        else
                          return (damage'', 0)

      -- check for invincibility
      (take', save') <- if isJust client
                          then do
                            let Just (GClientReference gClientIdx) = client
                            Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
                            levelFrameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

                            if (gClient^.gcInvincibleFrameNum) > fromIntegral levelFrameNum && dflags .&. Constants.damageNoProtection == 0
                              then do
                                levelTime <- use $ gameBaseGlobals.gbLevel.llTime

                                when ((targ^.ePainDebounceTime) < levelTime) $ do
                                  sound <- use $ gameBaseGlobals.gbGameImport.giSound
                                  soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

                                  soundIdx <- soundIndex (Just "items/protect4.wav")
                                  sound (Just targRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0
                                  modifyEdictT targRef (\v -> v & ePainDebounceTime .~ levelTime + 2)

                                return (0, damage'')
                              else
                                return (take, save)
                          else
                            return (take, save)

      psave <- checkPowerArmor targRef point normal take' dflags
      let take'' = take' - psave

      asave <- checkArmor targRef point normal take'' sparks dflags
      let take''' = take'' - asave
          -- treat cheat/powerup savings the same as armor
          asave' = asave + save'

      -- team damage avoidance
      teamDamage <- checkTeamDamage targRef attackerRef

      unless (dflags .&. Constants.damageNoProtection == 0 && teamDamage) $ do
        -- do the damage

        done <- if take''' /= 0
                  then do
                    if (targ^.eSvFlags) .&. Constants.svfMonster /= 0 || isJust client
                      then spawnDamage Constants.teBlood point normal take'''
                      else spawnDamage sparks point normal take'''
                    
                    modifyEdictT targRef (\v -> v & eHealth .~ (targ^.eHealth) - take''')

                    if (targ^.eHealth) - take''' <= 0
                      then do
                        when ((targ^.eSvFlags) .&. Constants.svfMonster /= 0 || isJust client) $
                          modifyEdictT targRef (\v -> v & eFlags %~ (.|. Constants.flNoKnockback))

                        killed targRef inflictorRef attackerRef take''' point
                        return True
                      else
                        return False
                  else
                    return False

        unless done $ do
          if | (targ^.eSvFlags) .&. Constants.svfMonster /= 0 -> do
                 reactToDamage targRef attackerRef
                 targ' <- readEdictT targRef

                 when ((targ'^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked == 0 && take''' /= 0) $ do
                   pain (fromJust $ targ'^.ePain) targRef attackerRef (fromIntegral knockback) take'''
                   -- nightmare mode monsters don't go into pain frames often
                   when (skillValue == 3) $ do
                     levelTime <- use $ gameBaseGlobals.gbLevel.llTime
                     modifyEdictT targRef (\v -> v & ePainDebounceTime .~ levelTime + 5)

             | isJust client -> do
                 when ((targ^.eFlags) .&. Constants.flGodMode == 0 && take''' /= 0) $
                   pain (fromJust $ targ^.ePain) targRef attackerRef (fromIntegral knockback) take'''

             | take''' /= 0 -> do
                 when (isJust (targ^.ePain)) $
                   pain (fromJust $ targ^.ePain) targRef attackerRef (fromIntegral knockback) take'''

             | otherwise ->
                 return ()

          -- add to the damage inflicted on a player this frame
          -- the total will be turned into screen blends and view angle kicks
          -- at the end of the frame
          when (isJust client) $ do
            let Just (GClientReference gClientIdx) = client
            zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
              gcDamagePArmor += psave
              gcDamageArmor += asave'
              gcDamageBlood += take'''
              gcDamageKnockback += knockback
              gcDamageFrom .= point

  where updateDamageAndMod :: EdictT -> Float -> Int -> Float -> Float -> Quake (Int, Int)
        updateDamageAndMod targ deathmatchValue dmFlags skillValue coopValue = do
          (damage', mod'') <- if targRef /= attackerRef && ((deathmatchValue /= 0 && (dmFlags .&. (Constants.dfModelTeams .|. Constants.dfSkinTeams)) /= 0) || coopValue /= 0)
                                then do
                                  sameTeam <- GameUtil.onSameTeam targRef attackerRef

                                  if sameTeam
                                    then return $ if dmFlags .&. Constants.dfNoFriendlyFire /= 0
                                                    then (0, mod')
                                                    else (damage, mod' .|. Constants.modFriendlyFire)
                                    else
                                      return (damage, mod')
                                else
                                  return (damage, mod')

          gameBaseGlobals.gbMeansOfDeath .= mod''

          if skillValue == 0 && deathmatchValue == 0 && isJust (targ^.eClient)
            then do
              let damage'' = if damage' == 0
                               then 1
                               else damage' `div` 2

              return (damage'', mod'')
            else
              return (damage', mod'')

{-
- CanDamage
- 
- Returns true if the inflictor can directly damage the target. Used for
- explosions and melee attacks.
-}
canDamage :: EdictReference -> EdictReference -> Quake Bool
canDamage targRef inflictorRef = do
    targ <- readEdictT targRef
    inflictor <- readEdictT inflictorRef

    v3o <- use $ globals.gVec3Origin
    trace <- use $ gameBaseGlobals.gbGameImport.giTrace

    -- bmodels need special checking because their origin is 0,0,0
    if (targ^.eMoveType) == Constants.moveTypePush
      then do
        let dest = fmap (* 0.5) ((targ^.eAbsMin) + (targ^.eAbsMax))

        traceT <- trace (inflictor^.eEntityState.esOrigin) (Just v3o) (Just v3o) dest (Just inflictorRef) Constants.maskSolid

        if | (traceT^.tFraction) == 1.0 -> return True
           | (traceT^.tEnt) == Just targRef -> return True
           | otherwise -> return False

      else do
        traceT <- trace (inflictor^.eEntityState.esOrigin) (Just v3o) (Just v3o) (targ^.eEntityState.esOrigin) (Just inflictorRef) Constants.maskSolid

        if (traceT^.tFraction) == 1.0
          then
            return True

          else do
            let V3 a b c = (targ^.eEntityState.esOrigin)
                dest = V3 (a + 15) (b + 15) c

            traceT' <- trace (inflictor^.eEntityState.esOrigin) (Just v3o) (Just v3o) dest (Just inflictorRef) Constants.maskSolid

            if (traceT'^.tFraction) == 1.0
              then
                return True

              else do
                let dest' = V3 (a + 15) (b - 15) c
                
                traceT'' <- trace (inflictor^.eEntityState.esOrigin) (Just v3o) (Just v3o) dest' (Just inflictorRef) Constants.maskSolid

                if (traceT''^.tFraction) == 1.0
                  then
                    return True

                  else do
                    let dest'' = V3 (a - 15) (b + 15) c

                    traceT''' <- trace (inflictor^.eEntityState.esOrigin) (Just v3o) (Just v3o) dest'' (Just inflictorRef) Constants.maskSolid

                    if (traceT'''^.tFraction) == 1.0
                      then
                        return True

                      else do
                        let dest''' = V3 (a - 15) (b - 15) c

                        traceT'''' <- trace (inflictor^.eEntityState.esOrigin) (Just v3o) (Just v3o) dest''' (Just inflictorRef) Constants.maskSolid

                        return $ if (traceT''''^.tFraction) == 1.0
                                   then True
                                   else False

spawnDamage :: Int -> V3 Float -> V3 Float -> Int -> Quake ()
spawnDamage dmgType origin normal damage = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        writeDir = gameImport^.giWriteDir
        multicast = gameImport^.giMulticast
        damage' = if damage > 255 then 255 else damage

    writeByte Constants.svcTempEntity
    writeByte dmgType
    -- writeByte damage' -- commented out in jake2 and original quake2 source
    writePosition origin
    writeDir normal
    multicast origin Constants.multicastPvs

checkPowerArmor :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Quake Int
checkPowerArmor edictRef point normal damage dflags = do
    if | damage == 0 -> return 0
       | dflags .&. Constants.damageNoArmor /= 0 -> return 0
       | otherwise -> do
           edict <- readEdictT edictRef

           if | isJust (edict^.eClient) -> do
                  powerArmorType <- GameItems.powerArmorType edictRef

                  if powerArmorType /= Constants.powerArmorNone
                    then do
                      Just (GItemReference itemIdx) <- GameItems.findItem "Cells"
                      Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx

                      let Just (GClientReference gClientIdx) = edict^.eClient
                      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
                      let power = (gClient^.gcPers.cpInventory) UV.! (item^.giIndex)

                      continueChecking powerArmorType power
                    else
                      continueChecking powerArmorType 0

              | (edict^.eSvFlags) .&. Constants.svfMonster /= 0 ->
                  continueChecking (edict^.eMonsterInfo.miPowerArmorType) (edict^.eMonsterInfo.miPowerArmorPower)

              | otherwise ->
                  return 0

  where continueChecking :: Int -> Int -> Quake Int
        continueChecking powerAmorType power = do
          if | powerAmorType == Constants.powerArmorNone -> return 0
             | power == 0 -> return 0
             | otherwise -> do
                 edict <- readEdictT edictRef

                 let maybeDamageInfo = if powerAmorType == Constants.powerArmorScreen
                                         then let (Just forward, _, _) = Math3D.angleVectors (edict^.eEntityState.esAngles) True False False
                                                  vec = normalize (point - (edict^.eEntityState.esOrigin))
                                                  dot' = vec `dot` forward
                                              in if dot' <= 0.3
                                                   then Nothing
                                                   else Just (1, Constants.teScreenSparks, damage `div` 3)

                                         else Just (2, Constants.teShieldSparks, (2 * damage) `div` 3)

                 case maybeDamageInfo of
                   Nothing ->
                     return 0

                   Just (damagePerCell, paTeType, damage') -> do
                     let save = power * damagePerCell

                     if save == 0
                       then return 0
                       else do
                         let save' = if save > damage'
                                       then damage'
                                       else save

                         spawnDamage paTeType point normal save'

                         levelTime <- use $ gameBaseGlobals.gbLevel.llTime
                         modifyEdictT edictRef (\v -> v & ePowerArmorTime .~ levelTime + 0.2)

                         let powerUsed = save' `div` damagePerCell

                         case edict^.eClient of
                           Nothing ->
                            modifyEdictT edictRef (\v -> v & eMonsterInfo.miPowerArmorPower -~ powerUsed)

                           Just (GClientReference gClientIdx) -> do
                             Just (GItemReference itemIdx) <- GameItems.findItem "Cells"
                             Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx
                             gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (item^.giIndex) -= powerUsed

                         return save'

checkArmor :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake Int
checkArmor edictRef point normal damage sparks dflags = do
    edict <- readEdictT edictRef

    if | damage == 0 -> return 0
       | isNothing (edict^.eClient) -> return 0
       | dflags .&. Constants.damageNoArmor /= 0 -> return 0
       | otherwise -> do
           index <- GameItems.armorIndex edictRef

           if index == 0
             then
               return 0

             else do
               Just item <- GameItems.getItemByIndex index

               let Just gArmor = item^.giInfo
                   save = if dflags .&. Constants.damageEnergy /= 0
                            then ceiling ((gArmor^.giaEnergyProtection) * (fromIntegral damage))
                            else ceiling ((gArmor^.giaNormalProtection) * (fromIntegral damage))
                   Just (GClientReference gClientIdx) = edict^.eClient

               Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

               let save' = if save >= (gClient^.gcPers.cpInventory) UV.! index
                             then (gClient^.gcPers.cpInventory) UV.! index
                             else save

               if save' == 0
                 then
                   return 0

                 else do
                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix index -= save'
                   spawnDamage sparks point normal save'

                   return save'

checkTeamDamage :: EdictReference -> EdictReference -> Quake Bool
checkTeamDamage _ _ =
    -- FIXME: make the next line real and uncomment this block
    -- if ((ability to damage a teammate == OFF) && (targ's team ==
    -- attacker's team))
    return False

killed :: EdictReference -> EdictReference -> EdictReference -> Int -> V3 Float -> Quake ()
killed targRef inflictorRef attackerRef damage point = do
    targ <- readEdictT targRef

    Com.dprintf ("Killing a " `B.append` (targ^.eClassName) `B.append` "\n")

    when ((targ^.eHealth) < -999) $
      modifyEdictT targRef (\v -> v & eHealth .~ (-999))

    modifyEdictT targRef (\v -> v & eEnemy .~ Just attackerRef)

    when ((targ^.eSvFlags) .&. Constants.svfMonster /= 0 && (targ^.eDeadFlag) /= Constants.deadDead) $
      when ((targ^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy == 0) $ do
        gameBaseGlobals.gbLevel.llKilledMonsters += 1
        coopValue <- liftM (^.cvValue) coopCVar

        attacker <- readEdictT attackerRef

        when (coopValue /= 0 && isJust (attacker^.eClient)) $ do
          let Just (GClientReference attackerClientIdx) = attacker^.eClient
          gameBaseGlobals.gbGame.glClients.ix attackerClientIdx.gcResp.crScore += 1
          -- medics won't heal monsters that they kill themselves
          when ((attacker^.eClassName) == "monster_medic") $
            modifyEdictT targRef (\v -> v & eOwner .~ Just attackerRef)

    if (targ^.eMoveType) `elem` [ Constants.moveTypePush, Constants.moveTypeStop, Constants.moveTypeNone ] -- doors, triggers, etc
      then
        die (fromJust $ targ^.eDie) targRef inflictorRef attackerRef damage point
      else do
        when ((targ^.eSvFlags) .&. Constants.svfMonster /= 0 && (targ^.eDeadFlag) /= Constants.deadDead) $ do
          modifyEdictT targRef (\v -> v & eTouch .~ Nothing)
          Monster.monsterDeathUse targRef

        die (fromJust $ targ^.eDie) targRef inflictorRef attackerRef damage point

reactToDamage :: EdictReference -> EdictReference -> Quake ()
reactToDamage targRef attackerRef = do
    skip <- shouldSkip

    unless skip $ do
      -- we no know that we are not both good guys
      targ <- readEdictT targRef
      attacker <- readEdictT attackerRef

      -- if attacker is a client, get mad at them because he's good and we're not
      case attacker^.eClient of
        Just _ -> do
          modifyEdictT targRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiSoundTarget)))

          -- this can only happen in coop (both new and old enemies are clients)
          -- only switch if can't see the current enemy
          done <- case targ^.eEnemy of
                    Nothing -> return False
                    Just enemyRef -> do
                      enemy <- readEdictT enemyRef
                      case enemy^.eClient of
                        Nothing -> return False
                        Just _ -> do
                          visible <- GameUtil.visible targRef enemyRef
                          if visible
                            then do
                              modifyEdictT targRef (\v -> v & eOldEnemy .~ Just attackerRef)
                              return True
                            else do
                              modifyEdictT targRef (\v -> v & eOldEnemy .~ Just enemyRef)
                              return False
          
          unless done $ do
            modifyEdictT targRef (\v -> v & eEnemy .~ Just attackerRef)
            
            when ((targ^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked == 0) $
              GameUtil.foundTarget targRef

        Nothing -> do
               -- it's the same base (walk/swim/fly) type and a different classname and
               -- it's not a tank
               -- (they spray too much), get mad at them
          if | (targ^.eFlags) .&. (Constants.flFly .|. Constants.flSwim) == (attacker^.eFlags) .&. (Constants.flFly .|. Constants.flSwim)
               && (targ^.eClassName) /= (attacker^.eClassName)
               && not ((attacker^.eClassName) `elem` ["monster_tank", "monster_supertank", "monster_makron", "monster_jorg"]) ->
                 updateTargetEnemy targ (Just attackerRef)
                 
               -- if they *meant* to shoot us, then shoot back
             | (attacker^.eEnemy) == Just targRef ->
                 updateTargetEnemy targ (Just attackerRef)
                 
               -- otherwise get mad at whoever they are mad at (help our buddy) unless
               -- it is us!
             | isJust (attacker^.eEnemy) && (attacker^.eEnemy) /= Just targRef ->
                 updateTargetEnemy targ (attacker^.eEnemy)
                 
             | otherwise ->
                 return ()
  
  where shouldSkip :: Quake Bool
        shouldSkip = do
          targ <- readEdictT targRef
          attacker <- readEdictT attackerRef

          if | isJust (attacker^.eClient) && (attacker^.eSvFlags) .&. Constants.svfMonster /= 0 ->
                 return True
             | attackerRef == targRef || Just attackerRef == (targ^.eEnemy) ->
                 return True
               -- if we are a good guy monster and our attacker is a player
               -- or another good guy, do not get mad at them
             | (targ^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy /= 0 && (isJust (attacker^.eClient) || (attacker^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy /= 0) ->
                 return True
             | otherwise ->
                 return False
                 
        updateTargetEnemy :: EdictT -> Maybe EdictReference -> Quake ()
        updateTargetEnemy targ targAttackerRef = do
          case targ^.eEnemy of
            Nothing -> return ()
            Just enemyRef -> do
              enemy <- readEdictT enemyRef
              case enemy^.eClient of
                Nothing -> return ()
                Just _ -> modifyEdictT targRef (\v -> v & eOldEnemy .~ (targ^.eEnemy))
          
          modifyEdictT targRef (\v -> v & eEnemy .~ targAttackerRef)
          
          when ((targ^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked == 0) $
            GameUtil.foundTarget targRef
