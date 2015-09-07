{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameCombat where

import Control.Lens (use, preuse, (^.), ix, (.=), (+=), (%=), zoom, (-=))
import Control.Monad (when, unless, liftM)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (V3, norm, normalize, dot)
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Util.Math3D as Math3D

radiusDamage :: EdictReference -> EdictReference -> Float -> Maybe EdictReference -> Float -> Int -> Quake ()
radiusDamage inflictorRef@(EdictReference inflictorIdx) attackerRef dmg ignoreRef radius mod' = do
    radiusDamage' Nothing

  where radiusDamage' :: Maybe EdictReference -> Quake ()
        radiusDamage' edictRef = do
          Just inflictor <- preuse $ gameBaseGlobals.gbGEdicts.ix inflictorIdx
          edictit <- GameBase.findRadius edictRef (inflictor^.eEntityState.esOrigin) radius

          case edictit of
            Nothing -> return ()

            Just entRef@(EdictReference entIdx) -> do
              Just ent <- preuse $ gameBaseGlobals.gbGEdicts.ix entIdx

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
                         v3o <- use $ globals.vec3Origin
                         damage entRef inflictorRef attackerRef dir (inflictor^.eEntityState.esOrigin) v3o (truncate points') (truncate points') Constants.damageRadius mod'

                     radiusDamage' edictit

damage :: EdictReference -> EdictReference -> EdictReference -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
damage targRef@(EdictReference targIdx) inflictorRef@(EdictReference inflictorIdx) attackerRef@(EdictReference attackerIdx) dir point normal damage knockback dflags mod' = do
    Just targ <- preuse $ gameBaseGlobals.gbGEdicts.ix targIdx

    unless ((targ^.eTakeDamage) == 0) $ do
      -- friendly fire avoidance
      -- if enabled you can't hurt teammates (but you can hurt yourself)
      -- knockback still occurs
      deathmatchValue <- liftM (^.cvValue) deathmatchCVar
      dmFlags <- liftM (truncate . (^.cvValue)) dmFlagsCVar
      skillValue <- liftM (^.cvValue) skillCVar
      coopValue <- liftM (^.cvValue) coopCVar

      Just targ <- preuse $ gameBaseGlobals.gbGEdicts.ix targIdx
      Just attacker <- preuse $ gameBaseGlobals.gbGEdicts.ix attackerIdx

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

        gameBaseGlobals.gbGEdicts.ix targIdx.eVelocity += kvel

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
                                  gameBaseGlobals.gbGEdicts.ix targIdx.ePainDebounceTime .= levelTime + 2

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

                    gameBaseGlobals.gbGEdicts.ix targIdx.eHealth .= (targ^.eHealth) - take'''

                    if (targ^.eHealth) - take''' <= 0
                      then do
                        when ((targ^.eSvFlags) .&. Constants.svfMonster /= 0 || isJust client) $
                          gameBaseGlobals.gbGEdicts.ix targIdx.eFlags %= (.|. Constants.flNoKnockback)

                        killed targRef inflictorRef attackerRef take''' point
                        return True
                      else
                        return False
                  else
                    return False

        unless done $ do
          if | (targ^.eSvFlags) .&. Constants.svfMonster /= 0 -> do
                 reactToDamage targRef attackerRef
                 Just targ' <- preuse $ gameBaseGlobals.gbGEdicts.ix targIdx

                 when ((targ'^.eMonsterInfo.miAIFlags) .&. Constants.aiDucked == 0 && take''' /= 0) $ do
                   pain (fromJust $ targ'^.ePain) targRef attackerRef (fromIntegral knockback) take'''
                   -- nightmare mode monsters don't go into pain frames often
                   when (skillValue == 3) $ do
                     levelTime <- use $ gameBaseGlobals.gbLevel.llTime
                     gameBaseGlobals.gbGEdicts.ix targIdx.ePainDebounceTime .= levelTime + 5

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

canDamage :: EdictReference -> EdictReference -> Quake Bool
canDamage _ _ = do
    io (putStrLn "GameCombat.canDamage") >> undefined -- TODO

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
checkPowerArmor edictRef@(EdictReference edictIdx) point normal damage dflags = do
    if | damage == 0 -> return 0
       | dflags .&. Constants.damageNoArmor /= 0 -> return 0
       | otherwise -> do
           Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

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
                 Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

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
                         gameBaseGlobals.gbGEdicts.ix edictIdx.ePowerArmorTime .= levelTime + 0.2

                         let powerUsed = save' `div` damagePerCell

                         case edict^.eClient of
                           Nothing ->
                             gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miPowerArmorPower -= powerUsed

                           Just (GClientReference gClientIdx) -> do
                             Just (GItemReference itemIdx) <- GameItems.findItem "Cells"
                             Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx
                             gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (item^.giIndex) -= powerUsed

                         return save'

checkArmor :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake Int
checkArmor _ _ _ _ _ _ = do
    io (putStrLn "GameCombat.checkArmor") >> undefined -- TODO

checkTeamDamage :: EdictReference -> EdictReference -> Quake Bool
checkTeamDamage _ _ =
    -- FIXME: make the next line real and uncomment this block
    -- if ((ability to damage a teammate == OFF) && (targ's team ==
    -- attacker's team))
    return False

killed :: EdictReference -> EdictReference -> EdictReference -> Int -> V3 Float -> Quake ()
killed _ _ _ _ _ = do
    io (putStrLn "GameCombat.killed") >> undefined -- TODO

reactToDamage :: EdictReference -> EdictReference -> Quake ()
reactToDamage _ _ = do
    io (putStrLn "GameCombat.reactToDamage") >> undefined -- TODO
