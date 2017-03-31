{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameTrigger ( spTriggerAlways
                        , spTriggerOnce
                        , spTriggerMultiple
                        , spTriggerRelay
                        , spTriggerPush
                        , spTriggerHurt
                        , spTriggerKey
                        , spTriggerCounter
                        , spTriggerGravity
                        , spTriggerMonsterJump
                        ) where

import Control.Lens ((.=), ix, preuse, use, (^.), (%=), zoom, (&), (.~), (%~), (-~), (-=))
import Control.Monad (when, unless, void, liftM)
import Data.Bits ((.&.), (.|.), complement, shiftL)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (dot, _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV

import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameBase as GameBase
import {-# SOURCE #-} Game.GameCombat as GameCombat
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

pushOnce :: Int
pushOnce = 1

spTriggerMultiple :: Ref EdictT -> Quake ()
spTriggerMultiple edictRef = do
    edict <- readRef edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity
        soundIndex = gameImport^.giSoundIndex

    let noiseIndex = if | (edict^.eSounds) == 1 -> Just "misc/secret.wav"
                        | (edict^.eSounds) == 2 -> Just "misc/talk.wav"
                        | (edict^.eSounds) == 3 -> Just "misc/trigger1.wav"
                        | otherwise -> Nothing

    when (isJust noiseIndex) $ do
      noiseIdx <- soundIndex noiseIndex
      modifyRef edictRef (\v -> v & eNoiseIndex .~ noiseIdx)

    when ((edict^.eWait) == 0) $
      modifyRef edictRef (\v -> v & eWait .~ 0.2)

    modifyRef edictRef (\v -> v & eTouch .~ Just touchMulti
                                   & eMoveType .~ Constants.moveTypeNone
                                   & eSvFlags %~ (.|. Constants.svfNoClient))
    
    if (edict^.eSpawnFlags) .&. 4 /= 0
      then
        modifyRef edictRef (\v -> v & eSolid .~ Constants.solidNot
                                       & eUse .~ Just triggerEnable)
      else
        modifyRef edictRef (\v -> v & eSolid .~ Constants.solidTrigger
                                       & eUse .~ Just useMulti)

    origin <- use $ globals.gVec3Origin

    unless ((edict^.eEntityState.esAngles) == origin) $
      GameBase.setMoveDir edictRef

    setModel edictRef (edict^.eiModel)
    linkEntity edictRef

{-
- QUAKED trigger_once (.5 .5 .5) ? x x TRIGGERED Triggers once, then
- removes itself. You must set the key "target" to the name of another
- object in the level that has a matching "targetname".
- 
- If TRIGGERED, this trigger must be triggered before it is live.
- 
- sounds 1) secret 2) beep beep 3) large switch 4)
- 
- "message" string to be displayed when triggered
-}
spTriggerOnce :: Ref EdictT -> Quake ()
spTriggerOnce edictRef = do
    edict <- readRef edictRef

    -- make old maps work because I messed up on flag assignments here
    -- triggered was on bit 1 when it should have been on bit 4
    when ((edict^.eSpawnFlags) .&. 1 /= 0) $ do
      let v = (edict^.eMins) + fmap (* 0.5) (edict^.eSize)

      modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. (complement 1))
                                     & eSpawnFlags %~ (.|. 4))

      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
      dprintf $ "fixed TRIGGERED flag on " `B.append` (edict^.eClassName) `B.append` (Lib.vtos v) `B.append` "\n"

    modifyRef edictRef (\v -> v & eWait .~ (-1))
    spTriggerMultiple edictRef

spTriggerRelay :: Ref EdictT -> Quake ()
spTriggerRelay edictRef =
    modifyRef edictRef (\v -> v & eUse .~ Just triggerRelayUse)

spTriggerKey :: Ref EdictT -> Quake ()
spTriggerKey selfRef = do
    self <- readRef selfRef
    mItem <- use $ gameBaseGlobals.gbSpawnTemp.stItem
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let dprintf = gameImport^.giDprintf
        soundIndex = gameImport^.giSoundIndex
    
    case mItem of
      Nothing ->
        dprintf ("no key item for trigger_key at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
      
      Just item -> do
        foundItemRef <- GameItems.findItemByClassname item
        modifyRef selfRef (\v -> v & eItem .~ foundItemRef)
        
        case foundItemRef of
          Nothing ->
            dprintf ("item " `B.append` item `B.append` " not found for trigger_key at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
          
          Just _ ->
            case self^.eTarget of
              Nothing ->
                dprintf ((self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` " has no target\n")
              
              Just _ -> do
                void $ soundIndex (Just "misc/keytry.wav")
                void $ soundIndex (Just "misc/keyuse.wav")
                modifyRef selfRef (\v -> v & eUse .~ Just triggerKeyUse)

spTriggerCounter :: Ref EdictT -> Quake ()
spTriggerCounter selfRef = do
    modifyRef selfRef (\v -> v & eWait -~ 1
                                  & eCount %~ (\a -> if a == 0 then 2 else a)
                                  & eUse .~ Just triggerCounterUse
                                  )

spTriggerAlways :: Ref EdictT -> Quake ()
spTriggerAlways edictRef = do
    edict <- readRef edictRef

    when ((edict^.eDelay) < 0.2) $
      modifyRef edictRef (\v -> v & eDelay .~ 0.2)

    GameUtil.useTargets edictRef (Just edictRef)

{-
- QUAKED trigger_push (.5 .5 .5) ? PUSH_ONCE Pushes the player "speed"
- defaults to 1000
-}
spTriggerPush :: Ref EdictT -> Quake ()
spTriggerPush selfRef = do
    initTrigger selfRef
    
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
    
    windSound <- soundIndex (Just "misc/windfly.wav")
    gameBaseGlobals.gbWindSound .= windSound
    
    modifyRef selfRef (\v -> v & eTouch .~ Just triggerPushTouch
                                  & eSpeed %~ (\a -> if a == 0 then 1000 else a)
                                  )
                                  
    linkEntity selfRef

spTriggerHurt :: Ref EdictT -> Quake ()
spTriggerHurt selfRef = do
    initTrigger selfRef
    
    self <- readRef selfRef
    
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
        
    soundIdx <- soundIndex (Just "world/electro.wav")
    
    let solid = if (self^.eSpawnFlags) .&. 1 /= 0
                  then Constants.solidNot
                  else Constants.solidTrigger
        selfUse = if (self^.eSpawnFlags) .&. 2 /= 0
                    then Just hurtUse
                    else self^.eUse
    
    modifyRef selfRef (\v -> v & eNoiseIndex .~ soundIdx
                                  & eTouch .~ Just hurtTouch
                                  & eDmg %~ (\a -> if a == 0 then 5 else a)
                                  & eSolid .~ solid
                                  & eUse .~ selfUse
                                  )
    
    linkEntity selfRef

spTriggerGravity :: Ref EdictT -> Quake ()
spTriggerGravity selfRef = do
    mGravity <- use $ gameBaseGlobals.gbSpawnTemp.stGravity
    
    case mGravity of
      Nothing -> do
        self <- readRef selfRef
        dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
        dprintf ("trigger_gravity without gravity set at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
        GameUtil.freeEdict selfRef
      
      Just gravity -> do
        initTrigger selfRef
        modifyRef selfRef (\v -> v & eGravity .~ fromIntegral (Lib.atoi gravity)
                                      & eTouch .~ Just triggerGravityTouch
                                      )

spTriggerMonsterJump :: Ref EdictT -> Quake ()
spTriggerMonsterJump selfRef = do
    modifyRef selfRef (\v -> v & eSpeed %~ (\a -> if a == 0 then 200 else a)
                                  & eEntityState.esAngles._y %~ (\a -> if a == 0 then 360 else a) -- IMPROVE: use Constants.yaw instead of using _y directly
                                  )
    
    gameBaseGlobals.gbSpawnTemp.stHeight %= (\v -> if v == 0 then 200 else v)
    height <- use $ gameBaseGlobals.gbSpawnTemp.stHeight
    
    initTrigger selfRef
    
    modifyRef selfRef (\v -> v & eTouch .~ Just triggerMonsterJumpTouch
                                  & eMoveDir._z .~ fromIntegral height
                                  )

{-
- QUAKED trigger_relay (.5 .5 .5) (-8 -8 -8) (8 8 8) This fixed size
- trigger cannot be touched, it can only be fired by other events.
-}
triggerRelayUse :: EntUse
triggerRelayUse =
  GenericEntUse "trigger_relay_use" $ \selfRef _ activatorRef -> do
    GameUtil.useTargets selfRef activatorRef

touchMulti :: EntTouch
touchMulti =
  GenericEntTouch "Touch_Multi" $ \edictRef otherRef _ _ -> do
    done <- shouldReturn edictRef otherRef

    unless done $ do
      edict <- readRef edictRef
      other <- readRef otherRef
      vec3origin <- use $ globals.gVec3Origin

      done' <- if (edict^.eMoveDir) /= vec3origin
                 then do
                   let (Just forward, _, _) = Math3D.angleVectors (other^.eEntityState.esAngles) True False False
                   if dot forward (edict^.eMoveDir) < 0
                     then return True
                     else return False
                 else
                   return False

      unless done' $ do
        modifyRef edictRef (\v -> v & eActivator .~ Just otherRef)
        multiTrigger edictRef

  where shouldReturn :: Ref EdictT -> Ref EdictT -> Quake Bool
        shouldReturn edictRef otherRef = do
          other <- readRef otherRef
          spawnFlags <- readRef edictRef >>= \e -> return (e^.eSpawnFlags)

          if | isJust (other^.eClient) -> return $ if spawnFlags .&. 2 /= 0 then True else False
             | (other^.eSvFlags) .&. Constants.svfMonster /= 0 -> return $ if spawnFlags .&. 1 == 0 then True else False
             | otherwise -> return True

{-
- QUAKED trigger_multiple (.5 .5 .5) ? MONSTER NOT_PLAYER TRIGGERED
- Variable sized repeatable trigger. Must be targeted at one or more
- entities. If "delay" is set, the trigger waits some time after activating
- before firing. "wait" : Seconds between triggerings. (.2 default) sounds
- 1) secret 2) beep beep 3) large switch 4) set "message" to text string
-}
triggerEnable :: EntUse
triggerEnable =
  GenericEntUse "trigger_enable" $ \selfRef _ _ -> do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidTrigger
                                  & eUse .~ Just useMulti
                                  )
    
    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

useMulti :: EntUse
useMulti =
  GenericEntUse "Use_Multi" $ \edictRef _ activatorRef -> do
    modifyRef edictRef (\v -> v & eActivator .~ activatorRef)
    multiTrigger edictRef

-- the trigger was just activated
-- ent.activator should be set to the activator so it can be held through a
-- delay so wait for the delay time before firing
multiTrigger :: Ref EdictT -> Quake ()
multiTrigger edictRef = do
    edict <- readRef edictRef

    when ((edict^.eNextThink) == 0) $ do
      GameUtil.useTargets edictRef (edict^.eActivator)

      edict' <- readRef edictRef
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      
      if (edict'^.eWait) > 0
        then
          modifyRef edictRef (\v -> v & eThink .~ Just multiWait
                                         & eNextThink .~ levelTime + (edict'^.eWait))
        else
          -- we can't just remove (self) here, because this is a touch
          -- function called while looping through area links...
          modifyRef edictRef (\v -> v & eTouch .~ Nothing
                                         & eNextThink .~ levelTime + (Constants.frameTime)
                                         & eThink .~ Just GameUtil.freeEdictA)

initTrigger :: Ref EdictT -> Quake ()
initTrigger selfRef = do
    v3o <- use $ globals.gVec3Origin
    self <- readRef selfRef

    unless ((self^.eEntityState.esAngles) == v3o) $
      GameBase.setMoveDir selfRef

    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidTrigger
                                  & eMoveType .~ Constants.moveTypeNone
                                  & eSvFlags .~ Constants.svfNoClient)

    setModel <- use $ gameBaseGlobals.gbGameImport.giSetModel
    setModel selfRef (self^.eiModel)

multiWait :: EntThink
multiWait =
  GenericEntThink "multi_wait" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eNextThink .~ 0)
    return True

{-
- QUAKED trigger_key (.5 .5 .5) (-8 -8 -8) (8 8 8) A relay trigger that
- only fires it's targets if player has the proper key. Use "item" to
- specify the required key, for example "key_data_cd"
-}
triggerKeyUse :: EntUse
triggerKeyUse =
  GenericEntUse "trigger_key_use" $ \selfRef _ mActivatorRef -> do
    self <- readRef selfRef
    proceed <- shouldProceed self mActivatorRef
    
    when proceed $ do
      let Just activatorRef = mActivatorRef
      activator <- readRef activatorRef
      
      let Just (GClientReference gClientIdx) = activator^.eClient
      Just activatorClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    
      let Just (GItemReference gItemIdx) = self^.eItem
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      
      gameImport <- use $ gameBaseGlobals.gbGameImport
      let sound = gameImport^.giSound
          soundIndex = gameImport^.giSoundIndex
          centerPrintf = gameImport^.giCenterPrintf
      
      if (activatorClient^.gcPers.cpInventory) UV.! (gItem^.giIndex) == 0
        then do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          
          unless (levelTime < (self^.eTouchDebounceTime)) $ do
            modifyRef selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 5.0)
            centerPrintf activatorRef ("You need the " `B.append` fromJust (gItem^.giPickupName))
            soundIdx <- soundIndex (Just "misc/keytry.wav")
            sound mActivatorRef Constants.chanAuto soundIdx 1 Constants.attnNorm 0
          
        else do
          soundIdx <- soundIndex (Just "misc/keyuse.wav")
          sound mActivatorRef Constants.chanAuto soundIdx 1 Constants.attnNorm 0
          
          coopValue <- liftM (^.cvValue) coopCVar
          
          if coopValue /= 0
            then do
              edicts <- use $ gameBaseGlobals.gbGEdicts
              maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
              
              if (gItem^.giClassName) == "key_power_cube"
                then do
                  let cube = findCube activatorClient 0 8
                  updatePlayers edicts (gItem^.giIndex) cube 1 maxClients
                  
                else do
                  updatePlayers2 edicts (gItem^.giIndex) 1 maxClients
            
            else
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= 1
              
          GameUtil.useTargets selfRef mActivatorRef
          
          modifyRef selfRef (\v -> v & eUse .~ Nothing)
  
  where shouldProceed :: EdictT -> Maybe (Ref EdictT) -> Quake Bool
        shouldProceed self mActivatorRef =
          case self^.eItem of
            Nothing -> return False
            Just _ ->
              case mActivatorRef of
                Nothing -> return False
                Just activatorRef -> do
                  activator <- readRef activatorRef
                  case activator^.eClient of
                    Nothing -> return False
                    _ -> return True
                    
        findCube :: GClientT -> Int -> Int -> Int
        findCube gClient idx maxIdx
          | idx >= maxIdx = maxIdx
          | otherwise =
              if (gClient^.gcPers.cpPowerCubes) .&. (1 `shiftL` idx) /= 0
                then idx
                else findCube gClient (idx + 1) maxIdx
        
        updatePlayers :: MV.IOVector EdictT -> Int -> Int -> Int -> Int -> Quake ()
        updatePlayers edicts itemIndex cube idx maxIdx
          | idx > maxIdx = return ()
          | otherwise = do
              edict <- io $ MV.read edicts idx
              if | not (edict^.eInUse) -> updatePlayers edicts itemIndex cube (idx + 1) maxIdx
                 | isNothing (edict^.eClient) -> updatePlayers edicts itemIndex cube (idx + 1) maxIdx
                 | otherwise -> do
                     let Just (GClientReference gClientIdx) = edict^.eClient
                     Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
                     
                     when ((gClient^.gcPers.cpPowerCubes) .&. (1 `shiftL` cube) /= 0) $ do
                       zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers) $ do
                         cpInventory.ix itemIndex -= 1
                         cpPowerCubes %= (\v -> v .&. (complement (1 `shiftL` cube)))
                      
                     updatePlayers edicts itemIndex cube (idx + 1) maxIdx
                     
        updatePlayers2 :: MV.IOVector EdictT -> Int -> Int -> Int -> Quake ()
        updatePlayers2 edicts itemIndex idx maxIdx
          | idx > maxIdx = return ()
          | otherwise = do
              edict <- io $ MV.read edicts idx
              if | not (edict^.eInUse) -> updatePlayers2 edicts itemIndex (idx + 1) maxIdx
                 | isNothing (edict^.eClient) -> updatePlayers2 edicts itemIndex (idx + 1) maxIdx
                 | otherwise -> do
                     let Just (GClientReference gClientIdx) = edict^.eClient
                     gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix itemIndex .= 0
                     updatePlayers2 edicts itemIndex (idx + 1) maxIdx

{-
- QUAKED trigger_counter (.5 .5 .5) ? nomessage Acts as an intermediary for
- an action that takes multiple inputs.
- 
- If nomessage is not set, t will print "1 more.. " etc when triggered and
- "sequence complete" when finished.
- 
- After the counter has been triggered "count" times (default 2), it will
- fire all of it's targets and remove itself.
-}
triggerCounterUse :: EntUse
triggerCounterUse =
  GenericEntUse "trigger_counter_use" $ \selfRef _ mActivatorRef -> do
    self <- readRef selfRef
    let Just activatorRef = mActivatorRef
    
    unless ((self^.eCount) == 0) $ do
      let count = (self^.eCount) - 1
      modifyRef selfRef (\v -> v & eCount .~ count)
      
      gameImport <- use $ gameBaseGlobals.gbGameImport
      let sound = gameImport^.giSound
          soundIndex = gameImport^.giSoundIndex
          centerPrintf = gameImport^.giCenterPrintf
      
      if count > 0
        then
          when ((self^.eSpawnFlags) .&. 1 == 0) $ do
            centerPrintf activatorRef (BC.pack (show count) `B.append` " more to go...") -- IMPROVE
            soundIdx <- soundIndex (Just "misc/talk1.wav")
            sound mActivatorRef Constants.chanAuto soundIdx 1 Constants.attnNorm 0
            
        else do
          when ((self^.eSpawnFlags) .&. 1 == 0) $ do
            centerPrintf activatorRef "Sequence completed!"
            soundIdx <- soundIndex (Just "misc/talk1.wav")
            sound mActivatorRef Constants.chanAuto soundIdx 1 Constants.attnNorm 0
          
          modifyRef selfRef (\v -> v & eActivator .~ mActivatorRef)
          multiTrigger selfRef

{-
- QUAKED trigger_hurt (.5 .5 .5) ? START_OFF TOGGLE SILENT NO_PROTECTION
- SLOW Any entity that touches this will be hurt.
- 
- It does dmg points of damage each server frame
- 
- SILENT supresses playing the sound SLOW changes the damage rate to once
- per second NO_PROTECTION *nothing* stops the damage
- 
- "dmg" default 5 (whole numbers only)
-  
-}
hurtUse :: EntUse
hurtUse =
  GenericEntUse "hurt_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    
    let solid = if (self^.eSolid) == Constants.solidNot
                  then Constants.solidTrigger
                  else Constants.solidNot
                  
    modifyRef selfRef (\v -> v & eSolid .~ solid)
    
    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef
    
    self' <- readRef selfRef
    when ((self'^.eSpawnFlags) .&. 2 == 0) $
      modifyRef selfRef (\v -> v & eUse .~ Nothing)

hurtTouch :: EntTouch
hurtTouch =
  GenericEntTouch "hurt_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    let proceed = shouldProceed self other levelTime
    
    when proceed $ do
      let timeStamp = if (self^.eSpawnFlags) .&. 16 /= 0
                        then levelTime + 1
                        else levelTime + Constants.frameTime
      
      modifyRef selfRef (\v -> v & eTimeStamp .~ timeStamp)
      
      frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
      
      when ((self^.eSpawnFlags) .&. 4 == 0 && frameNum `mod` 10 == 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just otherRef) Constants.chanAuto (self^.eNoiseIndex) 1 Constants.attnNorm 0
      
      let dFlags = if (self^.eSpawnFlags) .&. 8 /= 0
                     then Constants.damageNoProtection
                     else 0
      
      v3o <- use $ globals.gVec3Origin
      
      GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) (self^.eDmg) dFlags Constants.modTriggerHurt
      
  where shouldProceed :: EdictT -> EdictT -> Float -> Bool
        shouldProceed self other levelTime =
          if (other^.eTakeDamage) == 0
            then False
            else if (self^.eTimeStamp) > levelTime then False else True

triggerPushTouch :: EntTouch
triggerPushTouch =
  GenericEntTouch "trigger_push_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef
    
    if | (other^.eClassName) == "grenade" ->
           modifyRef otherRef (\v -> v & eVelocity .~ fmap (* (10 * (self^.eSpeed))) (self^.eMoveDir))
           
       | (other^.eHealth) > 0 -> do
           modifyRef otherRef (\v -> v & eVelocity .~ fmap (* (10 * (self^.eSpeed))) (self^.eMoveDir))
           
           case other^.eClient of
             Nothing -> return ()
             Just (GClientReference gClientIdx) -> do
               -- don't take falling damage immediately from this
               gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcOldVelocity .= (other^.eVelocity)
               
               levelTime <- use $ gameBaseGlobals.gbLevel.llTime
               
               when ((other^.eFlySoundDebounceTime) < levelTime) $ do
                 modifyRef otherRef (\v -> v & eFlySoundDebounceTime .~ levelTime + 1.5)
                 windSound <- use $ gameBaseGlobals.gbWindSound
                 sound <- use $ gameBaseGlobals.gbGameImport.giSound
                 sound (Just otherRef) Constants.chanAuto windSound 1 Constants.attnNorm 0
           
       | otherwise ->
           return ()
           
    when ((self^.eSpawnFlags) .&. pushOnce /= 0) $
      GameUtil.freeEdict selfRef

triggerGravityTouch :: EntTouch
triggerGravityTouch =
  GenericEntTouch "trigger_gravity_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    modifyRef otherRef (\v -> v & eGravity .~ (self^.eGravity))

{-
- QUAKED trigger_monsterjump (.5 .5 .5) ? Walking monsters that touch this
- will jump in the direction of the trigger's angle "speed" default to 200,
- the speed thrown forward "height" default to 200, the speed thrown
- upwards
-}
triggerMonsterJumpTouch :: EntTouch
triggerMonsterJumpTouch =
  GenericEntTouch "trigger_monsterjump_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef
    
    let proceed = shouldProceed other
    
    when proceed $ do
      -- set XY even if not on ground, so the jump will clear lips
      modifyRef otherRef (\v -> v & eVelocity._x .~ (self^.eMoveDir._x) * (self^.eSpeed)
                                     & eVelocity._y .~ (self^.eMoveDir._y) * (self^.eSpeed)
                                     )
      
      case other^.eGroundEntity of
        Just _ -> return ()
        Nothing -> modifyRef otherRef (\v -> v & eVelocity._z .~ (self^.eMoveDir._z))
  
  where shouldProceed :: EdictT -> Bool
        shouldProceed other =
          if | (other^.eFlags) .&. (Constants.flFly .|. Constants.flSwim) /= 0 -> False
             | (other^.eSvFlags) .&. Constants.svfDeadMonster /= 0 -> False
             | (other^.eSvFlags) .&. Constants.svfMonster == 0 -> False
             | otherwise -> True
