{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameTrigger where

import Control.Lens ((.=), ix, preuse, use, (^.), (%=), zoom)
import Control.Monad (when, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isJust)
import Linear (dot)
import qualified Data.ByteString as B

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

spTriggerMultiple :: EdictReference -> Quake ()
spTriggerMultiple er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity
        soundIndex = gameImport^.giSoundIndex

    let noiseIndex = if | (edict^.eSounds) == 1 -> Just "misc/secret.wav"
                        | (edict^.eSounds) == 2 -> Just "misc/talk.wav"
                        | (edict^.eSounds) == 3 -> Just "misc/trigger1.wav"
                        | otherwise -> Nothing

    when (isJust noiseIndex) $ do
      si <- soundIndex noiseIndex
      gameBaseGlobals.gbGEdicts.ix edictIdx.eNoiseIndex .= si

    when ((edict^.eWait) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eWait .= 0.2

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eTouch .= Just touchMulti
      eMoveType .= Constants.moveTypeNone
      eSvFlags %= (.|. Constants.svfNoClient)
    
    if (edict^.eSpawnFlags) .&. 4 /= 0
      then
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eSolid .= Constants.solidNot
          eUse .= Just triggerEnable
      else
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eSolid .= Constants.solidTrigger
          eUse .= Just useMulti

    origin <- use $ globals.vec3Origin

    unless ((edict^.eEntityState.esAngles) == origin) $
      GameBase.setMoveDir (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esAngles) (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveDir)

    setModel er (edict^.eiModel)
    linkEntity er

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
spTriggerOnce :: EdictReference -> Quake ()
spTriggerOnce er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    -- make old maps work because I messed up on flag assignments here
    -- triggered was on bit 1 when it should have been on bit 4
    when ((edict^.eSpawnFlags) .&. 1 /= 0) $ do
      let v = (edict^.eMins) + fmap (* 0.5) (edict^.eSize)
      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eSpawnFlags %= (.&. (complement 1))
        eSpawnFlags %= (.|. 4)

      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
      dprintf $ "fixed TRIGGERED flag on " `B.append` (edict^.eClassName) `B.append` (Lib.vtos v) `B.append` "\n"

    gameBaseGlobals.gbGEdicts.ix edictIdx.eWait .= -1
    spTriggerMultiple er

spTriggerRelay :: EdictReference -> Quake ()
spTriggerRelay (EdictReference edictIdx) =
    gameBaseGlobals.gbGEdicts.ix edictIdx.eUse .= Just triggerRelayUse

spTriggerKey :: EdictReference -> Quake ()
spTriggerKey _ = io (putStrLn "GameTrigger.spTriggerKey") >> undefined -- TODO

spTriggerCounter :: EdictReference -> Quake ()
spTriggerCounter _ = io (putStrLn "GameTrigger.spTriggerCounter") >> undefined -- TODO

spTriggerAlways :: EdictReference -> Quake ()
spTriggerAlways er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    when ((edict^.eDelay) < 0.2) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eDelay .= 0.2

    GameUtil.useTargets er (Just er)

spTriggerPush :: EdictReference -> Quake ()
spTriggerPush _ = io (putStrLn "GameTrigger.spTriggerPush") >> undefined -- TODO

spTriggerHurt :: EdictReference -> Quake ()
spTriggerHurt _ = io (putStrLn "GameTrigger.spTriggerHurt") >> undefined -- TODO

spTriggerGravity :: EdictReference -> Quake ()
spTriggerGravity _ = io (putStrLn "GameTrigger.spTriggerGravity") >> undefined -- TODO

spTriggerMonsterJump :: EdictReference -> Quake ()
spTriggerMonsterJump _ = io (putStrLn "GameTrigger.spTriggerMonsterJump") >> undefined -- TODO

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
  GenericEntTouch "Touch_Multi" $ \edictRef@(EdictReference edictIdx) otherRef@(EdictReference otherIdx) _ _ -> do
    done <- shouldReturn edictRef otherRef

    unless done $ do
      Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
      Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx
      vec3origin <- use $ globals.vec3Origin

      done' <- if (edict^.eMoveDir) /= vec3origin
                 then do
                   let (Just forward, _, _) = Math3D.angleVectors (other^.eEntityState.esAngles) True False False
                   if dot forward (edict^.eMoveDir) < 0
                     then return True
                     else return False
                 else
                   return False

      unless done' $ do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eActivator .= Just otherRef
        multiTrigger edictRef

  where shouldReturn :: EdictReference -> EdictReference -> Quake Bool
        shouldReturn (EdictReference edictIdx) (EdictReference otherIdx) = do
          Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

          if | isJust (other^.eClient) -> return $ if spawnFlags .&. 2 /= 0 then True else False
             | (other^.eSvFlags) .&. Constants.svfMonster /= 0 -> return $ if spawnFlags .&. 1 == 0 then True else False
             | otherwise -> return True

triggerEnable :: EntUse
triggerEnable =
  GenericEntUse "trigger_enable" $ \_ _ _ -> do
    io (putStrLn "GameTrigger.triggerEnable") >> undefined -- TODO

useMulti :: EntUse
useMulti =
  GenericEntUse "Use_Multi" $ \_ _ _ -> do
    io (putStrLn "GameTrigger.useMulti") >> undefined -- TODO

-- the trigger was just activated
-- ent.activator should be set to the activator so it can be held through a
-- delay so wait for the delay time before firing
multiTrigger :: EdictReference -> Quake ()
multiTrigger edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    when ((edict^.eNextThink) == 0) $ do
      GameUtil.useTargets edictRef (edict^.eActivator)

      Just edict' <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      
      if (edict'^.eWait) > 0
        then
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eThink .= Just multiWait
            eNextThink .= levelTime + (edict'^.eWait)
        else
          -- we can't just remove (self) here, because this is a touch
          -- function
          -- called while looping through area links...
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eTouch .= Nothing
            eNextThink .= levelTime + (Constants.frameTime)
            eThink .= Just GameUtil.freeEdictA

initTrigger :: EdictReference -> Quake ()
initTrigger selfRef@(EdictReference selfIdx) = do
    v3o <- use $ globals.vec3Origin
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    unless ((self^.eEntityState.esAngles) == v3o) $
      GameBase.setMoveDir (gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esAngles) (gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveDir)

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eSolid .= Constants.solidTrigger
      eMoveType .= Constants.moveTypeNone
      eSvFlags .= Constants.svfNoClient

    setModel <- use $ gameBaseGlobals.gbGameImport.giSetModel
    setModel selfRef (self^.eiModel)

multiWait :: EntThink
multiWait =
  GenericEntThink "multi_wait" $ \(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eNextThink .= 0
    return True
