{-# LANGUAGE OverloadedStrings #-}
module Game.GameTrigger where

import Control.Lens ((.=), ix, preuse, use, (^.), (%=), zoom)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), complement)
import qualified Data.ByteString as B

import Quake
import QuakeState
import Game.Adapters
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

spTriggerMultiple :: EdictReference -> Quake ()
spTriggerMultiple _ = io (putStrLn "GameTrigger.spTriggerMultiple") >> undefined -- TODO

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
      let v = (edict^.eEdictMinMax.eMins) + fmap (* 0.5) (edict^.eEdictMinMax.eSize)
      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eSpawnFlags %= (.&. (complement 1))
        eSpawnFlags %= (.|. 4)

      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
      dprintf $ "fixed TRIGGERED flag on " `B.append` (edict^.eClassName) `B.append` (Lib.vtos v) `B.append` "\n"

    gameBaseGlobals.gbGEdicts.ix edictIdx.eWait .= -1
    spTriggerMultiple er

    io (putStrLn "GameTrigger.spTriggerOnce") >> undefined -- TODO

spTriggerRelay :: EdictReference -> Quake ()
spTriggerRelay (EdictReference edictIdx) =
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaUse .= Just triggerRelayUse

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

triggerRelayUse :: EntUse
triggerRelayUse =
  GenericEntUse "trigger_relay_use" $ \_ _ _ -> do
    io (putStrLn "GameTrigger.triggerRelayUse") >> undefined -- TODO
