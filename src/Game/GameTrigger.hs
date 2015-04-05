{-# LANGUAGE OverloadedStrings #-}
module Game.GameTrigger where

import Control.Lens ((.=), ix)

import Quake
import QuakeState
import Game.Adapters

spTriggerMultiple :: EdictReference -> Quake ()
spTriggerMultiple _ = io (putStrLn "GameTrigger.spTriggerMultiple") >> undefined -- TODO

spTriggerOnce :: EdictReference -> Quake ()
spTriggerOnce _ = io (putStrLn "GameTrigger.spTriggerOnce") >> undefined -- TODO

spTriggerRelay :: EdictReference -> Quake ()
spTriggerRelay (EdictReference edictIdx) =
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaUse .= Just triggerRelayUse

spTriggerKey :: EdictReference -> Quake ()
spTriggerKey _ = io (putStrLn "GameTrigger.spTriggerKey") >> undefined -- TODO

spTriggerCounter :: EdictReference -> Quake ()
spTriggerCounter _ = io (putStrLn "GameTrigger.spTriggerCounter") >> undefined -- TODO

spTriggerAlways :: EdictReference -> Quake ()
spTriggerAlways _ = io (putStrLn "GameTrigger.spTriggerAlways") >> undefined -- TODO

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
