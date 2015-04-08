{-# LANGUAGE OverloadedStrings #-}
module Server.SV where

import Control.Lens (use, preuse, ix, (^.), (.=), (+=))
import Control.Monad (unless, when, void)
import Data.Bits ((.&.))
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3, _x, _y, _z)

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.Com as Com
import qualified Server.SVGame as SVGame

{-
- 
- Bmodel objects don't interact with each other, but push all box objects.
-}
physicsPusher :: EdictReference -> Quake ()
physicsPusher er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    -- if not a team captain, so movement will be handled elsewhere
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      -- make sure all team slaves can move before commiting
      -- any moves or calling any think functions
      -- if the move is blocked, all moved objects will be backed out
      -- retry:
      gameBaseGlobals.gbPushedP .= 0
      finalEdict <- pushTeamChain (Just er)

      pushedP <- use $ gameBaseGlobals.gbPushedP

      when (pushedP > Constants.maxEdicts) $
        SVGame.pfError2 Constants.errFatal "pushed_p > &pushed[MAX_EDICTS], memory corrupted"

      if isJust finalEdict
        then do
          -- the move failed, bump all nextthink times and back out moves
          backOutTeamChain (Just er)
          let Just (EdictReference blockedIdx) = finalEdict
          Just blockedEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix blockedIdx

          when (isJust (blockedEdict^.eEdictAction.eaBlocked)) $ do
            obstacle <- use $ gameBaseGlobals.gbObstacle
            blocked (fromJust $ blockedEdict^.eEdictAction.eaBlocked) (fromJust finalEdict) (fromJust obstacle)

        else
          -- the move succeeded, so call all think functions
          thinkTeamChain (Just er)

  where pushTeamChain :: Maybe EdictReference -> Quake (Maybe EdictReference)
        pushTeamChain Nothing = return Nothing
        pushTeamChain (Just chain@(EdictReference chainIdx)) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix chainIdx
          
          let velocity = edict^.eEdictPhysics.eVelocity
              avelocity = edict^.eEdictPhysics.eAVelocity

          if (velocity^._x) /= 0 || (velocity^._y) /= 0 || (velocity^._z) /= 0 || (avelocity^._x) /= 0 || (avelocity^._y) /= 0 || (avelocity^._z) /= 0
            then do
              -- object is moving
              let move = fmap (* Constants.frameTime) velocity
                  amove = fmap (* Constants.frameTime) avelocity

              pushed <- push chain move amove
              if pushed
                then pushTeamChain (edict^.eEdictOther.eoTeamChain)
                else return (Just chain)

            else pushTeamChain (edict^.eEdictOther.eoTeamChain)

        thinkTeamChain :: Maybe EdictReference -> Quake ()
        thinkTeamChain Nothing = return ()
        thinkTeamChain (Just chain@(EdictReference chainIdx)) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix chainIdx

          void $ runThink chain

          thinkTeamChain (edict^.eEdictOther.eoTeamChain)

        backOutTeamChain :: Maybe EdictReference -> Quake ()
        backOutTeamChain Nothing = return ()
        backOutTeamChain (Just (EdictReference chainIdx)) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix chainIdx

          when ((edict^.eEdictAction.eaNextThink) > 0) $
            gameBaseGlobals.gbGEdicts.ix chainIdx.eEdictAction.eaNextThink += Constants.frameTime

          backOutTeamChain (edict^.eEdictOther.eoTeamChain)

-- Non moving objects can only think
physicsNone :: EdictReference -> Quake ()
physicsNone = void . runThink -- regular thinking

physicsNoClip :: EdictReference -> Quake ()
physicsNoClip _ = io (putStrLn "SV.physicsNoClip") >> undefined -- TODO

physicsStep :: EdictReference -> Quake ()
physicsStep _ = io (putStrLn "SV.physicsStep") >> undefined -- TODO

physicsToss :: EdictReference -> Quake ()
physicsToss _ = io (putStrLn "SV.physicsToss") >> undefined -- TODO

push :: EdictReference -> V3 Float -> V3 Float -> Quake Bool
push _ _ _ = io (putStrLn "SV.push") >> undefined -- TODO

{-
- Runs thinking code for this frame if necessary.
-}
runThink :: EdictReference -> Quake Bool
runThink er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    let thinktime = edict^.eEdictAction.eaNextThink
    time <- use $ gameBaseGlobals.gbLevel.llTime

    if thinktime <= 0 || thinktime > time + 0.001
      then return True
      else do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaNextThink .= 0

        when (isNothing (edict^.eEdictAction.eaThink)) $
          Com.comError Constants.errFatal "NULL ent.think"

        void $ think (fromJust $ edict^.eEdictAction.eaThink) er

        return False
