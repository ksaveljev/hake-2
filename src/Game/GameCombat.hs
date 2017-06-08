module Game.GameCombat
    ( damage
    , radiusDamage
    ) where

import           Control.Lens (use, (^.), (&), (+~), (-~))
import           Control.Monad (when)
import           Linear (V3, norm, _x, _y)

import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import           Game.TraceT
import           QuakeRef
import           QuakeState
import           Types

import {-# SOURCE #-} qualified Game.GameBase as GameBase

damage :: Ref EdictT -> Ref EdictT -> Ref EdictT -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
damage = error "GameCombat.damage" -- TODO 

radiusDamage :: Ref EdictT -> Ref EdictT -> Float -> Maybe (Ref EdictT) -> Float -> Int -> Quake ()
radiusDamage inflictorRef attackerRef dmg ignoreRef radius mod' = doRadiusDamage Nothing
  where
    doRadiusDamage edictRef = do
        inflictor <- readRef inflictorRef
        foundRef <- GameBase.findRadius edictRef (inflictor^.eEntityState.esOrigin) radius
        maybe (return ()) (\entRef -> readRef entRef >>= proceedRadiusDamage inflictor entRef) foundRef
    proceedRadiusDamage inflictor entRef ent
        | Just entRef == ignoreRef = doRadiusDamage (Just entRef)
        | (ent^.eTakeDamage) == 0 = doRadiusDamage (Just entRef)
        | otherwise = do
            let v = (inflictor^.eEntityState.esOrigin) - (ent^.eEntityState.esOrigin) - fmap (* 0.5) ((ent^.eMins) + (ent^.eMaxs))
                points = (dmg - 0.5 * (norm v)) * (if entRef == attackerRef then 0.5 else 1)
            when (points > 0) $ do
                doDamage <- canDamage entRef inflictorRef
                when doDamage $ do
                    let dir = (ent^.eEntityState.esOrigin) - (inflictor^.eEntityState.esOrigin)
                    v3o <- use (globals.gVec3Origin)
                    damage entRef inflictorRef attackerRef dir (inflictor^.eEntityState.esOrigin) v3o (truncate points) (truncate points) Constants.damageRadius mod'
            doRadiusDamage (Just entRef)

canDamage :: Ref EdictT -> Ref EdictT -> Quake Bool
canDamage targetRef inflictorRef = do
    target <- readRef targetRef
    inflictor <- readRef inflictorRef
    v3o <- use (globals.gVec3Origin)
    checkCanDamage targetRef target inflictorRef inflictor v3o

-- IMPROVE: checkForward, checkLeft, checkRight, checkBack are the same, need to unify
checkCanDamage :: Ref EdictT -> EdictT -> Ref EdictT -> EdictT -> V3 Float -> Quake Bool
checkCanDamage targetRef target inflictorRef inflictor v3o
    | (target^.eMoveType) == Constants.moveTypePush = do
        let dest = fmap (* 0.5) ((target^.eAbsMin) + (target^.eAbsMax))
        traceT <- runTrace dest
        return ((traceT^.tFraction) == 1.0 || (traceT^.tEnt) == Just targetRef)
    | otherwise = do
        traceT <- runTrace (target^.eEntityState.esOrigin)
        if (traceT^.tFraction) == 1.0 then return True else checkForward
  where
    runTrace dest = do
        trace <- use (gameBaseGlobals.gbGameImport.giTrace)
        trace (inflictor^.eEntityState.esOrigin) (Just v3o) (Just v3o) dest (Just inflictorRef) Constants.maskSolid
    checkForward = do
        let dest = (target^.eEntityState.esOrigin) & _x +~ 15 & _y +~ 15
        traceT <- runTrace dest
        if (traceT^.tFraction) == 1.0 then return True else checkLeft
    checkLeft = do
        let dest = (target^.eEntityState.esOrigin) & _x +~ 15 & _y -~ 15
        traceT <- runTrace dest
        if (traceT^.tFraction) == 1.0 then return True else checkRight
    checkRight = do
        let dest = (target^.eEntityState.esOrigin) & _x -~ 15 & _y +~ 15
        traceT <- runTrace dest
        if (traceT^.tFraction) == 1.0 then return True else checkBack
    checkBack = do
        let dest = (target^.eEntityState.esOrigin) & _x -~ 15 & _y -~ 15
        traceT <- runTrace dest
        return ((traceT^.tFraction) == 1.0)
