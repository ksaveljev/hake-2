{-# LANGUAGE MultiWayIf #-}
module Game.GameCombat where

import Control.Lens (use, preuse, (^.), ix)
import Control.Monad (when)
import Linear (V3, norm)

import Quake
import QuakeState
import qualified Constants
import {-# SOURCE #-} qualified Game.GameBase as GameBase

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
damage _ _ _ _ _ _ _ _ _ _ = do
    io (putStrLn "GameCombat.damage") >> undefined -- TODO

canDamage :: EdictReference -> EdictReference -> Quake Bool
canDamage _ _ = do
    io (putStrLn "GameCombat.canDamage") >> undefined -- TODO
