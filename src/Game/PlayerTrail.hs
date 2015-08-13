{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerTrail where

import Control.Lens ((^.), (.=), ix)
import Control.Monad (liftM, when)
import Linear (V3)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Game.GameUtil as GameUtil

{-
- ==============================================================================
- 
- PLAYER TRAIL
- 
- ==============================================================================
- 
- This is a circular list containing the a list of points of where the
- player has been recently. It is used by monsters for pursuit.
- 
- .origin the spot .owner forward link .aiment backward link
-}

trailLength :: Int
trailLength = 8

init :: Quake ()
init = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    -- FIXME || coop
    when (deathmatchValue == 0) $ do
      trailEdicts <- mapM initTrail [0..trailLength-1]
      playerTrailGlobals.ptTrail .= V.fromList trailEdicts
      playerTrailGlobals.ptTrailHead .= 0
      playerTrailGlobals.ptTrailActive .= True

  where initTrail :: Int -> Quake EdictReference
        initTrail _ = do
          er@(EdictReference idx) <- GameUtil.spawn
          gameBaseGlobals.gbGEdicts.ix idx.eClassName .= "player_trail"
          return er

lastSpot :: Quake EdictReference
lastSpot = do
    io (putStrLn "PlayerTrail.lastSpot") >> undefined -- TODO

add :: V3 Float -> Quake ()
add _ = do
    io (putStrLn "PlayerTrail.add") >> undefined -- TODO

pickFirst :: EdictReference -> Quake (Maybe EdictReference)
pickFirst _ = do
    io (putStrLn "PlayerTrail.pickFirst") >> undefined -- TODO

pickNext :: EdictReference -> Quake (Maybe EdictReference)
pickNext _ = do
    io (putStrLn "PlayerTrail.pickNext") >> undefined -- TODO
