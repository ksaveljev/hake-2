{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerTrail where

import Control.Lens ((^.), (.=), ix, (&), (.~))
import Control.Monad (liftM, when)
import Linear (V3)
import qualified Data.Vector as V

import Game.CVarT
import Game.EdictT
import Types
import QuakeRef
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

  where initTrail :: Int -> Quake (Ref EdictT)
        initTrail _ = do
          edictRef <- GameUtil.spawn
          modifyRef edictRef (\v -> v & eClassName .~ "player_trail")
          return edictRef

lastSpot :: Quake (Ref EdictT)
lastSpot = do
    io (putStrLn "PlayerTrail.lastSpot") >> undefined -- TODO

add :: V3 Float -> Quake ()
add _ = do
    io (putStrLn "PlayerTrail.add") >> undefined -- TODO

pickFirst :: Ref EdictT -> Quake (Maybe (Ref EdictT))
pickFirst _ = do
    io (putStrLn "PlayerTrail.pickFirst") >> undefined -- TODO

pickNext :: Ref EdictT -> Quake (Maybe (Ref EdictT))
pickNext _ = do
    io (putStrLn "PlayerTrail.pickNext") >> undefined -- TODO
