module Game.PlayerTrail
    ( add
    , initialize
    , lastSpot
    ) where

import           Control.Lens          ((.=), (^.), (&), (.~))
import           Control.Monad         (when, replicateM)
import qualified Data.Vector           as V
import           Linear                (V3(..))

import           Game.CVarT
import           Game.EdictT
import qualified Game.GameUtil         as GameUtil
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

trailLength :: Int
trailLength = 8

initialize :: Quake ()
initialize = do
    deathmatch <- deathmatchCVar
    -- FIXME || coop
    when ((deathmatch^.cvValue) == 0) $ do
        trailEdicts <- replicateM trailLength initTrail
        playerTrailGlobals.ptTrail .= V.fromList trailEdicts
        playerTrailGlobals.ptTrailHead .= 0
        playerTrailGlobals.ptTrailActive .= True

initTrail :: Quake (Ref EdictT)
initTrail = do
    edictRef <- GameUtil.spawn
    modifyRef edictRef (\v -> v & eClassName .~ "player_trail")
    return edictRef

add :: V3 Float -> Quake ()
add = error "PlayerTrail.add" -- TODO

lastSpot :: Quake (Ref EdictT)
lastSpot = error "PlayerTrail.lastSpot" -- TODO