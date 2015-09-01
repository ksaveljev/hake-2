module Game.GameWeapon where

import Control.Lens (use, preuse, ix, (.=), (^.))
import Control.Monad (when)
import Data.Maybe (isJust)
import Linear (V3, normalize)

import Quake
import QuakeState
import qualified Constants
import qualified Game.GameUtil as GameUtil

fireHit :: EdictReference -> V3 Float -> Int -> Int -> Quake Bool
fireHit _ _ _ _ = do
    io (putStrLn "GameWeapon.fireHit") >> undefined -- TODO

{-
- ================= 
- fire_blaster
- 
- Fires a single blaster bolt. Used by the blaster and hyper blaster.
- =================
-}
fireBlaster :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()
fireBlaster selfRef@(EdictReference selfIdx) start direction damage speed effect hyper = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let dir = normalize dir

    boltRef@(EdictReference boltIdx) <- GameUtil.spawn
    Just bolt <- preuse $ gameBaseGlobals.gbGEdicts.ix boltIdx

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let linkEntity = gameImport^.giLinkEntity
        trace = gameImport^.giTrace
        modelIndex = gameImport^.giModelIndex
        soundIndex = gameImport^.giSoundIndex

    undefined -- TODO

    -- yes, I know it looks weird that projectiles are deadmonsters
    -- what this means is that when prediction is used against the object
    -- (blaster/hyperblaster shots), the player won't be solid clipped
    -- against the object. Right now trying to run into a firing hyperblaster
    -- is very jerky since you are predicted 'against' the shots.
    gameBaseGlobals.gbGEdicts.ix boltIdx .= bolt { _eSvFlags = Constants.svfDeadMonster
                                                 }

    linkEntity boltRef

    when (isJust (self^.eClient)) $
      checkDodge selfRef start dir speed

    traceT <- trace (self^.eEntityState.esOrigin) Nothing Nothing start (Just boltRef) Constants.maskShot

    when ((traceT^.tFraction) < 1.0) $ do
      io (putStrLn "GameWeapon.fireBlaster") >> undefined -- TODO

fireShotgun :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
fireShotgun _ _ _ _ _ _ _ _ _ = do
    io (putStrLn "GameWeapon.fireShotgun") >> undefined -- TODO

fireRail :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Quake ()
fireRail _ _ _ _ _ = do
    io (putStrLn "GameWeapon.fireRail") >> undefined -- TODO

{-
- ================= 
- check_dodge
- 
- This is a support routine used when a client is firing a non-instant
- attack weapon. It checks to see if a monster's dodge function should be
- called. 
- =================
-}
checkDodge :: EdictReference -> V3 Float -> V3 Float -> Int -> Quake ()
checkDodge _ _ _ _ = do
    io (putStrLn "GameWeapon.checkDodge") >> undefined -- TODO
