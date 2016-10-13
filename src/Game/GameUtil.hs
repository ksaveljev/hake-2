module Game.GameUtil
    ( freeEdict
    , freeEdictA
    , inFront
    , megaHealthThink
    , spawn
    , validateSelectedItem
    ) where

import           Control.Lens          (use, (^.), (+=), (&), (.~))
import           Control.Monad         (when)
import           Linear                (dot, normalize)

import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import           Game.GameLocalsT
import           Game.LevelLocalsT
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Math3D           as Math3D

freeEdict :: Ref EdictT -> Quake ()
freeEdict = error "GameUtil.freeEdict" -- TODO

freeEdictA :: EntThink
freeEdictA = error "GameUtil.freeEdictA" -- TODO

validateSelectedItem :: Ref EdictT -> Quake ()
validateSelectedItem = error "GameUtil.validateSelectedItem" -- TODO

megaHealthThink :: EntThink
megaHealthThink = error "GameUtil.megaHealthThink" -- TODO

spawn :: Quake (Ref EdictT)
spawn =
  do maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
     numEdicts <- use (gameBaseGlobals.gbNumEdicts)
     levelTime <- use (gameBaseGlobals.gbLevel.llTime)
     edictRef <- findFreeEdict levelTime (maxClients + 1) numEdicts
     maybe (notFoundRef numEdicts) foundRef edictRef
  where notFoundRef numEdicts =
          do maxEntities <- use (gameBaseGlobals.gbGame.glMaxEntities)
             when (numEdicts == maxEntities) $
               do err <- use (gameBaseGlobals.gbGameImport.giError)
                  err "ED_Alloc: no free edicts"
             initRef (Ref numEdicts)
        foundRef edictRef =
          do gameBaseGlobals.gbNumEdicts += 1
             initRef edictRef
        initRef edictRef@(Ref idx) =
          do writeRef edictRef (newEdictT idx)
             initEdict edictRef
             return edictRef

findFreeEdict :: Float -> Int -> Int -> Quake (Maybe (Ref EdictT))
findFreeEdict levelTime idx maxIdx
  | idx >= maxIdx = return Nothing
  | otherwise = checkFree =<< readRef (Ref idx)
  where checkFree edict
          | not (edict^.eInUse) && ((edict^.eFreeTime) < 2 || levelTime - (edict^.eFreeTime) > 0.5) = return (Just (Ref idx))
          | otherwise = findFreeEdict levelTime (idx + 1) maxIdx

initEdict :: Ref EdictT -> Quake ()
initEdict edictRef@(Ref idx) =
  modifyRef edictRef (\v -> v & eInUse .~ True
                              & eClassName .~ "noclass"
                              & eGravity .~ 1.0
                              & eEntityState .~ (newEntityStateT (Just edictRef) & esNumber .~ idx))

inFront :: EdictT -> EdictT -> Bool
inFront self other =
    let (forward, _, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True False False
        vec = normalize ((other^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin))
        dot' = vec `dot` forward
    in dot' > 0.3