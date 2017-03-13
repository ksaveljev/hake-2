module Game.GameUtil
    ( freeEdict
    , freeEdictA
    , inFront
    , killBox
    , megaHealthThink
    , spawn
    , useTargets
    , validateSelectedItem
    ) where

import           Control.Lens          (use, (^.), (+=), (&), (.~))
import           Control.Monad         (when)
import           Linear                (dot, normalize)

import qualified Constants
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

freeEdict :: Ref' EdictT -> Quake ()
freeEdict edictRef = do
    edict <- readRef edictRef
    unlinkEntity <- use (gameBaseGlobals.gbGameImport.giUnlinkEntity)
    unlinkEntity edictRef
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    when ((edict^.eIndex) > maxClients + Constants.bodyQueueSize) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        writeRef edictRef ((newEdictT (edict^.eIndex)) & eClassName .~ "freed"
                                                       & eFreeTime .~ levelTime
                                                       & eInUse .~ False)

freeEdictA :: EntThink
freeEdictA = EntThink "G_FreeEdictA" $ \edictRef -> do
    freeEdict edictRef
    return False

validateSelectedItem :: Ref' EdictT -> Quake ()
validateSelectedItem = error "GameUtil.validateSelectedItem" -- TODO

megaHealthThink :: EntThink
megaHealthThink = error "GameUtil.megaHealthThink" -- TODO

spawn :: Quake (Ref' EdictT)
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
             initRef (Ref Constants.noParent numEdicts)
        foundRef edictRef =
          do gameBaseGlobals.gbNumEdicts += 1
             initRef edictRef
        initRef edictRef@(Ref _ idx) =
          do writeRef edictRef (newEdictT idx)
             initEdict edictRef
             return edictRef

findFreeEdict :: Float -> Int -> Int -> Quake (Maybe (Ref' EdictT))
findFreeEdict levelTime idx maxIdx
  | idx >= maxIdx = return Nothing
  | otherwise = checkFree =<< readRef (Ref Constants.noParent idx)
  where checkFree edict
          | not (edict^.eInUse) && ((edict^.eFreeTime) < 2 || levelTime - (edict^.eFreeTime) > 0.5) = return (Just (Ref Constants.noParent idx))
          | otherwise = findFreeEdict levelTime (idx + 1) maxIdx

initEdict :: Ref' EdictT -> Quake ()
initEdict edictRef@(Ref _ idx) =
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

useTargets :: Ref' EdictT -> Maybe (Ref' EdictT) -> Quake ()
useTargets = error "GameUtil.useTargets" -- TODO

killBox :: Ref' EdictT -> Quake Bool
killBox = error "GameUtil.killBox" -- TODO