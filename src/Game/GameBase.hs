{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.GameBase where

import Control.Lens (use, (^.), (.=), Traversal', preuse, zoom, ix, (&), (.~))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), (.|.))
import Data.Char (toLower)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _x, _y, _z, dot, norm)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Game.EdictT
import Game.EntityStateT
import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import QuakeRef
import Types
import Game.CVarT
import QuakeState
import CVarVariables
import qualified Constants
import qualified Client.M as M
import qualified Game.GameAI as GameAI
import qualified Game.PlayerClient as PlayerClient
import qualified Game.PlayerView as PlayerView
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified Server.SV as SV
import qualified Server.SVWorld as SVWorld
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

vecUp :: V3 Float
vecUp = V3 0 (-1) 0

moveDirUp :: V3 Float
moveDirUp = V3 0 0 1

vecDown :: V3 Float
vecDown = V3 0 (-2) 0

moveDirDown :: V3 Float
moveDirDown = V3 0 0 (-1)

maxChoices :: Int
maxChoices = 8

stopEpsilon :: Float
stopEpsilon = 0.1

shutdownGame :: Quake ()
shutdownGame = do
    gameimport <- use $ gameBaseGlobals.gbGameImport
    (gameimport^.giDprintf) "==== ShutdownGame ====\n"

{-
- G_RunFrame
-  
- Advances the world by Defines.FRAMETIME (0.1) seconds.
-}
runFrame :: Quake ()
runFrame = do
    level <- use $ gameBaseGlobals.gbLevel

    zoom (gameBaseGlobals.gbLevel) $ do
      llFrameNum .= (level^.llFrameNum) + 1
      llTime .= (fromIntegral (level^.llFrameNum) + 1) * Constants.frameTime

    -- choose a client for monsters to target this frame
    GameAI.aiSetSightClient

    -- exit intermissions
    if level^.llExitIntermission
      then exitLevel
      else do
        -- treat each object in turn
        -- even the world gets a chance to think
        numEdicts <- use $ gameBaseGlobals.gbNumEdicts
        treatObjects numEdicts 0

        -- see if it is time to end a deathmatch
        checkDMRules

        -- see if needpass needs updated
        checkNeedPass
          
        -- build the playerstate_t structures for all players
        clientEndServerFrames

  where treatObjects :: Int -> Int -> Quake ()
        treatObjects maxIdx idx
         | idx >= maxIdx = return ()
         | otherwise = do
             let edictRef = Ref idx
             edict <- readRef edictRef

             if not (edict^.eInUse)
               then
                 treatObjects maxIdx (idx + 1)

               else do
                 gameBaseGlobals.gbLevel.llCurrentEntity .= Just edictRef
                 modifyRef edictRef (\v -> v & eEntityState.esOldOrigin .~ (edict^.eEntityState.esOrigin))

                 -- if the ground entity moved, make sure we are still on it
                 when (isJust (edict^.eGroundEntity)) $ do
                   let Just groundRef = edict^.eGroundEntity
                   groundEdict <- readRef groundRef

                   when (groundEdict^.eLinkCount /= (edict^.eGroundEntityLinkCount)) $ do
                     modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
                     when ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) == 0 && (edict^.eFlags) .&. Constants.svfMonster /= 0) $
                       M.checkGround edictRef

                 maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

                 if idx > 0 && idx <= maxClientsValue
                   then PlayerClient.clientBeginServerFrame edictRef
                   else runEntity edictRef

                 treatObjects maxIdx (idx + 1)

{-
- This return a pointer to the structure with all entry points and global
- variables. 
-}
getGameApi :: GameImportT -> Quake ()
getGameApi imp =
    gameBaseGlobals.gbGameImport .= imp { _giPointContents = SVWorld.pointContents }

findByTarget :: EdictT -> B.ByteString -> Bool
findByTarget e s =
    if isNothing (e^.eTargetName)
      then False
      else let Just targetName = e^.eTargetName
           in BC.map toLower targetName == BC.map toLower s

findByClass :: EdictT -> B.ByteString -> Bool
findByClass e s = BC.map toLower (e^.eClassName) == BC.map toLower s

gFind :: Maybe (Ref EdictT) -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe (Ref EdictT))
gFind ref findBy str = do
    let edictRef = case ref of
                     Nothing -> worldRef
                     Just (Ref idx) -> Ref (idx + 1)

    numEdicts <- use $ gameBaseGlobals.gbNumEdicts

    findEdict edictRef (Ref numEdicts)

  where findEdict :: Ref EdictT -> Ref EdictT -> Quake (Maybe (Ref EdictT))
        findEdict edictRef@(Ref idx) maxEdictRef
          | edictRef >= maxEdictRef = return Nothing
          | otherwise = do
              edict <- readRef edictRef

              -- TODO: do we need this?
              {-
              if (from.o.classname == null) {
                  Com.Printf("edict with classname = null" + from.o.index);
              }
              -}

              if | not (edict^.eInUse) -> findEdict (Ref (idx + 1)) maxEdictRef
                 | findBy edict str -> return $ Just edictRef
                 | otherwise -> findEdict (Ref (idx + 1)) maxEdictRef

setMoveDir :: Ref EdictT -> Quake ()
setMoveDir edictRef = do
    edict <- readRef edictRef

    let angles = edict^.eEntityState.esAngles
        moveDir = edict^.eMoveDir
        moveDir' = if | angles == vecUp -> moveDirUp
                      | angles == vecDown -> moveDirDown
                      | otherwise -> let (Just forward, _, _) = Math3D.angleVectors angles True False False
                                    in forward

    modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                                   & eMoveDir .~ moveDir')

{-
- used this version when Vector EdictT in GameBaseGlobals was immutable
-
setMoveDir :: Traversal' QuakeState (V3 Float) -> Traversal' QuakeState (V3 Float) -> Quake ()
setMoveDir anglesLens moveDirLens = do
    Just angles <- preuse anglesLens

    if | angles == vecUp -> moveDirLens .= moveDirUp
       | angles == vecDown -> moveDirLens .= moveDirDown
       | otherwise -> let (Just updatedMoveDir, _, _) = Math3D.angleVectors angles True False False
                     in moveDirLens .= updatedMoveDir

    anglesLens .= V3 0 0 0
    -}

exitLevel :: Quake ()
exitLevel = io (putStrLn "GameBase.exitLevel") >> undefined -- TODO

checkDMRules :: Quake ()
checkDMRules = do
    intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    unless (intermissionTime /= 0 || deathmatchValue == 0) $ do
      timeLimitValue <- liftM (^.cvValue) timeLimitCVar
      fragLimitValue <- liftM (truncate . (^.cvValue)) fragLimitCVar
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      bprintf <- use $ gameBaseGlobals.gbGameImport.giBprintf

      if timeLimitValue /= 0 && levelTime >= timeLimitValue * 60
        then do
          bprintf Constants.printHigh "Timelimit hit.\n"
          endDMLevel
        else do
          when (fragLimitValue /= 0) $ do
            maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
            shouldEnd <- checkFragLimit fragLimitValue 0 maxClientsValue
            when (shouldEnd) $ do
              bprintf Constants.printHigh "Fraglimit hit.\n"
              endDMLevel

  where checkFragLimit :: Int -> Int -> Int -> Quake Bool
        checkFragLimit fragLimit idx maxIdx
          | idx >= maxIdx = return False
          | otherwise = do
              edict <- readRef (Ref (idx + 1))

              if edict^.eInUse
                then do
                  Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix idx

                  if (client^.gcResp.crScore) >= fragLimit
                    then return True
                    else checkFragLimit fragLimit (idx + 1) maxIdx

                else
                  checkFragLimit fragLimit (idx + 1) maxIdx

checkNeedPass :: Quake ()
checkNeedPass = do
    -- if password or spectator_password has changed, update needpass as needed
    password <- passwordCVar
    spectatorPassword <- spectatorPasswordCVar

    when ((password^.cvModified) || (spectatorPassword^.cvModified)) $ do
      CVar.update password { _cvModified = False }
      CVar.update spectatorPassword { _cvModified = False }

      let need :: Int = 0
          need' = if BC.map toLower (password^.cvString) /= "none" then (need .|. 1) else need
          need'' = if BC.map toLower (spectatorPassword^.cvString) /= "none" then (need' .|. 2) else need'

      cVarSet <- use $ gameBaseGlobals.gbGameImport.giCVarSet
      void $ cVarSet "needpass" (BC.pack (show need'')) -- IMPROVE ?

clientEndServerFrames :: Quake ()
clientEndServerFrames = do
    -- calc the player views now that all pushing
    -- and damage has been added
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
    calcPlayerViews 0 maxClientsValue

  where calcPlayerViews :: Int -> Int -> Quake ()
        calcPlayerViews idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let edictRef = Ref (idx + 1)
              edict <- readRef edictRef

              unless (not (edict^.eInUse) || isNothing (edict^.eClient)) $
                PlayerView.clientEndServerFrame edictRef

              calcPlayerViews (idx + 1) maxIdx

runEntity :: Ref EdictT -> Quake ()
runEntity edictRef = do
    edict <- readRef edictRef

    when (isJust (edict^.ePrethink)) $
      void (think (fromJust $ edict^.ePrethink) edictRef)

    let moveType = edict^.eMoveType

    if | any (== moveType) [Constants.moveTypePush, Constants.moveTypeStop] -> SV.physicsPusher edictRef
       | moveType == Constants.moveTypeNone -> SV.physicsNone edictRef
       | moveType == Constants.moveTypeNoClip -> SV.physicsNoClip edictRef
       | moveType == Constants.moveTypeStep -> SV.physicsStep edictRef
       | any (== moveType) [Constants.moveTypeToss, Constants.moveTypeBounce, Constants.moveTypeFly, Constants.moveTypeFlyMissile] -> SV.physicsToss edictRef
       | otherwise -> do
           err <- use $ gameBaseGlobals.gbGameImport.giError
           err $ "SV_Physics: bad movetype " `B.append` BC.pack (show moveType) -- IMPROVE?

{-
- Searches all active entities for the next one that holds the matching
- string at fieldofs (use the FOFS() macro) in the structure.
- 
- Searches beginning at the edict after from, or the beginning if null null
- will be returned if the end of the list is reached.
-}
pickTarget :: Maybe B.ByteString -> Quake (Maybe (Ref EdictT))
pickTarget targetName = do
    dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf

    if isNothing targetName
      then do
        dprintf "G_PickTarget called with null targetname\n"
        return Nothing
      else do
        (foundRefs, numChoices) <- searchForTargets Nothing findByTarget [] 0

        if numChoices == 0
          then do
            dprintf $ "G_PickTarget: target " `B.append` (fromJust targetName) `B.append` " not found\n"
            return Nothing
          else do
            r <- Lib.rand
            return $ Just $ foundRefs !! (fromIntegral r `mod` numChoices)

  where searchForTargets :: Maybe (Ref EdictT) -> (EdictT -> B.ByteString -> Bool) -> [Ref EdictT] -> Int -> Quake ([Ref EdictT], Int)
        searchForTargets ref findBy foundRefs num
          | num == maxChoices = return (foundRefs, num)
          | otherwise = do
              edictRef <- gFind ref findBy (fromJust targetName)

              if isNothing edictRef
                then
                  return (foundRefs, num)

                else do
                  let Just foundEdictRef = edictRef
                  searchForTargets edictRef findBy (foundEdictRef : foundRefs) (num + 1)

clipVelocity :: V3 Float -> V3 Float -> Float -> (Int, V3 Float)
clipVelocity v3in normal overbounce =
    let isBlocked = if | normal^._z > 0 -> 1 -- floor
                       | normal^._z == 0 -> 2 -- step
                       | otherwise -> 0

        backoff = (dot v3in normal) * overbounce
        change = fmap (* backoff) normal
        out = v3in - change
        v3out = fmap (\v -> if v > (-stopEpsilon) && v < stopEpsilon then 0 else v) out

    in (isBlocked, v3out)

touchTriggers :: Ref EdictT -> Quake ()
touchTriggers edictRef = do
    edict <- readRef edictRef

    -- dead things don't activate triggers!
    unless ((isJust (edict^.eClient) || (edict^.eSvFlags) .&. Constants.svfMonster /= 0) && (edict^.eHealth <= 0)) $ do
      boxEdicts <- use $ gameBaseGlobals.gbGameImport.giBoxEdicts
      num <- boxEdicts (edict^.eAbsMin) (edict^.eAbsMax) (gameBaseGlobals.gbTouch) Constants.maxEdicts Constants.areaTriggers

      -- io (print "NUM")
      -- io (print num)

      -- be careful, it is possible to have an entity in this
      -- list removed before we got to it (killtriggered)
      touchEdicts 0 num

  where touchEdicts :: Int -> Int -> Quake ()
        touchEdicts idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just hitRef <- preuse $ gameBaseGlobals.gbTouch.ix idx
              hit <- readRef hitRef

              if not (hit^.eInUse) || isNothing (hit^.eTouch)
                then
                  touchEdicts (idx + 1) maxIdx

                else do
                  dummyPlane <- use $ gameBaseGlobals.gbDummyPlane
                  touch (fromJust $ hit^.eTouch) hitRef edictRef dummyPlane Nothing

addPointToBound :: V3 Float -> V3 Float -> V3 Float -> (V3 Float, V3 Float)
addPointToBound v mins maxs =
    let mina = if (v^._x) < (mins^._x) then v^._x else mins^._x
        minb = if (v^._y) < (mins^._y) then v^._z else mins^._z
        minc = if (v^._y) < (mins^._y) then v^._z else mins^._z
        maxa = if (v^._x) > (maxs^._x) then v^._x else maxs^._x
        maxb = if (v^._y) > (maxs^._y) then v^._y else maxs^._y
        maxc = if (v^._z) > (maxs^._z) then v^._z else maxs^._z
    in (V3 mina minb minc, V3 maxa maxb maxc)

-- The timelimit or fraglimit has been exceeded.
endDMLevel :: Quake ()
endDMLevel = io (putStrLn "GameBase.endDMLevel") >> undefined -- TODO

findRadius :: Maybe (Ref EdictT) -> V3 Float -> Float -> Quake (Maybe (Ref EdictT))
findRadius fromRef org rad = do
    let edictRef = case fromRef of
                     Nothing -> worldRef
                     Just (Ref idx) -> Ref (idx + 1)

    numEdicts <- use $ gameBaseGlobals.gbNumEdicts

    findEdict edictRef (Ref numEdicts)

  where findEdict :: Ref EdictT -> Ref EdictT -> Quake (Maybe (Ref EdictT))
        findEdict edictRef@(Ref idx) maxEdictRef
          | edictRef >= maxEdictRef = return Nothing
          | otherwise = do
              edict <- readRef edictRef

              if | not (edict^.eInUse) -> findEdict (Ref (idx + 1)) maxEdictRef
                 | (edict^.eSolid) == Constants.solidNot -> findEdict (Ref (idx + 1)) maxEdictRef
                 | otherwise -> do
                     let eorg = org - ((edict^.eEntityState.esOrigin) + fmap (* 0.5) ((edict^.eMins) + (edict^.eMaxs)))

                     if norm eorg > rad
                       then findEdict (Ref (idx + 1)) maxEdictRef
                       else return $ Just edictRef
