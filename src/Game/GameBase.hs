{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameBase where

import Control.Lens (use, (^.), (.=), Traversal', preuse, zoom, ix)
import Control.Monad (when, liftM, void)
import Data.Bits ((.&.), (.|.))
import Data.Char (toLower)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Client.M as M
import qualified Game.GameAI as GameAI
import qualified Game.PlayerClient as PlayerClient
import {-# SOURCE #-} qualified Server.SV as SV
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
        treatObjects maxIdx idx = do
          let er = EdictReference idx
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix idx

          if not (edict^.eInUse)
            then treatObjects maxIdx (idx + 1)
            else do
              gameBaseGlobals.gbLevel.llCurrentEntity .= Just er
              gameBaseGlobals.gbGEdicts.ix idx.eEntityState.esOldOrigin .= (edict^.eEntityState.esOrigin)

              -- if the ground entity moved, make sure we are still on it
              when (isJust (edict^.eEdictOther.eoGroundEntity)) $ do
                let Just (EdictReference groundIdx) = edict^.eEdictOther.eoGroundEntity
                Just groundEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix groundIdx

                when (groundEdict^.eLinkCount /= (edict^.eGroundEntityLinkCount)) $ do
                  gameBaseGlobals.gbGEdicts.ix idx.eEdictOther.eoGroundEntity .= Nothing
                  when ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) == 0 && (edict^.eFlags) .&. Constants.svfMonster /= 0) $
                    M.checkGround er

              maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

              if idx > 0 && idx <= maxClientsValue
                then PlayerClient.clientBeginServerFrame er
                else runEntity er

              treatObjects maxIdx (idx + 1)

{-
- This return a pointer to the structure with all entry points and global
- variables. 
-}
getGameApi :: GameImportT -> Quake ()
getGameApi imp =
    gameBaseGlobals.gbGameImport .= imp
    {- TODO:
        gi.pointcontents = new pmove_t.PointContentsAdapter() {
            public int pointcontents(float[] o) {
                return SV_WORLD.SV_PointContents(o);
            }
        };
    -} 

findByTarget :: EdictT -> B.ByteString -> Bool
findByTarget e s =
    if isNothing (e^.eEdictInfo.eiTargetName)
      then False
      else let Just targetName = e^.eEdictInfo.eiTargetName
           in BC.map toLower targetName == BC.map toLower s

gFind :: Maybe EdictReference -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe EdictReference)
gFind ref findBy str = do
    let (EdictReference edictIdx) = case ref of
                                      Nothing -> EdictReference 0
                                      Just (EdictReference refIdx) -> EdictReference (refIdx + 1)

    numEdicts <- use $ gameBaseGlobals.gbNumEdicts

    findEdict edictIdx numEdicts

  where findEdict :: Int -> Int -> Quake (Maybe EdictReference)
        findEdict idx maxIdx
          | idx == maxIdx = return Nothing
          | otherwise = do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix idx

              -- TODO: do we need this?
              {-
              if (from.o.classname == null) {
                  Com.Printf("edict with classname = null" + from.o.index);
              }
              -}

              if | not (edict^.eInUse) -> findEdict (idx + 1) maxIdx
                 | findBy edict str -> return $ Just (EdictReference idx)
                 | otherwise -> findEdict (idx + 1) maxIdx

setMoveDir :: Traversal' QuakeState (V3 Float) -> Traversal' QuakeState (V3 Float) -> Quake ()
setMoveDir anglesLens moveDirLens = do
    Just angles <- preuse anglesLens

    if | angles == vecUp -> moveDirLens .= moveDirUp
       | angles == vecDown -> moveDirLens .= moveDirDown
       | otherwise -> let (Just updatedMoveDir, _, _) = Math3D.angleVectors angles True False False
                     in moveDirLens .= updatedMoveDir

    anglesLens .= V3 0 0 0

exitLevel :: Quake ()
exitLevel = io (putStrLn "GameBase.exitLevel") >> undefined -- TODO

checkDMRules :: Quake ()
checkDMRules = io (putStrLn "GameBase.checkDMRules") >> undefined -- TODO

checkNeedPass :: Quake ()
checkNeedPass = io (putStrLn "GameBase.checkNeedPass") >> undefined -- TODO

clientEndServerFrames :: Quake ()
clientEndServerFrames = io (putStrLn "GameBase.clientEndServerFrames") >> undefined -- TODO

runEntity :: EdictReference -> Quake ()
runEntity er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    when (isJust (edict^.eEdictAction.eaPrethink)) $
      void (think (fromJust $ edict^.eEdictAction.eaPrethink) er)

    let moveType = edict^.eMoveType

    if | any (== moveType) [Constants.moveTypePush, Constants.moveTypeStop] -> SV.physicsPusher er
       | moveType == Constants.moveTypeNone -> SV.physicsNone er
       | moveType == Constants.moveTypeNoClip -> SV.physicsNoClip er
       | moveType == Constants.moveTypeStep -> SV.physicsStep er
       | any (== moveType) [Constants.moveTypeToss, Constants.moveTypeBounce, Constants.moveTypeFly, Constants.moveTypeFlyMissile] -> SV.physicsToss er
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
pickTarget :: B.ByteString -> Quake (Maybe EdictReference)
pickTarget targetName = do
    -- TODO: do we need this?
{-
    if (targetname == null) {
        gi.dprintf("G_PickTarget called with null targetname\n");
        return null;
    }
-}
    (foundRefs, numChoices) <- searchForTargets Nothing findByTarget [] 0

    if numChoices == 0
      then do
        dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
        dprintf $ "G_PickTarget: target " `B.append` targetName `B.append` " not found\n"
        return Nothing
      else do
        r <- Lib.rand
        return $ Just $ foundRefs !! (fromIntegral r `mod` numChoices)

  where searchForTargets :: Maybe EdictReference -> (EdictT -> B.ByteString -> Bool) -> [EdictReference] -> Int -> Quake ([EdictReference], Int)
        searchForTargets ref findBy foundRefs num
          | num == maxChoices = return (foundRefs, num)
          | otherwise = do
              edictRef <- gFind ref findBy targetName

              if isNothing edictRef
                then return (foundRefs, num)
                else do
                  let Just foundEdictRef = edictRef
                  searchForTargets edictRef findBy (foundEdictRef : foundRefs) (num + 1)

clipVelocity :: V3 Float -> V3 Float -> Traversal' QuakeState (V3 Float) -> Float -> Quake Int
clipVelocity _ _ _ _ = io (putStrLn "GameBase.clipVelocity") >> undefined -- TODO
