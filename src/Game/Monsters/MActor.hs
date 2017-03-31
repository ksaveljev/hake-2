{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MActor where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (%=), (&), (.~), (%~))
import Control.Monad (when, void, unless, liftM)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _y, _z)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Game.MMoveT
import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameMisc as GameMisc
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

modelScale :: Float
modelScale = 1.0

maxActorNames :: Int
maxActorNames = 8

frameAttack01 :: Int
frameAttack01 = 0

frameAttack04 :: Int
frameAttack04 = 3

frameDeath101 :: Int
frameDeath101 = 4

frameDeath107 :: Int
frameDeath107 = 10

frameDeath201 :: Int
frameDeath201 = 11

frameDeath213 :: Int
frameDeath213 = 23

frameFlip01 :: Int
frameFlip01 = 39

frameFlip14 :: Int
frameFlip14 = 52

framePain101 :: Int
framePain101 = 74

framePain103 :: Int
framePain103 = 76

framePain201 :: Int
framePain201 = 77

framePain203 :: Int
framePain203 = 79

framePain301 :: Int
framePain301 = 80

framePain303 :: Int
framePain303 = 82

frameRun02 :: Int
frameRun02 = 93

frameRun07 :: Int
frameRun07 = 98

frameStand101 :: Int
frameStand101 = 128

frameStand140 :: Int
frameStand140 = 167

frameTaunt01 :: Int
frameTaunt01 = 234

frameTaunt17 :: Int
frameTaunt17 = 250

frameWalk01 :: Int
frameWalk01 = 251

frameWalk08 :: Int
frameWalk08 = 258

actorNames :: V.Vector B.ByteString
actorNames = V.fromList [ "Hellrot", "Tokay", "Killme", "Disruptor"
                        , "Adrianator", "Rambear", "Titus", "Bitterman"
                        ]

actorStand :: EntThink
actorStand =
  GenericEntThink "actor_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveStand)

    -- randomize on startup
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    when (levelTime < 1.0) $ do
      r <- Lib.rand
      let currentMove = actorMoveStand
      modifyRef selfRef (\v -> v & eEntityState.esFrame .~ (currentMove^.mmFirstFrame) + (fromIntegral r `mod` ((currentMove^.mmLastFrame) - (currentMove^.mmFirstFrame) + 1)))

    return True

actorFramesStand :: V.Vector MFrameT
actorFramesStand =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               ]

actorMoveStand :: MMoveT
actorMoveStand = MMoveT "actorMoveStand" frameStand101 frameStand140 actorFramesStand Nothing

actorFramesWalk :: V.Vector MFrameT
actorFramesWalk =
    V.fromList [ MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  6 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk)  3 Nothing
               , MFrameT (Just GameAI.aiWalk)  2 Nothing
               , MFrameT (Just GameAI.aiWalk)  7 Nothing
               , MFrameT (Just GameAI.aiWalk) 10 Nothing
               , MFrameT (Just GameAI.aiWalk)  1 Nothing
               , MFrameT (Just GameAI.aiWalk)  4 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               , MFrameT (Just GameAI.aiWalk)  0 Nothing
               ]

actorMoveWalk :: MMoveT
actorMoveWalk = MMoveT "actorMoveWalk" frameWalk01 frameWalk08 actorFramesWalk Nothing

actorWalk :: EntThink
actorWalk =
  GenericEntThink "actor_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveWalk)
    return True

actorFramesRun :: V.Vector MFrameT
actorFramesRun =
    V.fromList [ MFrameT (Just GameAI.aiRun)   4  Nothing
               , MFrameT (Just GameAI.aiRun)  15  Nothing
               , MFrameT (Just GameAI.aiRun)  15  Nothing
               , MFrameT (Just GameAI.aiRun)   8  Nothing
               , MFrameT (Just GameAI.aiRun)  20  Nothing
               , MFrameT (Just GameAI.aiRun)  15  Nothing
               , MFrameT (Just GameAI.aiRun)   8  Nothing
               , MFrameT (Just GameAI.aiRun)  17  Nothing
               , MFrameT (Just GameAI.aiRun)  12  Nothing
               , MFrameT (Just GameAI.aiRun) (-2) Nothing
               , MFrameT (Just GameAI.aiRun) (-2) Nothing
               , MFrameT (Just GameAI.aiRun) (-1) Nothing
               ]

actorMoveRun :: MMoveT
actorMoveRun = MMoveT "actorMoveRun" frameRun02 frameRun07 actorFramesRun Nothing

actorRun :: EntThink
actorRun =
  GenericEntThink "actor_run" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if levelTime < (self^.ePainDebounceTime) && isNothing (self^.eEnemy)
      then
        case self^.eMoveTarget of
          Nothing -> void $ think actorStand selfRef
          _ -> void $ think actorWalk selfRef

      else do
        if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
          then void $ think actorStand selfRef
          else modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveRun)

    return True

actorFramesPain1 :: V.Vector MFrameT
actorFramesPain1 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-5) Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               ]

actorMovePain1 :: MMoveT
actorMovePain1 = MMoveT "actorMovePain1" framePain101 framePain103 actorFramesPain1 (Just actorRun)

actorFramesPain2 :: V.Vector MFrameT
actorFramesPain2 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-4) Nothing
               , MFrameT (Just GameAI.aiMove)   4  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

actorMovePain2 :: MMoveT
actorMovePain2 = MMoveT "actorMovePain2" framePain201 framePain203 actorFramesPain2 (Just actorRun)

actorFramesPain3 :: V.Vector MFrameT
actorFramesPain3 =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-1) Nothing
               , MFrameT (Just GameAI.aiMove)   1  Nothing
               , MFrameT (Just GameAI.aiMove)   0  Nothing
               ]

actorMovePain3 :: MMoveT
actorMovePain3 = MMoveT "actorMovePain3" framePain301 framePain303 actorFramesPain3 (Just actorRun)

actorFramesFlipOff :: V.Vector MFrameT
actorFramesFlipOff =
    V.fromList [ MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               ]

actorMoveFlipOff :: MMoveT
actorMoveFlipOff = MMoveT "actorFramesFlipOff" frameFlip01 frameFlip14 actorFramesFlipOff (Just actorRun)

actorFramesTaunt :: V.Vector MFrameT
actorFramesTaunt =
    V.fromList [ MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               , MFrameT (Just GameAI.aiTurn) 0 Nothing
               ]

actorMoveTaunt :: MMoveT
actorMoveTaunt = MMoveT "actorMoveTaunt" frameTaunt01 frameTaunt17 actorFramesTaunt (Just actorRun)

messages :: V.Vector B.ByteString
messages = V.fromList [ "Watch it", "#$@*&", "Idiot", "Check your targets" ]

actorPain :: EntPain
actorPain =
  GenericEntPain "actor_pain" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef

    when ((self^.eHealth) < (self^.eMaxHealth) `div` 2) $
      modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      modifyRef selfRef (\v -> v & ePainDebounceTime .~ levelTime + 3)

      other <- readRef otherRef
      r <- Lib.randomF

      if isJust (other^.eClient) && r < 0.4
        then do
          let a = (other^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)

          r' <- Lib.randomF
          r'' <- Lib.rand

          let currentMove = if r' < 0.5
                              then actorMoveFlipOff
                              else actorMoveTaunt

          modifyRef selfRef (\v -> v & eIdealYaw .~ Math3D.vectorYaw a
                                        & eMonsterInfo.miCurrentMove .~ Just currentMove)

          -- FIXME: does the ent-id work out ?
          let name = actorNames V.! ((self^.eIndex) `mod` maxActorNames)

          cprintf <- use $ gameBaseGlobals.gbGameImport.giCprintf
          cprintf (Just otherRef) Constants.printChat (name `B.append` ": " `B.append` (messages V.! (fromIntegral r'' `mod` 3)) `B.append` "!\n")

        else do
          n <- liftM (`mod` 3) Lib.rand

          let currentMove = if | n == 0 -> actorMovePain1
                               | n == 1 -> actorMovePain2
                               | otherwise -> actorMovePain3

          modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just currentMove)

actorDead :: EntThink
actorDead =
  GenericEntThink "actor_dead" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMins .~ V3 (-16) (-16) (-24)
                                  & eMaxs .~ V3 16 16 (-8)
                                  & eMoveType .~ Constants.moveTypeToss
                                  & eSvFlags %~ (.|. Constants.svfDeadMonster)
                                  & eNextThink .~ 0)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    return True

actorFramesDeath1 :: V.Vector MFrameT
actorFramesDeath1 =
    V.fromList [ MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove) (-13) Nothing
               , MFrameT (Just GameAI.aiMove)   14  Nothing
               , MFrameT (Just GameAI.aiMove)    3  Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               ]

actorMoveDeath1 :: MMoveT
actorMoveDeath1 = MMoveT "actorMoveDeath1" frameDeath101 frameDeath107 actorFramesDeath1 (Just actorDead)

actorFramesDeath2 :: V.Vector MFrameT
actorFramesDeath2 =
    V.fromList [ MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)    7  Nothing
               , MFrameT (Just GameAI.aiMove)  (-6) Nothing
               , MFrameT (Just GameAI.aiMove)  (-5) Nothing
               , MFrameT (Just GameAI.aiMove)    1  Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-2) Nothing
               , MFrameT (Just GameAI.aiMove)  (-1) Nothing
               , MFrameT (Just GameAI.aiMove)  (-9) Nothing
               , MFrameT (Just GameAI.aiMove) (-13) Nothing
               , MFrameT (Just GameAI.aiMove) (-13) Nothing
               , MFrameT (Just GameAI.aiMove)    0  Nothing
               ]

actorMoveDeath2 :: MMoveT
actorMoveDeath2 = MMoveT "actorMoveDeath2" frameDeath201 frameDeath213 actorFramesDeath2 (Just actorDead)

actorDie :: EntDie
actorDie =
  GenericEntDie "actor_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef

    if | (self^.eHealth) <= (-80) -> do -- check for gib
           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/bone/tris.md2" damage Constants.gibOrganic

           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
           GameMisc.throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

           GameMisc.throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead)

       | (self^.eDeadFlag) == Constants.deadDead ->
           return ()

       | otherwise -> do -- regular death
           n <- Lib.rand

           let currentMove = if n `mod` 2 == 0
                               then actorMoveDeath1
                               else actorMoveDeath2

           modifyRef selfRef (\v -> v & eDeadFlag .~ Constants.deadDead
                                         & eTakeDamage .~ Constants.damageYes
                                         & eMonsterInfo.miCurrentMove .~ Just currentMove)

actorFire :: EntThink
actorFire =
  GenericEntThink "actor_fire" $ \selfRef -> do
    actorMachineGun selfRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    self <- readRef selfRef

    if levelTime >= (self^.eMonsterInfo.miPauseTime)
      then modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiHoldFrame)))
      else modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiHoldFrame))

    return True

actorFramesAttack :: V.Vector MFrameT
actorFramesAttack =
    V.fromList [ MFrameT (Just GameAI.aiCharge) (-2) (Just actorFire)
               , MFrameT (Just GameAI.aiCharge) (-2) Nothing
               , MFrameT (Just GameAI.aiCharge)   3  Nothing
               , MFrameT (Just GameAI.aiCharge)   2  Nothing
               ]

actorMoveAttack :: MMoveT
actorMoveAttack = MMoveT "actorMoveAttack" frameAttack01 frameAttack04 actorFramesAttack (Just actorRun)

actorAttack :: EntThink
actorAttack =
  GenericEntThink "actor_attack" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    r <- Lib.rand
    let n = (r .&. 15) + 3 + 7

    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveAttack
                                  & eMonsterInfo.miPauseTime .~ levelTime + (fromIntegral n) * Constants.frameTime)

    return True

actorUse :: EntUse
actorUse =
  GenericEntUse "actor_use" $ \selfRef _ _ -> do
    self <- readRef selfRef

    target <- GameBase.pickTarget (self^.eTarget)

    modifyRef selfRef (\v -> v & eGoalEntity .~ target
                                  & eMoveTarget .~ target)

    case target of
      Nothing ->
        badTarget selfRef

      Just targetRef -> do
        target <- readRef targetRef

        if (target^.eClassName) /= "target_actor"
          then
            badTarget selfRef

          else do
            let v = (target^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
                yaw = Math3D.vectorYaw v

            modifyRef selfRef (\v -> v & eIdealYaw .~ yaw
                                          & eEntityState.esAngles._y .~ yaw) -- IMPROVE: use Constants.yaw instead of using _y directly

            void $ think (fromJust $ self^.eMonsterInfo.miWalk) selfRef

            modifyRef selfRef (\v -> v & eTarget .~ Nothing)

  where badTarget :: Ref EdictT -> Quake ()
        badTarget selfRef = do
          self <- readRef selfRef
          dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf

          dprintf ((self^.eClassName) `B.append` " has bad target " `B.append` (fromJust $ self^.eTarget) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")

          modifyRef selfRef (\v -> v & eTarget .~ Nothing
                                        & eMonsterInfo.miPauseTime .~ 100000000)

          void $ think (fromJust $ self^.eMonsterInfo.miStand) selfRef

{-
- QUAKED target_actor (.5 .3 0) (-8 -8 -8) (8 8 8) JUMP SHOOT ATTACK x HOLD
- BRUTAL JUMP jump in set direction upon reaching this target SHOOT take a
- single shot at the pathtarget ATTACK attack pathtarget until it or actor
- is dead
- 
- "target" next target_actor "pathtarget" target of any action to be taken
- at this point "wait" amount of time actor should pause at this point
- "message" actor will "say" this to the player
- 
- for JUMP only: "speed" speed thrown forward (default 200) "height" speed
- thrown upwards (default 200)
-}
targetActorTouch :: EntTouch
targetActorTouch =
  GenericEntTouch "target_actor_touch" $ \_ _ _ _ -> do
    io (putStrLn "MActor.targetActorTouch") >> undefined -- TODO

actorMachineGun :: Ref EdictT -> Quake ()
actorMachineGun _ = do
    io (putStrLn "MActor.actorMachineGun") >> undefined -- TODO

{-
- QUAKED misc_actor (1 .5 0) (-16 -16 -24) (16 16 32)
-}
spMiscActor :: Ref EdictT -> Quake ()
spMiscActor selfRef = do
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    let dprintf = gameImport^.giDprintf
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    if | deathmatchValue /= 0 ->
           GameUtil.freeEdict selfRef

       | isNothing (self^.eTargetName) -> do
           dprintf ("untargeted " `B.append` (self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
           GameUtil.freeEdict selfRef

       | isNothing (self^.eTarget) -> do
           dprintf ((self^.eClassName) `B.append` " with no target at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
           GameUtil.freeEdict selfRef

       | otherwise -> do
           modelIdx <- modelIndex (Just "players/male/tris.md2")

           modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                                         & eSolid .~ Constants.solidBbox
                                         & eEntityState.esModelIndex .~ modelIdx
                                         & eMins .~ V3 (-16) (-16) (-24)
                                         & eMaxs .~ V3 16 16 32
                                         & eHealth %~ (\v -> if v == 0 then 100 else v)
                                         & eMass .~ 200
                                         & ePain .~ Just actorPain
                                         & eDie .~ Just actorDie
                                         & eMonsterInfo.miStand .~ Just actorStand
                                         & eMonsterInfo.miWalk .~ Just actorWalk
                                         & eMonsterInfo.miRun .~ Just actorRun
                                         & eMonsterInfo.miAttack .~ Just actorAttack
                                         & eMonsterInfo.miMelee .~ Nothing
                                         & eMonsterInfo.miSight .~ Nothing
                                         & eMonsterInfo.miAIFlags %~ (.|. Constants.aiGoodGuy))

           linkEntity selfRef

           modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just actorMoveStand
                                         & eMonsterInfo.miScale .~ modelScale)

           void $ think GameAI.walkMonsterStart selfRef

           -- actors always start in a dormant state, they *must* be used
           -- to get going
           modifyRef selfRef (\v -> v & eUse .~ Just actorUse)

spTargetActor :: Ref EdictT -> Quake ()
spTargetActor selfRef = do
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity

    when (isNothing (self^.eTargetName)) $ do
      dprintf ((self^.eClassName) `B.append` " with no targetname at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")

    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidTrigger
                                  & eTouch .~ Just targetActorTouch
                                  & eMins .~ V3 (-8) (-8) (-8)
                                  & eMaxs .~ V3 8 8 8
                                  & eSvFlags .~ Constants.svfNoClient)

    when ((self^.eSpawnFlags) .&. 1 /= 0) $ do
      gameBaseGlobals.gbSpawnTemp.stHeight %= (\v -> if v == 0 then 200 else v)

      let speed = if (self^.eSpeed) == 0 then 200 else self^.eSpeed
          angle = if (self^.eEntityState.esAngles._y) == 0 then 360 else self^.eEntityState.esAngles._y -- IMPROVE: use Constants.yaw instead of using _y directly

      modifyRef selfRef (\v -> v & eSpeed .~ speed
                                    & eEntityState.esAngles._y .~ angle) -- IMPROVE: use Constants.yaw

      GameBase.setMoveDir selfRef

      height <- use $ gameBaseGlobals.gbSpawnTemp.stHeight
      modifyRef selfRef (\v -> v & eMoveDir._z .~ fromIntegral height)

    linkEntity selfRef
