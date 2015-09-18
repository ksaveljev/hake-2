{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Monsters.MInsane where

import Control.Lens (use, preuse, ix, zoom, (^.), (.=), (%=))
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameAI as GameAI
import qualified Util.Lib as Lib

frameStand1 :: Int
frameStand1 = 0

frameStand40 :: Int
frameStand40 = 39

frameStand41 :: Int
frameStand41 = 40

frameStand59 :: Int
frameStand59 = 58

frameStand60 :: Int
frameStand60 = 59

frameStand65 :: Int
frameStand65 = 64

frameStand94 :: Int
frameStand94 = 93

frameStand96 :: Int
frameStand96 = 95

frameStand99 :: Int
frameStand99 = 98

frameStand100 :: Int
frameStand100 = 99

frameStand160 :: Int
frameStand160 = 159

frameWalk27 :: Int
frameWalk27 = 160

frameWalk39 :: Int
frameWalk39 = 172

frameWalk1 :: Int
frameWalk1 = 173

frameWalk26 :: Int
frameWalk26 = 198

frameStandPain2 :: Int
frameStandPain2 = 199

frameStandPain12 :: Int
frameStandPain12 = 209

frameStandDeath2 :: Int
frameStandDeath2 = 210

frameStandDeath18 :: Int
frameStandDeath18 = 226

frameCrawl1 :: Int
frameCrawl1 = 227

frameCrawl9 :: Int
frameCrawl9 = 235

frameCrawlPain2 :: Int
frameCrawlPain2 = 236

frameCrawlPain10 :: Int
frameCrawlPain10 = 244

frameCrawlDeath10 :: Int
frameCrawlDeath10 = 245

frameCrawlDeath16 :: Int
frameCrawlDeath16 = 251

frameCross1 :: Int
frameCross1 = 252

frameCross15 :: Int
frameCross15 = 266

frameCross16 :: Int
frameCross16 = 267

frameCross30 :: Int
frameCross30 = 281

insaneFist :: EntThink
insaneFist =
  GenericEntThink "insane_fist" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundFist <- use $ mInsaneGlobals.mInsaneSoundFist
    sound (Just selfRef) Constants.chanVoice soundFist 1 Constants.attnIdle 0
    return True

insaneShake :: EntThink
insaneShake =
  GenericEntThink "insane_shake" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundShake <- use $ mInsaneGlobals.mInsaneSoundShake
    sound (Just selfRef) Constants.chanVoice soundShake 1 Constants.attnIdle 0
    return True

insaneMoan :: EntThink
insaneMoan =
  GenericEntThink "insane_moan" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundMoan <- use $ mInsaneGlobals.mInsaneSoundMoan
    sound (Just selfRef) Constants.chanVoice soundMoan 1 Constants.attnIdle 0
    return True

insaneScream :: EntThink
insaneScream =
  GenericEntThink "insane_scream" $ \selfRef -> do
    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    soundScream <- use $ mInsaneGlobals.mInsaneSoundScream
    r <- Lib.rand
    sound (Just selfRef) Constants.chanVoice (soundScream UV.! (fromIntegral $ r `mod` 8)) 1 Constants.attnIdle 0
    return True

insaneCross :: EntThink
insaneCross =
  GenericEntThink "insane_cross" $ \(EdictReference selfIdx) -> do
    r <- Lib.randomF

    let action = if r < 0.8
                   then insaneMoveCross
                   else insaneMoveStruggleCross

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

insaneWalk :: EntThink
insaneWalk =
  GenericEntThink "insane_walk" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eSpawnFlags) .&. 16 /= 0 && (self^.eEntityState.esFrame) == frameCrawlPain10
      then do
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just insaneMoveDown
        return True

      else do
        r <- Lib.randomF

        let currentMove = if | (self^.eSpawnFlags) .&. 4 /= 0 -> insaneMoveCrawl
                             | r <= 0.5 -> insaneMoveWalkNormal
                             | otherwise -> insaneMoveWalkInsane

        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove
        return True

insaneRun :: EntThink
insaneRun =
  GenericEntThink "insane_run" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eSpawnFlags) .&. 16 /= 0 && (self^.eEntityState.esFrame) == frameCrawlPain10
      then do
        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just insaneMoveDown
        return True

      else do
        r <- Lib.randomF

        let currentMove = if | (self^.eSpawnFlags) .&. 4 /= 0 -> insaneMoveRunCrawl
                             | r <= 0.5 -> insaneMoveRunNormal
                             | otherwise -> insaneMoveRunInsane

        gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove
        return True

insanePain :: EntPain
insanePain =
  GenericEntPain "insane_pain" $ \selfRef@(EdictReference selfIdx) _ _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    unless (levelTime < (self^.ePainDebounceTime)) $ do
      gameBaseGlobals.gbGEdicts.ix selfIdx.ePainDebounceTime .= levelTime + 3

      r <- Lib.rand
      let r' = 1 + (r .&. 1)
          l = if | (self^.eHealth) < 25 -> 25
                 | (self^.eHealth) < 50 -> 50
                 | (self^.eHealth) < 75 -> 75
                 | otherwise -> 100

      gameImport <- use $ gameBaseGlobals.gbGameImport

      let soundIndex = gameImport^.giSoundIndex
          sound = gameImport^.giSound

      soundIdx <- soundIndex (Just ("player/male/pain" `B.append` BC.pack (show l) `B.append` "_" `B.append` BC.pack (show r') `B.append` ".wav"))
      sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnIdle 0

      skillValue <- liftM (^.cvValue) skillCVar

      unless (skillValue == 3) $ do -- no pain anims in nightmare
        -- don't go into pain frames if crucified
        if (self^.eSpawnFlags) .&. 8 /= 0
          then
            gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just insaneMoveStruggleCross

          else do
            let frame = self^.eEntityState.esFrame
                currentMove = if frame >= frameCrawl1 && frame <= frameCrawl9 || frame >= frameStand99 && frame <= frameStand160
                                then insaneMoveCrawlPain
                                else insaneMoveStandPain

            gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove

insaneOnGround :: EntThink
insaneOnGround =
  GenericEntThink "insane_onground" $ \(EdictReference selfIdx) -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just insaneMoveDown
    return True

insaneCheckDown :: EntThink
insaneCheckDown =
  GenericEntThink "insane_checkdown" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eSpawnFlags) .&. 32 /= 0 -- always stand
      then
        return True

      else do
        r <- Lib.randomF

        when (r < 0.3) $ do
          r' <- Lib.randomF

          let currentMove = if r' < 0.5
                              then insaneMoveUpToDown
                              else insaneMoveJumpDown

          gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just currentMove

        return True

insaneCheckUp :: EntThink
insaneCheckUp =
  GenericEntThink "insane_checkup" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    -- If Hold_Ground and Crawl are set
    if (self^.eSpawnFlags) .&. 4 /= 0 && (self^.eSpawnFlags) .&. 16 /= 0
      then return True
      else do
        r <- Lib.randomF

        when (r < 0.5) $
          gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just insaneMoveDownToUp

        return True

insaneStand :: EntThink
insaneStand =
  GenericEntThink "insane_stand" $ \(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    action <- if | (self^.eSpawnFlags) .&. 8 /= 0 -> do -- If crucified
                     gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiStandGround)
                     return insaneMoveCross

                 | (self^.eSpawnFlags) .&. 4 /= 0 && (self^.eSpawnFlags) .&. 16 /= 0 ->
                     return insaneMoveDown

                 | otherwise -> do
                     r <- Lib.randomF
                     return $ if r < 0.5 then insaneMoveStandNormal else insaneMoveStandInsane

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miCurrentMove .= Just action
    return True

insaneDead :: EntThink
insaneDead =
  GenericEntThink "insane_dead" $ \_ -> do
    io (putStrLn "MInsane.insaneDead") >> undefined -- TODO

insaneDie :: EntDie
insaneDie =
  GenericEntDie "insane_die" $ \_ _ _ _ _ -> do
    io (putStrLn "MInsane.insaneDie") >> undefined -- TODO

insaneFramesStandNormal :: V.Vector MFrameT
insaneFramesStandNormal =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 Nothing
               , MFrameT (Just GameAI.aiStand) 0 (Just insaneCheckDown)
               ]

insaneMoveStandNormal :: MMoveT
insaneMoveStandNormal = MMoveT "insaneMoveStandNormal" frameStand60 frameStand65 insaneFramesStandNormal (Just insaneStand)

insaneFramesStandInsane :: V.Vector MFrameT
insaneFramesStandInsane =
    V.fromList [ MFrameT (Just GameAI.aiStand) 0 (Just insaneShake)
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
               , MFrameT (Just GameAI.aiStand) 0 (Just insaneCheckDown)
               ]

insaneMoveStandInsane :: MMoveT
insaneMoveStandInsane = MMoveT "insaneMoveStandInsane" frameStand65 frameStand94 insaneFramesStandInsane (Just insaneStand)

insaneFramesUpToDown :: V.Vector MFrameT
insaneFramesUpToDown =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   (Just insaneMoan)
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 2.7 Nothing
               , MFrameT (Just GameAI.aiMove) 4.1 Nothing
               , MFrameT (Just GameAI.aiMove) 6   Nothing
               , MFrameT (Just GameAI.aiMove) 7.6 Nothing
               , MFrameT (Just GameAI.aiMove) 3.6 Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   (Just insaneFist)
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   (Just insaneFist)
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               , MFrameT (Just GameAI.aiMove) 0   Nothing
               ]

insaneMoveUpToDown :: MMoveT
insaneMoveUpToDown = MMoveT "insaneMoveUpToDown" frameStand1 frameStand40 insaneFramesUpToDown (Just insaneOnGround)

insaneFramesDownToUp :: V.Vector MFrameT
insaneFramesDownToUp =
    V.fromList [ MFrameT (Just GameAI.aiMove) (-0.7) Nothing -- 41
               , MFrameT (Just GameAI.aiMove) (-1.2) Nothing -- 42
               , MFrameT (Just GameAI.aiMove) (-1.5) Nothing -- 43
               , MFrameT (Just GameAI.aiMove) (-4.5) Nothing -- 44
               , MFrameT (Just GameAI.aiMove) (-3.5) Nothing -- 45
               , MFrameT (Just GameAI.aiMove) (-0.2) Nothing -- 46
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 47
               , MFrameT (Just GameAI.aiMove) (-1.3) Nothing -- 48
               , MFrameT (Just GameAI.aiMove)   (-3) Nothing -- 49
               , MFrameT (Just GameAI.aiMove)   (-2) Nothing -- 50
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 51
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 52
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 53
               , MFrameT (Just GameAI.aiMove) (-3.3) Nothing -- 54
               , MFrameT (Just GameAI.aiMove) (-1.6) Nothing -- 55
               , MFrameT (Just GameAI.aiMove) (-0.3) Nothing -- 56
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 57
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 58
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 59
               ]

insaneMoveDownToUp :: MMoveT
insaneMoveDownToUp = MMoveT "insaneMoveDownToUp" frameStand41 frameStand59 insaneFramesDownToUp (Just insaneStand)

insaneFramesJumpDown :: V.Vector MFrameT
insaneFramesJumpDown =
    V.fromList [ MFrameT (Just GameAI.aiMove)  0.2 Nothing
               , MFrameT (Just GameAI.aiMove) 11.5 Nothing
               , MFrameT (Just GameAI.aiMove)  5.1 Nothing
               , MFrameT (Just GameAI.aiMove)  7.1 Nothing
               , MFrameT (Just GameAI.aiMove)    0 Nothing
               ]

insaneMoveJumpDown :: MMoveT
insaneMoveJumpDown = MMoveT "insaneMoveJumpDown" frameStand96 frameStand100 insaneFramesJumpDown (Just insaneOnGround)

insaneFramesDown :: V.Vector MFrameT
insaneFramesDown =
    V.fromList [ MFrameT (Just GameAI.aiMove)     0  Nothing -- 100
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 110
               , MFrameT (Just GameAI.aiMove) (-1.7) Nothing
               , MFrameT (Just GameAI.aiMove) (-1.6) Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  (Just insaneFist)
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 120)
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 130
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  (Just insaneMoan)
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 140
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing -- 150
               , MFrameT (Just GameAI.aiMove)   0.5  Nothing
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove) (-0.2) (Just insaneScream)
               , MFrameT (Just GameAI.aiMove)     0  Nothing
               , MFrameT (Just GameAI.aiMove)   0.2  Nothing
               , MFrameT (Just GameAI.aiMove)   0.4  Nothing
               , MFrameT (Just GameAI.aiMove)   0.6  Nothing
               , MFrameT (Just GameAI.aiMove)   0.8  Nothing
               , MFrameT (Just GameAI.aiMove)   0.7  Nothing
               , MFrameT (Just GameAI.aiMove)     0  (Just insaneCheckUp) -- 160
               ]

insaneMoveDown :: MMoveT
insaneMoveDown = MMoveT "insaneMoveDown" frameStand100 frameStand160 insaneFramesDown (Just insaneOnGround)

insaneFramesWalkNormal :: V.Vector MFrameT
insaneFramesWalkNormal =
    V.fromList [ MFrameT (Just GameAI.aiWalk)   0 (Just insaneScream)
               , MFrameT (Just GameAI.aiWalk) 2.5 Nothing
               , MFrameT (Just GameAI.aiWalk) 3.5 Nothing
               , MFrameT (Just GameAI.aiWalk) 1.7 Nothing
               , MFrameT (Just GameAI.aiWalk) 2.3 Nothing
               , MFrameT (Just GameAI.aiWalk) 2.4 Nothing
               , MFrameT (Just GameAI.aiWalk) 2.2 Nothing
               , MFrameT (Just GameAI.aiWalk) 4.2 Nothing
               , MFrameT (Just GameAI.aiWalk) 5.6 Nothing
               , MFrameT (Just GameAI.aiWalk) 3.3 Nothing
               , MFrameT (Just GameAI.aiWalk) 2.4 Nothing
               , MFrameT (Just GameAI.aiWalk) 0.9 Nothing
               , MFrameT (Just GameAI.aiWalk)   0 Nothing
               ]

insaneMoveWalkNormal :: MMoveT
insaneMoveWalkNormal = MMoveT "insaneMoveWalkNormal" frameWalk27 frameWalk39 insaneFramesWalkNormal (Just insaneWalk)

insaneMoveRunNormal :: MMoveT
insaneMoveRunNormal = MMoveT "insaneMoveRunNormal" frameWalk27 frameWalk39 insaneFramesWalkNormal (Just insaneRun)

insaneFramesWalkInsane :: V.Vector MFrameT
insaneFramesWalkInsane =
    V.fromList [ MFrameT (Just GameAI.aiWalk)   0 (Just insaneScream) -- walk 1
               , MFrameT (Just GameAI.aiWalk) 3.4 Nothing -- walk 2
               , MFrameT (Just GameAI.aiWalk) 3.6 Nothing -- 3
               , MFrameT (Just GameAI.aiWalk) 2.9 Nothing -- 4
               , MFrameT (Just GameAI.aiWalk) 2.2 Nothing -- 5
               , MFrameT (Just GameAI.aiWalk) 2.6 Nothing -- 6
               , MFrameT (Just GameAI.aiWalk)   0 Nothing -- 7
               , MFrameT (Just GameAI.aiWalk) 0.7 Nothing -- 8
               , MFrameT (Just GameAI.aiWalk) 4.8 Nothing -- 9
               , MFrameT (Just GameAI.aiWalk) 5.3 Nothing -- 10
               , MFrameT (Just GameAI.aiWalk) 1.1 Nothing -- 11
               , MFrameT (Just GameAI.aiWalk)   2 Nothing -- 12
               , MFrameT (Just GameAI.aiWalk) 0.5 Nothing -- 13
               , MFrameT (Just GameAI.aiWalk)   0 Nothing -- 14
               , MFrameT (Just GameAI.aiWalk)   0 Nothing -- 15
               , MFrameT (Just GameAI.aiWalk) 4.9 Nothing -- 16
               , MFrameT (Just GameAI.aiWalk) 6.7 Nothing -- 17
               , MFrameT (Just GameAI.aiWalk) 3.8 Nothing -- 18
               , MFrameT (Just GameAI.aiWalk)   2 Nothing -- 19
               , MFrameT (Just GameAI.aiWalk) 0.2 Nothing -- 20
               , MFrameT (Just GameAI.aiWalk)   0 Nothing -- 21
               , MFrameT (Just GameAI.aiWalk) 3.4 Nothing -- 22
               , MFrameT (Just GameAI.aiWalk) 6.4 Nothing -- 23
               , MFrameT (Just GameAI.aiWalk)   5 Nothing -- 24
               , MFrameT (Just GameAI.aiWalk) 1.8 Nothing -- 25
               , MFrameT (Just GameAI.aiWalk)   0 Nothing -- 26
               ]

insaneMoveWalkInsane :: MMoveT
insaneMoveWalkInsane = MMoveT "insaneMoveWalkInsane" frameWalk1 frameWalk26 insaneFramesWalkInsane (Just insaneWalk)

insaneMoveRunInsane :: MMoveT
insaneMoveRunInsane = MMoveT "insaneMoveRunInsane" frameWalk1 frameWalk26 insaneFramesWalkInsane (Just insaneRun)

insaneFramesStandPain :: V.Vector MFrameT
insaneFramesStandPain =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

insaneMoveStandPain :: MMoveT
insaneMoveStandPain = MMoveT "insaneMoveStandPain" frameStandPain2 frameStandPain12 insaneFramesStandPain (Just insaneRun)

insaneFramesStandDeath :: V.Vector MFrameT
insaneFramesStandDeath =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

insaneMoveStandDeath :: MMoveT
insaneMoveStandDeath = MMoveT "insaneMoveStandDeath" frameStandDeath2 frameStandDeath18 insaneFramesStandDeath (Just insaneDead)

insaneFramesCrawl :: V.Vector MFrameT
insaneFramesCrawl =
    V.fromList [ MFrameT (Just GameAI.aiWalk)   0 (Just insaneScream)
               , MFrameT (Just GameAI.aiWalk) 1.5 Nothing
               , MFrameT (Just GameAI.aiWalk) 2.1 Nothing
               , MFrameT (Just GameAI.aiWalk) 3.6 Nothing
               , MFrameT (Just GameAI.aiWalk)   2 Nothing
               , MFrameT (Just GameAI.aiWalk) 0.9 Nothing
               , MFrameT (Just GameAI.aiWalk)   3 Nothing
               , MFrameT (Just GameAI.aiWalk) 3.4 Nothing
               , MFrameT (Just GameAI.aiWalk) 2.4 Nothing
               ]

insaneMoveCrawl :: MMoveT
insaneMoveCrawl = MMoveT "insaneMoveCrawl" frameCrawl1 frameCrawl9 insaneFramesCrawl Nothing

insaneMoveRunCrawl :: MMoveT
insaneMoveRunCrawl = MMoveT "insaneMoveRunCrawl" frameCrawl1 frameCrawl9 insaneFramesCrawl Nothing

insaneFramesCrawlPain :: V.Vector MFrameT
insaneFramesCrawlPain =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

insaneMoveCrawlPain :: MMoveT
insaneMoveCrawlPain = MMoveT "insaneMoveCrawlPain" frameCrawlPain2 frameCrawlPain10 insaneFramesCrawlPain (Just insaneRun)

insaneFramesCrawlDeath :: V.Vector MFrameT
insaneFramesCrawlDeath =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

insaneMoveCrawlDeath :: MMoveT
insaneMoveCrawlDeath = MMoveT "insaneMoveCrawlDeath" frameCrawlDeath10 frameCrawlDeath16 insaneFramesCrawlDeath (Just insaneDead)

insaneFramesCross :: V.Vector MFrameT
insaneFramesCross =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 (Just insaneMoan)
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

insaneMoveCross :: MMoveT
insaneMoveCross = MMoveT "insaneMoveCross" frameCross1 frameCross15 insaneFramesCross (Just insaneCross)

insaneFramesStruggleCross :: V.Vector MFrameT
insaneFramesStruggleCross =
    V.fromList [ MFrameT (Just GameAI.aiMove) 0 (Just insaneScream)
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               , MFrameT (Just GameAI.aiMove) 0 Nothing
               ]

insaneMoveStruggleCross :: MMoveT
insaneMoveStruggleCross = MMoveT "insaneMoveStruggleCross" frameCross16 frameCross30 insaneFramesStruggleCross (Just insaneCross)

{-
- QUAKED misc_insane (1 .5 0) (-16 -16 -24) (16 16 32) Ambush Trigger_Spawn
- CRAWL CRUCIFIED STAND_GROUND ALWAYS_STAND
-}
spMiscInsane :: EdictReference -> Quake ()
spMiscInsane _ = do
    io (putStrLn "MInsane.spMiscInsane") >> undefined -- TODO
