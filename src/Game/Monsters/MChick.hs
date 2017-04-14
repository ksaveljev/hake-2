module Game.Monsters.MChick
    ( spMonsterChick
    ) where

import           Control.Lens          (use, preuse, ix, zoom)
import           Control.Lens          ((^.), (.=), (%=), (+=), (-=), (&), (.~), (%~), (-~), (+~))
import           Control.Monad         (when, unless, void)
import           Data.Bits             (complement, (.&.), (.|.))
import           Data.Maybe            (isJust)
import qualified Data.Vector           as V
import           Linear                (V3(..), normalize, _x, _y, _z)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameWeapon       as GameWeapon
import qualified Game.GameUtil         as GameUtil
import           Game.MMoveT
import           Game.MonsterInfoT
import qualified Game.Monsters.MFlash  as MFlash
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

modelScale :: Float
modelScale = 1.0

frameAttack101 :: Int
frameAttack101 = 0

frameAttack113 :: Int
frameAttack113 = 12

frameAttack114 :: Int
frameAttack114 = 13

frameAttack127 :: Int
frameAttack127 = 26

frameAttack128 :: Int
frameAttack128 = 27

frameAttack132 :: Int
frameAttack132 = 31

frameAttack201 :: Int
frameAttack201 = 32

frameAttack203 :: Int
frameAttack203 = 34

frameAttack204 :: Int
frameAttack204 = 35

frameAttack212 :: Int
frameAttack212 = 43

frameAttack213 :: Int
frameAttack213 = 44

frameAttack216 :: Int
frameAttack216 = 47

frameDeath101 :: Int
frameDeath101 = 48

frameDeath112 :: Int
frameDeath112 = 59

frameDeath201 :: Int
frameDeath201 = 60

frameDeath223 :: Int
frameDeath223 = 82

frameDuck01 :: Int
frameDuck01 = 83

frameDuck07 :: Int
frameDuck07 = 89

framePain101 :: Int
framePain101 = 90

framePain105 :: Int
framePain105 = 94

framePain201 :: Int
framePain201 = 95

framePain205 :: Int
framePain205 = 99

framePain301 :: Int
framePain301 = 100

framePain321 :: Int
framePain321 = 120

frameStand101 :: Int
frameStand101 = 121

frameStand130 :: Int
frameStand130 = 150

frameStand201 :: Int
frameStand201 = 151

frameStand230 :: Int
frameStand230 = 180

frameWalk01 :: Int
frameWalk01 = 181

frameWalk10 :: Int
frameWalk10 = 190

frameWalk11 :: Int
frameWalk11 = 191

frameWalk20 :: Int
frameWalk20 = 200

spMonsterChick :: Ref EdictT -> Quake ()
spMonsterChick selfRef = do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    proceedSpawnMonsterChick selfRef gameImport

proceedSpawnMonsterChick :: Ref EdictT -> GameImportT -> Quake ()
proceedSpawnMonsterChick selfRef gameImport = do
    soundIndex (Just "chick/chkatck1.wav") >>= (mChickGlobals.mChickSoundMissilePrelaunch .=)
    soundIndex (Just "chick/chkatck2.wav") >>= (mChickGlobals.mChickSoundMissileLaunch    .=)
    soundIndex (Just "chick/chkatck3.wav") >>= (mChickGlobals.mChickSoundMeleeSwing       .=)
    soundIndex (Just "chick/chkatck4.wav") >>= (mChickGlobals.mChickSoundMeleeHit         .=)
    soundIndex (Just "chick/chkatck5.wav") >>= (mChickGlobals.mChickSoundMissileReload    .=)
    soundIndex (Just "chick/chkdeth1.wav") >>= (mChickGlobals.mChickSoundDeath1           .=)
    soundIndex (Just "chick/chkdeth2.wav") >>= (mChickGlobals.mChickSoundDeath2           .=)
    soundIndex (Just "chick/chkfall1.wav") >>= (mChickGlobals.mChickSoundFallDown         .=)
    soundIndex (Just "chick/chkidle1.wav") >>= (mChickGlobals.mChickSoundIdle1            .=)
    soundIndex (Just "chick/chkidle2.wav") >>= (mChickGlobals.mChickSoundIdle2            .=)
    soundIndex (Just "chick/chkpain1.wav") >>= (mChickGlobals.mChickSoundPain1            .=)
    soundIndex (Just "chick/chkpain2.wav") >>= (mChickGlobals.mChickSoundPain2            .=)
    soundIndex (Just "chick/chkpain3.wav") >>= (mChickGlobals.mChickSoundPain3            .=)
    soundIndex (Just "chick/chksght1.wav") >>= (mChickGlobals.mChickSoundSight            .=)
    soundIndex (Just "chick/chksrch1.wav") >>= (mChickGlobals.mChickSoundSearch           .=)
    modelIdx <- modelIndex (Just "models/monsters/bitch/tris.md2")
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeStep
                               & eSolid .~ Constants.solidBbox
                               & eEntityState.esModelIndex .~ modelIdx
                               & eMins .~ V3 (-16) (-16) 0
                               & eMaxs .~ V3 16 16 56
                               & eHealth .~ 175
                               & eGibHealth .~ (-70)
                               & eMass .~ 200
                               & ePain .~ Just chickPain
                               & eDie .~ Just chickDie
                               & eMonsterInfo.miStand .~ Just chickStand
                               & eMonsterInfo.miWalk .~ Just chickWalk
                               & eMonsterInfo.miRun .~ Just chickRun
                               & eMonsterInfo.miDodge .~ Just chickDodge
                               & eMonsterInfo.miAttack .~ Just chickAttack
                               & eMonsterInfo.miMelee .~ Just chickMelee
                               & eMonsterInfo.miSight .~ Just chickSight)
    linkEntity selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStand
                               & eMonsterInfo.miScale .~ modelScale)
    void (entThink GameAI.walkMonsterStart selfRef)
  where
    soundIndex = gameImport^.giSoundIndex
    modelIndex = gameImport^.giModelIndex
    linkEntity = gameImport^.giLinkEntity

chickStand :: EntThink
chickStand = EntThink "chick_stand" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStand)
    return True

chickWalk :: EntThink
chickWalk = EntThink "chick_walk" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveWalk)
    return True

chickRun :: EntThink
chickRun = EntThink "chick_run" $ \selfRef -> do
    action <- pickAction <$> readRef selfRef
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just action)
    return True
  where
    pickAction self
        | shouldStandGround self = chickMoveStand
        | otherwise =
            case self^.eMonsterInfo.miCurrentMove of
                Nothing -> chickMoveStartRun
                Just move -> pickMoveAction move
    shouldStandGround self =
        (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
    pickMoveAction move
        | (move^.mmId) `elem` [chickMoveWalk^.mmId, chickMoveStartRun^.mmId] = chickMoveRun
        | otherwise = chickMoveStartRun

chickDodge :: EntDodge
chickDodge = EntDodge "chick_dodge" $ \selfRef attackerRef _ -> do
    r <- Lib.randomF
    unless (r > 0.25) $ do
        self <- readRef selfRef
        when (isJust (self^.eEnemy)) $ do
            modifyRef selfRef (\v -> v & eEnemy .~ Just attackerRef)
        modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveDuck)

chickAttack :: EntThink
chickAttack = EntThink "chick_attack" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStartAttack1)
    return True

chickMelee :: EntThink
chickMelee = EntThink "chick_melee" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveStartSlash)
    return True

chickSight :: EntInteract
chickSight = EntInteract "chick_sight" $ \selfRef _ -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundSight <- use (mChickGlobals.mChickSoundSight)
    sound (Just selfRef) Constants.chanVoice soundSight 1 Constants.attnNorm 0
    return True

chickPain :: EntPain
chickPain = undefined -- TODO

chickDie :: EntDie
chickDie = undefined -- TODO

chickMoveStand :: MMoveT
chickMoveStand = MMoveT "chickMoveStand" frameStand101 frameStand130 chickFramesStand Nothing

chickFramesStand :: V.Vector MFrameT
chickFramesStand = V.snoc (V.replicate 29 (MFrameT (Just GameAI.aiStand) 0 Nothing))
                          (MFrameT (Just GameAI.aiStand) 0 (Just chickFidget))

chickMoveWalk :: MMoveT
chickMoveWalk = MMoveT "chickMoveWalk" frameWalk11 frameWalk20 chickFramesWalk Nothing

chickFramesWalk :: V.Vector MFrameT
chickFramesWalk =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiWalk) v Nothing) dist)
  where
    dist = [ 6, 8, 13, 5, 7, 4, 11, 5, 9, 7 ]

chickMoveStartRun :: MMoveT
chickMoveStartRun = MMoveT "chickMoveStartRun" frameWalk01 frameWalk10 chickFramesStartRun (Just chickRun)

chickFramesStartRun :: V.Vector MFrameT
chickFramesStartRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dist)
  where
    dist = [ 1, 0, 0, -1, -1, 0, 1, 3, 6, 3 ]

chickMoveDuck :: MMoveT
chickMoveDuck = MMoveT "chickMoveDuck" frameDuck01 frameDuck07 chickFramesDuck (Just chickRun)

chickFramesDuck :: V.Vector MFrameT
chickFramesDuck = V.fromList
    [ MFrameT (Just GameAI.aiMove)   0  (Just chickDuckDown)
    , MFrameT (Just GameAI.aiMove)   1  Nothing
    , MFrameT (Just GameAI.aiMove)   4  (Just chickDuckHold)
    , MFrameT (Just GameAI.aiMove) (-4) Nothing
    , MFrameT (Just GameAI.aiMove) (-5) (Just chickDuckUp)
    , MFrameT (Just GameAI.aiMove)   3  Nothing
    , MFrameT (Just GameAI.aiMove)   1  Nothing
    ]

chickMoveStartAttack1 :: MMoveT
chickMoveStartAttack1 = MMoveT "chickMoveStartAttack1" frameAttack101 frameAttack113 chickFramesStartAttack1 Nothing

chickFramesStartAttack1 :: V.Vector MFrameT
chickFramesStartAttack1 = V.fromList
    [ MFrameT (Just GameAI.aiCharge)   0  (Just chickPreAttack1)
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge) (-3) Nothing
    , MFrameT (Just GameAI.aiCharge)   3  Nothing
    , MFrameT (Just GameAI.aiCharge)   5  Nothing
    , MFrameT (Just GameAI.aiCharge)   7  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  (Just chickAttack1)
    ]

chickMoveStartSlash :: MMoveT
chickMoveStartSlash = MMoveT "chickMoveStartSlash" frameAttack201 frameAttack203 chickFramesStartSlash (Just chickStartSlash)


chickFramesStartSlash :: V.Vector MFrameT
chickFramesStartSlash = V.fromList
    [ MFrameT (Just GameAI.aiCharge) 1 Nothing
    , MFrameT (Just GameAI.aiCharge) 8 Nothing
    , MFrameT (Just GameAI.aiCharge) 3 Nothing
    ]

chickMoveRun :: MMoveT
chickMoveRun = MMoveT "chickMoveRun" frameWalk11 frameWalk20 chickFramesRun Nothing

chickFramesRun :: V.Vector MFrameT
chickFramesRun =
    V.fromList (fmap (\v -> MFrameT (Just GameAI.aiRun) v Nothing) dist)
  where
    dist = [ 6, 8, 13, 5, 7, 4, 11, 5, 9, 7 ]

chickFidget :: EntThink
chickFidget = undefined -- TODO

chickDuckDown :: EntThink
chickDuckDown = undefined -- TODO

chickDuckHold :: EntThink
chickDuckHold = undefined -- TODO

chickDuckUp :: EntThink
chickDuckUp = undefined -- TODO

chickPreAttack1 :: EntThink
chickPreAttack1 = EntThink "Chick_PreAttack1" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundMissilePrelaunch <- use (mChickGlobals.mChickSoundMissilePrelaunch)
    sound (Just selfRef) Constants.chanVoice soundMissilePrelaunch 1 Constants.attnNorm 0
    return True

chickAttack1 :: EntThink
chickAttack1 = EntThink "chick_attack1" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveAttack1)
    return True

chickStartSlash :: EntThink
chickStartSlash = EntThink "chick_slash" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just chickMoveSlash)
    return True

chickMoveAttack1 :: MMoveT
chickMoveAttack1 = MMoveT "chickMoveAttack1" frameAttack114 frameAttack127 chickFramesAttack1 Nothing

chickFramesAttack1 :: V.Vector MFrameT
chickFramesAttack1 = V.fromList
    [ MFrameT (Just GameAI.aiCharge)  19  (Just chickRocket)
    , MFrameT (Just GameAI.aiCharge) (-6) Nothing
    , MFrameT (Just GameAI.aiCharge) (-5) Nothing
    , MFrameT (Just GameAI.aiCharge) (-2) Nothing
    , MFrameT (Just GameAI.aiCharge) (-7) Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)  10  (Just chickReload)
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   5  Nothing
    , MFrameT (Just GameAI.aiCharge)   6  Nothing
    , MFrameT (Just GameAI.aiCharge)   6  Nothing
    , MFrameT (Just GameAI.aiCharge)   4  Nothing
    , MFrameT (Just GameAI.aiCharge)   3  (Just chickReRocket)
    ]

chickMoveSlash :: MMoveT
chickMoveSlash = MMoveT "chickMoveSlash" frameAttack204 frameAttack212 chickFramesSlash Nothing

chickFramesSlash :: V.Vector MFrameT
chickFramesSlash = V.fromList
    [ MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)   7  (Just chickSlash)
    , MFrameT (Just GameAI.aiCharge) (-7) Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge) (-1) Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge)   0  Nothing
    , MFrameT (Just GameAI.aiCharge)   1  Nothing
    , MFrameT (Just GameAI.aiCharge) (-2) (Just chickReSlash)
    ]

chickRocket :: EntThink
chickRocket = undefined -- TODO

chickReload :: EntThink
chickReload = EntThink "ChickReload" $ \selfRef -> do
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundMissileReload <- use (mChickGlobals.mChickSoundMissileReload)
    sound (Just selfRef) Constants.chanVoice soundMissileReload 1 Constants.attnNorm 0
    return True

chickReRocket :: EntThink
chickReRocket = undefined -- TODO

chickSlash :: EntThink
chickSlash = undefined -- TODO

chickReSlash :: EntThink
chickReSlash = undefined -- TODO
