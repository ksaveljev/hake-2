module Game.Monsters.MSoldier
  ( spMonsterSoldier
  , spMonsterSoldierLight
  , spMonsterSoldierSS
  ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~))
import           Control.Monad         (void, when)
import qualified Data.Vector           as V
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI           as GameAI
import qualified Game.GameUtil         as GameUtil
import           Game.MMoveT
import           Game.MonsterInfoT
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib

import {-# SOURCE #-} Game.GameImportT

modelScale :: Float
modelScale = 1.20000

frameAttak101 :: Int
frameAttak101 = 0

frameAttak102 :: Int
frameAttak102 = 1

frameAttak110 :: Int
frameAttak110 = 9

frameAttak112 :: Int
frameAttak112 = 11

frameAttak201 :: Int
frameAttak201 = 12

frameAttak204 :: Int
frameAttak204 = 15

frameAttak216 :: Int
frameAttak216 = 27

frameAttak218 :: Int
frameAttak218 = 29

frameAttak301 :: Int
frameAttak301 = 30

frameAttak303 :: Int
frameAttak303 = 32

frameAttak309 :: Int
frameAttak309 = 38

frameAttak401 :: Int
frameAttak401 = 39

frameAttak406 :: Int
frameAttak406 = 44

frameDuck01 :: Int
frameDuck01 = 45

frameDuck05 :: Int
frameDuck05 = 49

framePain101 :: Int
framePain101 = 50

framePain105 :: Int
framePain105 = 54

framePain201 :: Int
framePain201 = 55

framePain207 :: Int
framePain207 = 61

framePain301 :: Int
framePain301 = 62

framePain318 :: Int
framePain318 = 79

framePain401 :: Int
framePain401 = 80

framePain417 :: Int
framePain417  = 96

frameRun01 :: Int
frameRun01 = 97

frameRun02 :: Int
frameRun02 = 98

frameRun03 :: Int
frameRun03 = 99

frameRun08 :: Int
frameRun08 = 104

frameRuns01 :: Int
frameRuns01 = 109

frameRuns03 :: Int
frameRuns03 = 111

frameRuns14 :: Int
frameRuns14 = 122

frameStand101 :: Int
frameStand101 = 146

frameStand130 :: Int
frameStand130 = 175

frameStand301 :: Int
frameStand301 = 176

frameStand322 :: Int
frameStand322 = 197

frameStand339 :: Int
frameStand339 = 214

frameWalk101 :: Int
frameWalk101 = 215

frameWalk133 :: Int
frameWalk133 = 247

frameWalk209 :: Int
frameWalk209 = 256

frameWalk218 :: Int
frameWalk218 = 265

frameDeath101 :: Int
frameDeath101 = 272

frameDeath136 :: Int
frameDeath136 = 307

frameDeath201 :: Int
frameDeath201 = 308

frameDeath235 :: Int
frameDeath235 = 342

frameDeath301 :: Int
frameDeath301 = 343

frameDeath345 :: Int
frameDeath345 = 387

frameDeath401 :: Int
frameDeath401 = 388

frameDeath453 :: Int
frameDeath453 = 440

frameDeath501 :: Int
frameDeath501 = 441

frameDeath524 :: Int
frameDeath524 = 464

frameDeath601 :: Int
frameDeath601 = 465

frameDeath610 :: Int
frameDeath610 = 474

spMonsterSoldier :: EntThink
spMonsterSoldier = error "MSoldier.spMonsterSoldier" -- TODO

spMonsterSoldierLight :: EntThink
spMonsterSoldierLight = EntThink "SP_monster_soldier_light" $ \selfRef -> do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    monsterSoldierLight selfRef deathmatch
    return True
  where
    monsterSoldierLight selfRef deathmatch
        | deathmatch /= 0 = GameUtil.freeEdict selfRef
        | otherwise = do
            void (entThink spMonsterSoldierX selfRef)
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let soundIndex = gameImport^.giSoundIndex
                modelIndex = gameImport^.giModelIndex
            soundIndex (Just "soldier/solpain2.wav") >>= (mSoldierGlobals.msSoundPainLight .=)
            soundIndex (Just "soldier/soldeth2.wav") >>= (mSoldierGlobals.msSoundDeathLight .=)
            void (modelIndex (Just "models/objects/laser/tris.md2"))
            void (soundIndex (Just "misc/lasfly.wav"))
            void (soundIndex (Just "soldier/solatck2.wav"))
            modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 0
                                       & eHealth .~ 20
                                       & eGibHealth .~ (-30))

spMonsterSoldierSS :: EntThink
spMonsterSoldierSS = error "MSoldier.spMonsterSoldierSS" -- TODO

spMonsterSoldierX :: EntThink
spMonsterSoldierX = EntThink "SP_monster_soldier_x" $ \selfRef -> do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity
    soundIndex (Just "soldier/solidle1.wav") >>= (mSoldierGlobals.msSoundIdle .=)
    soundIndex (Just "soldier/solsght1.wav") >>= (mSoldierGlobals.msSoundSight1 .=)
    soundIndex (Just "soldier/solsrch1.wav") >>= (mSoldierGlobals.msSoundSight2 .=)
    soundIndex (Just "infantry/infatck3.wav") >>= (mSoldierGlobals.msSoundCock .=)
    modelIdx <- modelIndex (Just "models/monsters/soldier/tris.md2")
    modifyRef selfRef (\v -> v & eEntityState.esModelIndex .~ modelIdx
                               & eMonsterInfo.miScale .~ modelScale
                               & eMins .~ V3 (-16) (-16) (-24)
                               & eMaxs .~ V3 16 16 32
                               & eMoveType .~ Constants.moveTypeStep
                               & eSolid .~ Constants.solidBbox
                               & eMass .~ 100
                               & ePain .~ Just soldierPain
                               & eDie  .~ Just soldierDie
                               & eMonsterInfo.miStand  .~ Just soldierStand
                               & eMonsterInfo.miWalk   .~ Just soldierWalk
                               & eMonsterInfo.miRun    .~ Just soldierRun
                               & eMonsterInfo.miDodge  .~ Just soldierDodge
                               & eMonsterInfo.miAttack .~ Just soldierAttack
                               & eMonsterInfo.miMelee  .~ Nothing
                               & eMonsterInfo.miSight  .~ Just soldierSight)
    linkEntity selfRef
    void (entThink soldierStand selfRef)
    void (entThink GameAI.walkMonsterStart selfRef)
    return True

soldierPain :: EntPain
soldierPain = error "MSoldier.soldierPain" -- TODO

soldierDie :: EntDie
soldierDie = error "MSoldier.soldierDie" -- TODO

soldierStand :: EntThink
soldierStand = EntThink "soldier_stand" $ \selfRef -> do
    self <- readRef selfRef
    r <- Lib.randomF
    modifyRef selfRef (\v -> v & eMonsterInfo.miCurrentMove .~ Just (getNextMove r (self^.eMonsterInfo.miCurrentMove)))
    return True
  where
    getNextMove r Nothing
        | r < 0.8   = soldierMoveStand1
        | otherwise = soldierMoveStand3
    getNextMove r (Just move)
        | (move^.mmId) == "soldierMoveStand3" || r < 0.8 = soldierMoveStand1
        | otherwise                                     = soldierMoveStand3

soldierWalk :: EntThink
soldierWalk = error "MSoldier.soldierWalk" -- TODO

soldierRun :: EntThink
soldierRun = error "MSoldier.soldierRun" -- TODO

soldierDodge :: EntDodge
soldierDodge = error "MSoldier.soldierDodge" -- TODO

soldierAttack :: EntThink
soldierAttack = error "MSoldier.soldierAttack" -- TODO

soldierSight :: EntInteract
soldierSight = error "MSoldier.soldierSight" -- TODO

soldierIdle :: EntThink
soldierIdle = EntThink "soldier_idle" $ \selfRef -> do
    r <- Lib.randomF
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundIdle <- use (mSoldierGlobals.msSoundIdle)
    when (r > 0.8) $
        sound (Just selfRef) Constants.chanVoice soundIdle 1 Constants.attnIdle 0
    return True

soldierCock :: EntThink
soldierCock = EntThink "soldier_cock" $ \selfRef -> do
    self <- readRef selfRef
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    soundCock <- use (mSoldierGlobals.msSoundCock)
    sound (Just selfRef) Constants.chanWeapon soundCock 1 (getAttn (self^.eEntityState.esFrame)) 0
    return True
  where
    getAttn frame
        | frame == frameStand322 = Constants.attnIdle
        | otherwise              = Constants.attnNorm

soldierFramesStand1 :: V.Vector MFrameT
soldierFramesStand1 = V.fromList $
    [ MFrameT (Just GameAI.aiStand) 0 (Just soldierIdle) ]
    ++ replicate 29 (MFrameT (Just GameAI.aiStand) 0 Nothing)

soldierMoveStand1 :: MMoveT
soldierMoveStand1 = MMoveT "soldierMoveStand1" frameStand101 frameStand130 soldierFramesStand1 (Just soldierStand)

soldierFramesStand3 :: V.Vector MFrameT
soldierFramesStand3 = V.fromList $
    replicate 21 (MFrameT (Just GameAI.aiStand) 0 Nothing)
    ++ [ MFrameT (Just GameAI.aiStand) 0 (Just soldierCock) ]
    ++ replicate 17 (MFrameT (Just GameAI.aiStand) 0 Nothing)

soldierMoveStand3 :: MMoveT
soldierMoveStand3 = MMoveT "soldierMoveStand3" frameStand301 frameStand339 soldierFramesStand3 (Just soldierStand)
