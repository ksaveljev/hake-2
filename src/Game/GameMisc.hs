module Game.GameMisc
    ( becomeExplosion1
    , spFuncAreaPortal
    , spFuncClock
    , spFuncExplosive
    , spFuncObject
    , spFuncWall
    , spInfoNotNull
    , spInfoNull
    , spLight
    , spLightMine1
    , spLightMine2
    , spMiscBanner
    , spMiscBigViper
    , spMiscBlackHole
    , spMiscDeadSoldier
    , spMiscEasterChick
    , spMiscEasterChick2
    , spMiscEasterTank
    , spMiscExploBox
    , spMiscGibArm
    , spMiscGibHead
    , spMiscGibLeg
    , spMiscSatelliteDish
    , spMiscStroggShip
    , spMiscTeleporter
    , spMiscTeleporterDest
    , spMiscViper
    , spMiscViperBomb
    , spMonsterCommanderBody
    , spPathCorner
    , spPointCombat
    , spTargetCharacter
    , spTargetString
    , spViewThing
    , throwGib
    , throwHead
    ) where

import           Control.Lens          (use, (^.), (&), (.~), (%~), (+~))
import           Control.Monad         (replicateM_, unless, when, void)
import           Data.Bits             (complement, xor, (.|.), (.&.))
import qualified Data.ByteString       as B
import           Data.Maybe            (isJust, isNothing)
import           Linear                (V3(..), _y, _z)

import qualified Client.M              as M
import qualified Constants
import           Game.CPlaneT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameCombat       as GameCombat
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import           Game.MonsterInfoT
import           Game.MoveInfoT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary           (encode)
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

import {-# SOURCE #-} qualified Game.GameBase as GameBase
import {-# SOURCE #-} qualified Game.GameFunc as GameFunc

startOff :: Int
startOff = 1

spFuncAreaPortal :: EntThink
spFuncAreaPortal = EntThink "sp_func_areaportal" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eUse .~ Just useAreaPortal
                                & eCount .~ 0) -- always start closed
    return True

spFuncClock :: Ref EdictT -> Quake ()
spFuncClock selfRef = do
    self <- readRef selfRef
    doSpawnFuncClock self
  where
    doSpawnFuncClock self
        | isNothing (self^.eTarget) = do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf (B.concat [self^.eClassName, " with no target at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
            GameUtil.freeEdict selfRef
        | (self^.eSpawnFlags) .&. 2 /= 0 && (self^.eCount) == 0 = do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf (B.concat [self^.eClassName, " with no count at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
            GameUtil.freeEdict selfRef
        | otherwise = do
            when ((self^.eSpawnFlags) .&. 1 /= 0 && (self^.eCount) == 0) $
                modifyRef selfRef (\v -> v & eCount .~ 60 * 60)
            funcClockReset selfRef
            modifyRef selfRef (\v -> v & eMessage .~ Just B.empty
                                       & eThink .~ Just funcClockThink)
            updateSelf =<< readRef selfRef
    updateSelf self
        | (self^.eSpawnFlags) .&. 4 /= 0 =
            modifyRef selfRef (\v -> v & eUse .~ Just funcClockUse)
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eNextThink .~ levelTime + 1)

funcClockReset :: Ref EdictT -> Quake ()
funcClockReset selfRef = do
    self <- readRef selfRef
    doReset self
  where
    doReset self
        | (self^.eSpawnFlags) .&. 1 /= 0 =
            modifyRef selfRef (\v -> v & eActivator .~ Nothing
                                       & eHealth .~ 0
                                       & eWait .~ fromIntegral (self^.eCount))
        | otherwise =
            modifyRef selfRef (\v -> v & eActivator .~ Nothing
                                       & eHealth .~ (self^.eCount)
                                       & eWait .~ 0)

funcClockThink :: EntThink
funcClockThink = error "GameMisc.funcClockThink" -- TODO

funcClockUse :: EntUse
funcClockUse = EntUse "func_clock_use" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef
    when ((self^.eSpawnFlags) .&. 8 == 0) $
        modifyRef selfRef (\v -> v & eUse .~ Nothing)
    when (isNothing (self^.eActivator)) $ do
        modifyRef selfRef (\v -> v & eActivator .~ activatorRef)
        maybe thinkError (doThink selfRef) (self^.eThink)
  where
    thinkError = Com.fatalError "GameMisc.funcClockUse self^.eThink is Nothing"
    doThink selfRef think = void (entThink think selfRef)

spFuncExplosive :: Ref EdictT -> Quake ()
spFuncExplosive edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnFuncExplosive deathmatch
  where
    doSpawnFuncExplosive deathmatch
        | deathmatch /= 0 = -- auto-remove for deathmatch
            GameUtil.freeEdict edictRef
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush)
            void ((gameImport^.giModelIndex) (Just "models/objects/debris1/tris.md2"))
            void ((gameImport^.giModelIndex) (Just "models/objects/debris2/tris.md2"))
            edict <- readRef edictRef
            (gameImport^.giSetModel) edictRef (edict^.eiModel)
            setSolid edict
            when ((edict^.eSpawnFlags) .&. 2 /= 0) $
                modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))
            when ((edict^.eSpawnFlags) .&. 4 /= 0) $
                modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))
            updatedEdict <- readRef edictRef
            checkEdictUse updatedEdict (updatedEdict^.eUse)
            (gameImport^.giLinkEntity) edictRef
    setSolid edict
        | (edict^.eSpawnFlags) .&. 1 /= 0 =
            modifyRef edictRef (\v -> v & eSvFlags %~ (.|. Constants.svfNoClient)
                                        & eSolid .~ Constants.solidNot
                                        & eUse .~ Just funcExplosiveSpawn)
        | otherwise = do
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidBsp)
            when (isJust (edict^.eTargetName)) $
                modifyRef edictRef (\v -> v & eUse .~ Just funcExplosiveUse)
    checkEdictUse _ (Just (EntUse "func_explosive_use" _)) = return ()
    checkEdictUse edict _ = do
        when ((edict^.eHealth) == 0) $
            modifyRef edictRef (\v -> v & eHealth .~ 100)
        modifyRef edictRef (\v -> v & eDie .~ Just funcExplosiveExplode
                                    & eTakeDamage .~ Constants.damageYes)

funcExplosiveSpawn :: EntUse
funcExplosiveSpawn = EntUse "func_explosive_spawn" $ \selfRef _ _ -> do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp
                               & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                               & eUse .~ Nothing)
    void (GameUtil.killBox selfRef)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef

funcExplosiveUse :: EntUse
funcExplosiveUse = EntUse "func_explosive_use" $ \selfRef otherRef _ -> do
    self <- readRef selfRef
    v3o <- use (globals.gVec3Origin)
    maybe otherError (doFuncExplosiveUse selfRef self v3o) otherRef
  where
    otherError = Com.fatalError "GameMisc.funcExplosiveUse otherRef is Nothing"
    doFuncExplosiveUse selfRef self v3o otherRef =
        entDie funcExplosiveExplode selfRef selfRef otherRef (self^.eHealth) v3o

funcExplosiveExplode :: EntDie
funcExplosiveExplode = error "GameMisc.funcExplosiveExplode" -- TODO

spFuncObject :: Ref EdictT -> Quake ()
spFuncObject selfRef = do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giSetModel) selfRef (self^.eiModel)
    doSpawnFuncObject selfRef self
    (gameImport^.giLinkEntity) selfRef

doSpawnFuncObject :: Ref EdictT -> EdictT -> Quake ()
doSpawnFuncObject selfRef self
    | (self^.eSpawnFlags) == 0 = do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eMins .~ mins
                                   & eMaxs .~ maxs
                                   & eDmg .~ dmg
                                   & eEntityState.esEffects .~ effects'
                                   & eClipMask .~ Constants.maskMonsterSolid
                                   & eSolid .~ Constants.solidBsp
                                   & eMoveType .~ Constants.moveTypePush
                                   & eThink .~ Just funcObjectRelease
                                   & eNextThink .~ levelTime + 2 * Constants.frameTime)
    | otherwise =
        modifyRef selfRef (\v -> v & eMins .~ mins
                                   & eMaxs .~ maxs
                                   & eDmg .~ dmg
                                   & eEntityState.esEffects .~ effects'
                                   & eClipMask .~ Constants.maskMonsterSolid
                                   & eSolid .~ Constants.solidNot
                                   & eMoveType .~ Constants.moveTypePush
                                   & eUse .~ Just funcObjectUse
                                   & eSvFlags %~ (.|. Constants.svfNoClient))
  where
    mins = fmap (+ 1) (self^.eMins)
    maxs = fmap (subtract 1) (self^.eMaxs)
    dmg = if (self^.eDmg) == 0 then 100 else self^.eDmg
    effects = if (self^.eSpawnFlags) .&. 2 /= 0 then (self^.eEntityState.esEffects) .|. Constants.efAnimAll else self^.eEntityState.esEffects
    effects' = if (self^.eSpawnFlags) .&. 4 /= 0 then (self^.eEntityState.esEffects) .|. Constants.efAnimAllFast else effects

funcObjectRelease :: EntThink
funcObjectRelease = EntThink "func_object_release" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeToss
                               & eTouch .~ Just funcObjectTouch)
    return True

funcObjectUse :: EntUse
funcObjectUse = EntUse "func_object_use" $ \selfRef _ _ -> do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp
                               & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                               & eUse .~ Nothing)
    void (GameUtil.killBox selfRef)
    void (entThink funcObjectRelease selfRef)

funcObjectTouch :: EntTouch
funcObjectTouch = EntTouch "func_object_touch" $ \selfRef otherRef plane _ -> do
    -- TODO: jake2 checks if (plane == null)
    self <- readRef selfRef
    other <- readRef otherRef
    unless ((plane^.cpNormal._z) < 1.0 || (other^.eTakeDamage) == Constants.damageNo) $ do
        v3o <- use (globals.gVec3Origin)
        GameCombat.damage otherRef selfRef selfRef v3o (self^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

spFuncWall :: Ref EdictT -> Quake ()
spFuncWall edictRef = do
    setEdictModel =<< readRef edictRef
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush)
    updateEdictEffects
    isAWall <- checkWall
    doSpawnFuncWall isAWall
  where
    setEdictModel edict = do
        setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
        setModel edictRef (edict^.eiModel)
    doSpawnFuncWall isAWall
        | isAWall = do
            -- just a wall
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidBsp)
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef
        | otherwise = do
            -- it must be TRIGGER_SPAWN
            checkTriggerSpawn
            -- yell if the spawnflags are odd
            checkOddSpawnFlags
            modifyRef edictRef (\v -> v & eUse .~ Just funcWallUse)
            edict <- readRef edictRef
            setSolid edict
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef
    setSolid edict
        | (edict^.eSpawnFlags) .&. 4 /= 0 =
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidBsp)
        | otherwise =
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidNot
                                        & eSvFlags %~ (.|. Constants.svfNoClient))
    updateEdictEffects = do
        edict <- readRef edictRef
        when ((edict^.eSpawnFlags) .&. 8 /= 0) $
            modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))
        when ((edict^.eSpawnFlags) .&. 16 /= 0) $
            modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))
    checkWall = do
        edict <- readRef edictRef
        return ((edict^.eSpawnFlags) .&. 7 == 0)
    checkTriggerSpawn = do
        edict <- readRef edictRef
        when ((edict^.eSpawnFlags) .&. 1 == 0) $ do
          dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
          dprintf "func_wall missing TRIGGER_SPAWN\n"
          modifyRef edictRef (\v -> v & eSpawnFlags %~ (.|. 1))
    checkOddSpawnFlags = do
        edict <- readRef edictRef
        when ((edict^.eSpawnFlags) .&. 4 /= 0 && (edict^.eSpawnFlags) .&. 2 == 0) $ do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf "func_wall START_ON without TOGGLE\n"
            modifyRef edictRef (\v -> v & eSpawnFlags %~ (.|. 2))

funcWallUse :: EntUse
funcWallUse = EntUse "func_wall_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    setSolid selfRef self
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    updatedSelf <- readRef selfRef
    when ((updatedSelf^.eSpawnFlags) .&. 2 == 0) $
        modifyRef selfRef (\v -> v & eUse .~ Nothing)
  where
    setSolid selfRef self
        | (self^.eSolid) == Constants.solidNot = do
            modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp
                                       & eSvFlags %~ (.&. (complement Constants.svfNoClient)))
            void (GameUtil.killBox selfRef)
        | otherwise =
            modifyRef selfRef (\v -> v & eSolid .~ Constants.solidNot
                                       & eSvFlags %~ (.|. Constants.svfNoClient))

spInfoNotNull :: Ref EdictT -> Quake ()
spInfoNotNull edictRef = do
    edict <- readRef edictRef
    modifyRef edictRef (\v -> v & eAbsMin .~ (edict^.eEntityState.esOrigin)
                                & eAbsMax .~ (edict^.eEntityState.esOrigin))

spInfoNull :: Ref EdictT -> Quake ()
spInfoNull = GameUtil.freeEdict

spLight :: Ref EdictT -> Quake ()
spLight edictRef = do
    -- no targeted lights in deathmatch, because they cause global messages
    edict <- readRef edictRef
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnLight edict deathmatch
  where
    doSpawnLight edict deathmatch
        | isNothing (edict^.eTargetName) || deathmatch /= 0 =
            GameUtil.freeEdict edictRef
        | otherwise =
            when ((edict^.eStyle) >= 32) $ do
                modifyRef edictRef (\v -> v & eUse .~ Just lightUse)
                configString <- use (gameBaseGlobals.gbGameImport.giConfigString)
                configString (Constants.csLights + (edict^.eStyle)) (lightStyle edict)
    lightStyle edict
        | (edict^.eSpawnFlags) .&. startOff /= 0 = "a"
        | otherwise                              = "m"

lightUse :: EntUse
lightUse = EntUse "light_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    configString <- use (gameBaseGlobals.gbGameImport.giConfigString)
    doLightUse selfRef self configString
  where
    doLightUse selfRef self configString
        | (self^.eSpawnFlags) .&. startOff /= 0 = do
            void (configString (Constants.csLights + (self^.eStyle)) "m")
            modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement startOff)))
        | otherwise = do
            void (configString (Constants.csLights + (self^.eStyle)) "a")
            modifyRef selfRef (\v -> v & eSpawnFlags %~ (.|. startOff))

spLightMine1 :: Ref EdictT -> Quake ()
spLightMine1 edictRef = do
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/objects/minelite/light1/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eEntityState.esModelIndex .~ modelIdx)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

spLightMine2 :: Ref EdictT -> Quake ()
spLightMine2 edictRef = do
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/objects/minelite/light2/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eEntityState.esModelIndex .~ modelIdx)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

spMiscBanner :: Ref EdictT -> Quake ()
spMiscBanner edictRef = do
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/objects/banner/tris.md2")
    r <- Lib.rand
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidNot
                                & eEntityState.esModelIndex .~ modelIdx
                                & eEntityState.esFrame .~ (fromIntegral r) `mod` 16)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef
    modifyRef edictRef (\v -> v & eThink .~ Just miscBannerThink
                                & eNextThink .~ levelTime + Constants.frameTime)

miscBannerThink :: EntThink
miscBannerThink = EntThink "misc_banner_think" $ \edictRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eEntityState.esFrame %~ (`mod` 16) . (+ 1)
                                & eNextThink .~ levelTime + Constants.frameTime)
    return True

spMiscBigViper :: Ref EdictT -> Quake ()
spMiscBigViper edictRef = do
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/ships/bigviper/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eMins .~ V3 (-176) (-120) (-24)
                                & eMaxs .~ V3 176 120 72
                                & eEntityState.esModelIndex .~ modelIdx)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

spMiscBlackHole :: Ref EdictT -> Quake ()
spMiscBlackHole edictRef = do
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/objects/black/tris.md2")
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidNot
                                & eMins .~ V3 (-64) (-64) 0
                                & eMaxs .~ V3 64 64 8
                                & eEntityState.esModelIndex .~ modelIdx
                                & eEntityState.esRenderFx .~ Constants.rfTranslucent
                                & eUse .~ Just miscBlackHoleUse
                                & eThink .~ Just miscBlackHoleThink
                                & eNextThink .~ levelTime + 2 * Constants.frameTime)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

miscBlackHoleUse :: EntUse
miscBlackHoleUse = EntUse "misc_blackhole_use" $ \edictRef _ _ ->
    GameUtil.freeEdict edictRef

miscBlackHoleThink :: EntThink
miscBlackHoleThink = EntThink "misc_blackhole_think" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eEntityState.esFrame %~ (\vv -> if vv + 1 < 19 then vv + 1 else 0)
                               & eNextThink .~ levelTime + Constants.frameTime)
    return True

spMiscDeadSoldier :: Ref EdictT -> Quake ()
spMiscDeadSoldier edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    miscDeadSoldier deathmatch
  where
    miscDeadSoldier deathmatch
        | deathmatch /= 0 =
            GameUtil.freeEdict edictRef
        | otherwise = do
            modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
            modelIdx <- modelIndex (Just "models/deadbods/dude/tris.md2")
            modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                        & eSolid .~ Constants.solidBbox
                                        & eEntityState.esModelIndex .~ modelIdx)
            frame <- fmap getFrame (readRef edictRef)
            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ frame
                                        & eMins .~ V3 (-16) (-16) 0
                                        & eMaxs .~ V3 16 16 16
                                        & eDeadFlag .~ Constants.deadDead
                                        & eTakeDamage .~ Constants.damageYes
                                        & eSvFlags %~ (.|. (Constants.svfMonster .|. Constants.svfDeadMonster))
                                        & eDie .~ Just miscDeadSoldierDie
                                        & eMonsterInfo.miAIFlags %~ (.|. Constants.aiGoodGuy))
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef
    getFrame edict =
        let spawnFlags = edict^.eSpawnFlags
            -- defaults to frame 0
            frame | spawnFlags .&. 2 /= 0  = 1
                  | spawnFlags .&. 4 /= 0  = 2
                  | spawnFlags .&. 8 /= 0  = 3
                  | spawnFlags .&. 16 /= 0 = 4
                  | spawnFlags .&. 32 /= 0 = 5
                  | otherwise              = 0
        in frame

miscDeadSoldierDie :: EntDie
miscDeadSoldierDie = EntDie "misc_deadsoldier_die" $ \selfRef _ _ damage _ -> do
    self <- readRef selfRef
    unless ((self^.eHealth) > (-80)) $ do
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        soundIdx <- soundIndex (Just "misc/udeath.wav")
        sound <- use (gameBaseGlobals.gbGameImport.giSound)
        sound (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0
        replicateM_ 4 (throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic)
        throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic

spMiscEasterChick :: Ref EdictT -> Quake ()
spMiscEasterChick edictRef = do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/monsters/bitch/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eMins .~ V3 (-32) (-32) 0
                                & eMaxs .~ V3 32 32 32
                                & eEntityState.esModelIndex .~ modelIdx
                                & eEntityState.esFrame .~ 208
                                & eThink .~ Just miscEasterChickThink
                                & eNextThink .~ levelTime + 2 * Constants.frameTime)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

miscEasterChickThink :: EntThink
miscEasterChickThink = EntThink "misc_easterchick_think" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eEntityState.esFrame %~ (\vv -> if vv + 1 < 247 then vv + 1 else 208)
                               & eNextThink .~ levelTime + Constants.frameTime)
    return True

spMiscEasterChick2 :: Ref EdictT -> Quake ()
spMiscEasterChick2 edictRef = do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/monsters/bitch/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eMins .~ V3 (-32) (-32) 0
                                & eMaxs .~ V3 32 32 32
                                & eEntityState.esModelIndex .~ modelIdx
                                & eEntityState.esFrame .~ 248
                                & eThink .~ Just miscEasterChick2Think
                                & eNextThink .~ levelTime + 2 * Constants.frameTime)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

miscEasterChick2Think :: EntThink
miscEasterChick2Think = EntThink "misc_easterchick2_think" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eEntityState.esFrame %~ (\vv -> if vv + 1 < 287 then vv + 1 else 248)
                               & eNextThink .~ levelTime + Constants.frameTime)
    return True

spMiscEasterTank :: Ref EdictT -> Quake ()
spMiscEasterTank edictRef = do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/monsters/tank/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eMins .~ V3 (-32) (-32) (-16)
                                & eMaxs .~ V3 32 32 32
                                & eEntityState.esModelIndex .~ modelIdx
                                & eEntityState.esFrame .~ 254
                                & eThink .~ Just miscEasterTankThink
                                & eNextThink .~ levelTime + 2 * Constants.frameTime)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

miscEasterTankThink :: EntThink
miscEasterTankThink = EntThink " misc_eastertank_think" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eEntityState.esFrame %~ (\vv -> if vv + 1 < 293 then vv + 1 else 254)
                               & eNextThink .~ levelTime + Constants.frameTime)
    return True

spMiscExploBox :: Ref EdictT -> Quake ()
spMiscExploBox edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    miscExploBox deathmatch
  where
    trisModel = "models/objects/barrels/tris.md2"
    miscExploBox deathmatch
        | deathmatch /= 0 =
            GameUtil.freeEdict edictRef
        | otherwise = do
            modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
            void (modelIndex (Just "models/objects/debris1/tris.md2"))
            void (modelIndex (Just "models/objects/debris2/tris.md2"))
            void (modelIndex (Just "models/objects/debris3/tris.md2"))
            tris <- modelIndex (Just trisModel)
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidBbox
                                        & eMoveType .~ Constants.moveTypeStep
                                        & eiModel .~ Just trisModel
                                        & eEntityState.esModelIndex .~ tris
                                        & eMins .~ V3 (-16) (-16) 0
                                        & eMaxs .~ V3 16 16 40
                                        & eDie .~ Just barrelDelay
                                        & eTakeDamage .~ Constants.damageYes
                                        & eMonsterInfo.miAIFlags .~ Constants.aiNoStep
                                        & eTouch .~ Just barrelTouch
                                        & eThink .~ Just M.dropToFloor
                                        & eNextThink .~ levelTime + 2 * Constants.frameTime)
            edict <- readRef edictRef
            when ((edict^.eMass) == 0) $
                modifyRef edictRef (\v -> v & eMass .~ 400)
            when ((edict^.eHealth) == 0) $
                modifyRef edictRef (\v -> v & eHealth .~ 10)
            when ((edict^.eDmg) == 0) $
                modifyRef edictRef (\v -> v & eDmg .~ 150)
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef

barrelDelay :: EntDie
barrelDelay = EntDie "barrel_delay" $ \selfRef attackerRef _ _ _ -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageNo
                               & eNextThink .~ levelTime + 2 * Constants.frameTime
                               & eThink .~ Just barrelExplode
                               & eActivator .~ Just attackerRef)

barrelExplode :: EntThink
barrelExplode = EntThink "barrel_explode" $ \selfRef -> do
    error "GameMisc.barrelExplode" -- TODO

barrelTouch :: EntTouch
barrelTouch = EntTouch "barrel_touch" $ \selfRef otherRef _ _ -> do
    other <- readRef otherRef
    unless (isNothing (other^.eGroundEntity) || (other^.eGroundEntity) == Just selfRef) $ do
        self <- readRef selfRef
        let ratio = fromIntegral (other^.eMass) / fromIntegral (self^.eMass)
            v = (self^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)
        void (M.walkMove selfRef (Math3D.vectorYaw v) (20 * ratio * Constants.frameTime))

spMiscGibArm :: Ref EdictT -> Quake ()
spMiscGibArm edictRef = do
    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eSolid .~ Constants.solidNot
                                & eEntityState.esEffects %~ (.|. Constants.efGib)
                                & eTakeDamage .~ Constants.damageYes
                                & eDie .~ Just gibDie
                                & eMoveType .~ Constants.moveTypeToss
                                & eSvFlags %~ (.|. Constants.svfMonster)
                                & eDeadFlag .~ Constants.deadDead
                                & eAVelocity .~ V3 (r1 * 200) (r2 * 200) (r3 * 200)
                                & eThink .~ Just GameUtil.freeEdictA
                                & eNextThink .~ levelTime + 30)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel edictRef (Just "models/objects/gibs/arm/tris.md2")
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

spMiscGibHead :: Ref EdictT -> Quake ()
spMiscGibHead edictRef = do
    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eSolid .~ Constants.solidNot
                                & eEntityState.esEffects %~ (.|. Constants.efGib)
                                & eTakeDamage .~ Constants.damageYes
                                & eDie .~ Just gibDie
                                & eMoveType .~ Constants.moveTypeToss
                                & eSvFlags %~ (.|. Constants.svfMonster)
                                & eDeadFlag .~ Constants.deadDead
                                & eAVelocity .~ V3 (r1 * 200) (r2 * 200) (r3 * 200)
                                & eThink .~ Just GameUtil.freeEdictA
                                & eNextThink .~ levelTime + 30)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel edictRef (Just "models/objects/gibs/head/tris.md2")
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

spMiscGibLeg :: Ref EdictT -> Quake ()
spMiscGibLeg edictRef = do
    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eSolid .~ Constants.solidNot
                                & eEntityState.esEffects %~ (.|. Constants.efGib)
                                & eTakeDamage .~ Constants.damageYes
                                & eDie .~ Just gibDie
                                & eMoveType .~ Constants.moveTypeToss
                                & eSvFlags %~ (.|. Constants.svfMonster)
                                & eDeadFlag .~ Constants.deadDead
                                & eAVelocity .~ V3 (r1 * 200) (r2 * 200) (r3 * 200)
                                & eThink .~ Just GameUtil.freeEdictA
                                & eNextThink .~ levelTime + 30)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel edictRef (Just "models/objects/gibs/leg/tris.md2")
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef

spMiscSatelliteDish :: Ref EdictT -> Quake ()
spMiscSatelliteDish edictRef = do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    modelIdx <- (gameImport^.giModelIndex) (Just "models/objects/satellite/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eMins .~ V3 (-64) (-64) 0
                                & eMaxs .~ V3 64 64 128
                                & eEntityState.esModelIndex .~ modelIdx
                                & eUse .~ Just miscSatelliteDishUse)
    (gameImport^.giLinkEntity) edictRef

miscSatelliteDishUse :: EntUse
miscSatelliteDishUse = EntUse "misc_satellite_dish_use" $ \selfRef _ _ -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eEntityState.esFrame .~ 0
                               & eThink .~ Just miscSatelliteDishThink
                               & eNextThink .~ levelTime + Constants.frameTime)

miscSatelliteDishThink :: EntThink
miscSatelliteDishThink = EntThink "misc_satellite_dish_think" $ \selfRef -> do
    self <- readRef selfRef
    doThink selfRef self
    return True
  where
    doThink selfRef self 
        | (self^.eEntityState.esFrame) + 1 < 38 = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eEntityState.esFrame +~ 1
                                       & eNextThink .~ levelTime + Constants.frameTime)
        | otherwise =
            modifyRef selfRef (\v -> v & eEntityState.esFrame +~ 1)

spMiscStroggShip :: Ref EdictT -> Quake ()
spMiscStroggShip edictRef = do
    edict <- readRef edictRef
{-
    let dprintf = gameImport^.giDprintf
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity
-}
    maybe (noTarget edict) (targetAvailable edict) (edict^.eTarget)
  where
    noTarget edict = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat [edict^.eClassName, " without a target at ", Lib.vtos (edict^.eAbsMin), "\n"])
        GameUtil.freeEdict edictRef
    targetAvailable edict _ = do
        gameImport <- use (gameBaseGlobals.gbGameImport)
        modelIdx <- (gameImport^.giModelIndex) (Just "models/ships/strogg1/tris.md2")
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                    & eSolid .~ Constants.solidNot
                                    & eEntityState.esModelIndex .~ modelIdx
                                    & eMins .~ V3 (-16) (-16) 0
                                    & eMaxs .~ V3 16 16 32
                                    & eThink .~ Just (GameFunc.funcTrainFind)
                                    & eNextThink .~ levelTime + Constants.frameTime
                                    & eUse .~ Just (miscStroggShipUse)
                                    & eSvFlags %~ (.|. Constants.svfNoClient)
                                    & eMoveInfo.miAccel .~ (edict^.eSpeed)
                                    & eMoveInfo.miDecel .~ (edict^.eSpeed)
                                    & eMoveInfo.miSpeed .~ (edict^.eSpeed))
        (gameImport^.giLinkEntity) edictRef

miscStroggShipUse :: EntUse
miscStroggShipUse = EntUse "misc_strogg_ship_use" $ \selfRef otherRef activatorRef -> do
    modifyRef selfRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                               & eUse .~ Just GameFunc.trainUse)
    entUse GameFunc.trainUse selfRef otherRef activatorRef

spMiscTeleporter :: Ref EdictT -> Quake ()
spMiscTeleporter = error "GameMisc.spMiscTeleporter" -- TODO

spMiscTeleporterDest :: EntThink
spMiscTeleporterDest = EntThink "SP_misc_teleporter_dest" $ \edictRef -> do
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel edictRef (Just "models/objects/dmspot/tris.md2")
    modifyRef edictRef (\v -> v & eEntityState.esSkinNum .~ 0
                                & eSolid .~ Constants.solidBbox
                                & eMins .~ V3 (-32) (-32) (-24)
                                & eMaxs .~ V3 32 32 (-16))
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef
    return True

spMiscViper :: Ref EdictT -> Quake ()
spMiscViper edictRef = do
    edict <- readRef edictRef
    doSpawnMiscViper edict
  where
    doSpawnMiscViper edict
        | isNothing (edict^.eTarget) = do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf (B.concat ["misc_viper without a target at ", Lib.vtos (edict^.eAbsMin), "\n"])
            GameUtil.freeEdict edictRef
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            modelIdx <- (gameImport^.giModelIndex) (Just "models/ships/viper/tris.md2")
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            let speed = if (edict^.eSpeed) == 0 then 300 else edict^.eSpeed
            modifyRef edictRef (\v -> v & eSpeed .~ speed
                                        & eMoveType .~ Constants.moveTypePush
                                        & eSolid .~ Constants.solidNot
                                        & eEntityState.esModelIndex .~ modelIdx
                                        & eMins .~ V3 (-16) (-16) 0
                                        & eMaxs .~ V3 16 16 32
                                        & eThink .~ Just GameFunc.funcTrainFind
                                        & eNextThink .~ levelTime + Constants.frameTime
                                        & eUse .~ Just miscViperUse
                                        & eSvFlags %~ (.|. Constants.svfNoClient)
                                        & eMoveInfo.miAccel .~ speed
                                        & eMoveInfo.miDecel .~ speed
                                        & eMoveInfo.miSpeed .~ speed)
            (gameImport^.giLinkEntity) edictRef

miscViperUse :: EntUse
miscViperUse = EntUse "misc_viper_use" $ \selfRef otherRef activatoRef -> do
    modifyRef selfRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                               & eUse .~ Just GameFunc.trainUse)
    entUse (GameFunc.trainUse) selfRef otherRef activatoRef

spMiscViperBomb :: Ref EdictT -> Quake ()
spMiscViperBomb selfRef = do
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/objects/bomb/tris.md2")
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                               & eSolid .~ Constants.solidNot
                               & eMins .~ V3 (-8) (-8) (-8)
                               & eMaxs .~ V3 8 8 8
                               & eEntityState.esModelIndex .~ modelIdx
                               & eDmg %~ (\vv -> if vv == 0 then 1000 else vv)
                               & eUse .~ Just miscViperBombUse
                               & eSvFlags %~ (.|. Constants.svfNoClient))
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef

miscViperBombUse :: EntUse
miscViperBombUse = EntUse "misc_viper_bomb_use" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBbox
                               & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                               & eEntityState.esEffects %~ (.|. Constants.efRocket)
                               & eUse .~ Nothing
                               & eMoveType .~ Constants.moveTypeToss
                               & ePrethink .~ Just miscViperBombPrethink
                               & eTouch .~ Just miscViperBombTouch
                               & eActivator .~ activatorRef)
    es <- GameBase.gFind Nothing GameBase.findByClass "misc_viper"
    maybe (return ()) (setViperInfo selfRef) es
  where
    setViperInfo selfRef viperRef = do
        viper <- readRef viperRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eVelocity .~ fmap (* (viper^.eMoveInfo.miSpeed)) (viper^.eMoveInfo.miDir)
                                   & eTimeStamp .~ levelTime
                                   & eMoveInfo.miDir .~ (viper^.eMoveInfo.miDir))

miscViperBombPrethink :: EntThink
miscViperBombPrethink = EntThink "misc_viper_bomb_prethink" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    let diff = (self^.eTimeStamp) - levelTime
        diff' = max (-1.0) diff
        V3 a b c = fmap (* (1.0 + diff')) (self^.eMoveInfo.miDir)
        V3 a' b' c' = Math3D.vectorAngles (V3 a b diff')
    modifyRef selfRef (\v -> v & eGroundEntity .~ Nothing
                               & eEntityState.esAngles .~ V3 a' b' ((self^.eEntityState.esAngles._z) + 10))
    return True

miscViperBombTouch :: EntTouch
miscViperBombTouch = EntTouch "misc_viper_bomb_touch" $ \selfRef _ _ _ -> do
    self <- readRef selfRef
    GameUtil.useTargets selfRef (self^.eActivator)
    modifyRef selfRef (\v -> v & eEntityState.esOrigin._z .~ (self^.eAbsMin._z) + 1)
    GameCombat.radiusDamage selfRef selfRef (fromIntegral (self^.eDmg)) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modBomb
    becomeExplosion2 selfRef

spMonsterCommanderBody :: Ref EdictT -> Quake ()
spMonsterCommanderBody selfRef = do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modelIdx <- (gameImport^.giModelIndex) (Just "models/monsters/commandr/tris.md2")
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                               & eSolid .~ Constants.solidBbox
                               & eiModel .~ Just "models/monsters/commandr/tris.md2"
                               & eEntityState.esModelIndex .~ modelIdx
                               & eMins .~ V3 (-32) (-32) 0
                               & eMaxs .~ V3 32 32 48
                               & eUse .~ Just commanderBodyUse
                               & eTakeDamage .~ Constants.damageYes
                               & eFlags .~ Constants.flGodMode
                               & eEntityState.esRenderFx %~ (.|. Constants.rfFrameLerp))
    (gameImport^.giLinkEntity) selfRef
    void ((gameImport^.giSoundIndex) (Just "tank/thud.wav"))
    void ((gameImport^.giSoundIndex) (Just "tank/pain.wav"))
    modifyRef selfRef (\v -> v & eThink .~ Just commanderBodyDrop
                               & eNextThink .~ levelTime + 5 * Constants.frameTime)

commanderBodyUse :: EntUse
commanderBodyUse = EntUse "commander_body_use" $ \selfRef _ _ -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    gameImport <- use (gameBaseGlobals.gbGameImport)
    soundIdx <- (gameImport^.giSoundIndex) (Just "tank/pain.wav")
    modifyRef selfRef (\v -> v & eThink .~ Just commanderBodyThink
                               & eNextThink .~ levelTime + Constants.frameTime)
    (gameImport^.giSound) (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

commanderBodyThink :: EntThink
commanderBodyThink = EntThink "commander_body_think" $ \selfRef -> do
    doThink selfRef =<< readRef selfRef
    self <- readRef selfRef
    when ((self^.eEntityState.esFrame) + 1 == 22) $ do
        gameImport <- use (gameBaseGlobals.gbGameImport)
        soundIdx <- (gameImport^.giSoundIndex) (Just "tank/thud.wav")
        (gameImport^.giSound) (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0
    return True
  where
    doThink selfRef self
        | (self^.eEntityState.esFrame) + 1 < 24 = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eEntityState.esFrame +~ 1
                                       & eNextThink .~ levelTime + Constants.frameTime)
        | otherwise =
            modifyRef selfRef (\v -> v & eEntityState.esFrame +~ 1
                                       & eNextThink .~ 0)

commanderBodyDrop :: EntThink
commanderBodyDrop = EntThink "commander_body_drop" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeToss
                               & eEntityState.esOrigin._z +~ 2)
    return True

spPathCorner :: Ref EdictT -> Quake ()
spPathCorner edictRef = do
    edict <- readRef edictRef
    pathCorner edict (edict^.eTargetName)
  where
    pathCorner edict Nothing = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["path_corner with no targetname at ", encode (edict^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict edictRef
    pathCorner edict _ = do
        modifyRef edictRef (\v -> v & eSolid .~ Constants.solidTrigger
                                    & eTouch .~ Just pathCornerTouch
                                    & eMins .~ V3 (-8) (-8) (-8)
                                    & eMaxs .~ V3 8 8 8
                                    & eSvFlags %~ (.|. Constants.svfNoClient))
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity edictRef

pathCornerTouch :: EntTouch
pathCornerTouch = EntTouch "path_corner_touch" $ \selfRef otherRef _ _ -> do
    done <- shouldReturn selfRef otherRef
    unless done $ do
        saveTarget selfRef otherRef
        target <- pickTarget selfRef
        nextTarget <- maybe (return Nothing) (\t -> pickNextTarget t otherRef) target
        modifyRef otherRef (\v -> v & eGoalEntity .~ nextTarget
                                    & eMoveTarget .~ nextTarget)
        self <- readRef selfRef
        doPathCornerTouch selfRef self otherRef
  where
    shouldReturn selfRef otherRef = do
      other <- readRef otherRef
      return ((other^.eMoveTarget) /= (Just selfRef) || isJust (other^.eEnemy))
    saveTarget selfRef otherRef = do
      self <- readRef selfRef
      modifyRef selfRef (\v -> v & eTarget .~ (self^.ePathTarget))
      GameUtil.useTargets selfRef (Just otherRef)
      modifyRef selfRef (\v -> v & eTarget .~ (self^.eTarget))
    pickTarget selfRef = do
      self <- readRef selfRef
      maybe (return Nothing) (\_ -> GameBase.pickTarget (self^.eTarget)) (self^.eTarget)
    pickNextTarget edictRef otherRef = do
      edict <- readRef edictRef
      doPickNextTarget edictRef edict otherRef
    doPickNextTarget edictRef edict otherRef
        | (edict^.eSpawnFlags) .&. 1 /= 0 = do
            other <- readRef otherRef
            let vv = (edict^.eEntityState.esOrigin) & _z +~ ((edict^.eMins._z) - (other^.eMins._z))
            modifyRef otherRef (\v -> v & eEntityState.esOrigin .~ vv)
            next <- GameBase.pickTarget (edict^.eTarget)
            modifyRef otherRef (\v -> v & eEntityState.esEvent .~ Constants.evOtherTeleport)
            return next
        | otherwise =
            return (Just edictRef)
    doPathCornerTouch selfRef self otherRef
        | (self^.eWait) /= 0 = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            other <- readRef otherRef
            modifyRef otherRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + (self^.eWait))
            doStand otherRef (other^.eMonsterInfo.miStand)
        | otherwise = do
            other <- readRef otherRef
            maybe (noMoveTarget otherRef other) (\_ -> hasMoveTarget otherRef other) (other^.eMoveTarget)
    noMoveTarget otherRef other = do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef otherRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + 100000000)
        doStand otherRef (other^.eMonsterInfo.miStand)
    hasMoveTarget otherRef other =
        maybe goalEntityError (proceedHasMoveTarget otherRef other) (other^.eGoalEntity)
    doStand otherRef Nothing = Com.fatalError "GameMisc.pathCornerTouch#doStand standF is Nothing"
    doStand otherRef (Just standF) = void (entThink standF otherRef)
    goalEntityError = Com.fatalError "GameMisc.pathCornerTouch#hasMoveTarget other^.eGoalEntity is Nothing"
    proceedHasMoveTarget otherRef other goalRef = do
        goal <- readRef goalRef
        modifyRef otherRef (\v -> v & eIdealYaw .~ Math3D.vectorYaw ((goal^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)))

spPointCombat :: Ref EdictT -> Quake ()
spPointCombat edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    pointCombat deathmatch
  where
    pointCombat deathmatch
        | deathmatch /= 0 =
            GameUtil.freeEdict edictRef
        | otherwise = do
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidTrigger
                                        & eTouch .~ Just pointCombatTouch
                                        & eMins .~ V3 (-8) (-8) (-16)
                                        & eMaxs .~ V3 8 8 16
                                        & eSvFlags .~ Constants.svfNoClient)
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef

pointCombatTouch :: EntTouch
pointCombatTouch = EntTouch "point_combat_touch" $ \selfRef otherRef plane surf -> do
    error "GameMisc.pointCombatTouch" -- TODO

spTargetCharacter :: Ref EdictT -> Quake ()
spTargetCharacter selfRef = do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypePush
                               & eSolid .~ Constants.solidBsp
                               & eEntityState.esFrame .~ 12)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel selfRef (self^.eiModel)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef

spTargetString :: Ref EdictT -> Quake ()
spTargetString selfRef = do
    modifyRef selfRef (\v -> v & eMessage %~ (\v -> if isNothing v then Just "" else v)
                               & eUse .~ Just targetStringUse)

targetStringUse :: EntUse
targetStringUse = EntUse "target_string_use" $ \selfRef _ _ -> do
    error "GameMisc.targetStringUse" -- TODO

spViewThing :: Ref EdictT -> Quake ()
spViewThing edictRef = do
    modelIndex <- use (gameBaseGlobals.gbGameImport.giModelIndex)
    modelIdx <- modelIndex (Just "models/objects/banner/tris.md2")
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                & eSolid .~ Constants.solidBbox
                                & eEntityState.esRenderFx .~ Constants.rfFrameLerp
                                & eMins .~ V3 (-16) (-16) (-24)
                                & eMaxs .~ V3 16 16 32
                                & eEntityState.esModelIndex .~ modelIdx)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eNextThink .~ levelTime + 0.5
                                & eThink .~ Just thViewThing)

thViewThing :: EntThink
thViewThing = EntThink "th_viewthing" $ \edictRef -> do
    edict <- readRef edictRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eEntityState.esFrame .~ ((edict^.eEntityState.esFrame) + 1) `mod` 7
                                & eNextThink .~ levelTime + Constants.frameTime)
    return True

throwGib :: Ref EdictT -> B.ByteString -> Int -> Int -> Quake ()
throwGib = error "GameMisc.throwGib" -- TODO

throwHead :: Ref EdictT -> B.ByteString -> Int -> Int -> Quake ()
throwHead selfRef gibName damage gibType = do
    modifyRef selfRef (\v -> v & eEntityState.esSkinNum .~ 0
                               & eEntityState.esFrame .~ 0
                               & eMins .~ V3 0 0 0
                               & eMaxs .~ V3 0 0 0
                               & eEntityState.esModelIndex2 .~ 0
                               & eSolid .~ Constants.solidNot
                               & eEntityState.esEffects %~ (.|. Constants.efGib)
                               & eEntityState.esEffects %~ (.&. (complement Constants.efFlies))
                               & eEntityState.esSound .~ 0
                               & eFlags %~ (.|. Constants.flNoKnockback)
                               & eSvFlags %~ (.&. (complement Constants.svfMonster))
                               & eTakeDamage .~ Constants.damageYes
                               & eDie .~ Just gibDie)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel selfRef (Just gibName)
    vscale <- getVScale
    vd <- velocityForDamage damage
    modifyRef selfRef (\v -> v & eVelocity +~ fmap (* vscale) vd)
    clipGibVelocity selfRef
    r <- Lib.crandom
    r' <- Lib.randomF
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eAVelocity._y .~ r * 600 -- IMPROVE: use Constants.yaw instead of using _y directly
                               & eThink .~ Just GameUtil.freeEdictA
                               & eNextThink .~ levelTime + 10 + r' * 10)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
  where
    getVScale
        | gibType == Constants.gibOrganic = do
            modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeToss
                                       & eTouch .~ Just gibTouch)
            return 0.5
        | otherwise = do
            modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeBounce)
            return 1.0

gibTouch :: EntTouch
gibTouch = EntTouch "gib_touch" $ \selfRef _ plane _ -> do
    error "GameMisc.gibTouch" -- TODO

gibDie :: EntDie
gibDie = EntDie "gib_die" $ \selfRef _ _ _ _ ->
    GameUtil.freeEdict selfRef

clipGibVelocity :: Ref EdictT -> Quake ()
clipGibVelocity edictRef = do
    edict <- readRef edictRef
    let V3 a b c = edict^.eVelocity
        a' = min (max (-300) a) 300
        b' = min (max (-300) b) 300
        c' = min (max 200 c) 500
    modifyRef edictRef (\v -> v & eVelocity .~ V3 a' b' c')

velocityForDamage :: Int -> Quake (V3 Float)
velocityForDamage damage = do
    r1 <- Lib.crandom
    r2 <- Lib.crandom
    r3 <- Lib.crandom
    let v = V3 (100 * r1) (100 * r2) (200 + 100 * r3)
    return (fmap (* coef) v)
  where
    coef | damage < 50 = 0.7
         | otherwise = 1.2

useAreaPortal :: EntUse
useAreaPortal = EntUse "use_areaportal" $ \edictRef _ _ -> do
    modifyRef edictRef (\v -> v & eCount %~ (`xor` 1)) -- toggle state
    edict <- readRef edictRef
    setAreaPortalState <- use (gameBaseGlobals.gbGameImport.giSetAreaPortalState)
    setAreaPortalState (edict^.eStyle) ((edict^.eCount) /= 0)

becomeExplosion1 :: Ref EdictT -> Quake ()
becomeExplosion1 selfRef = do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giWriteByte) Constants.svcTempEntity
    (gameImport^.giWriteByte) Constants.teExplosion1
    (gameImport^.giWritePosition) (self^.eEntityState.esOrigin)
    (gameImport^.giMulticast) (self^.eEntityState.esOrigin) Constants.multicastPvs
    GameUtil.freeEdict selfRef

becomeExplosion2 :: Ref EdictT -> Quake ()
becomeExplosion2 selfRef = do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giWriteByte) Constants.svcTempEntity
    (gameImport^.giWriteByte) Constants.teExplosion2
    (gameImport^.giWritePosition) (self^.eEntityState.esOrigin)
    (gameImport^.giMulticast) (self^.eEntityState.esOrigin) Constants.multicastPvs
    GameUtil.freeEdict selfRef
