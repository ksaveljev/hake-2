module Client.Types where

import Data.Int (Int8, Int16, Int64)
import Linear.V3 (V3)
import Linear.V4 (V4)
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

import QCommon.Types
import Game.Types

data CEntity = CEntity { cEntityBaseline    :: EntityState
                       , cEntityCurrent     :: EntityState
                       , cEntityPrev        :: EntityState
                       , cEntityServerFrame :: Int
                       , cEntityTrailCount  :: Int
                       , cEntityLerpOrigin  :: V3 Float
                       , cEntityFlyStopTime :: Int
                       }

data CLSustain = CLSustain { clSustainId            :: Int
                           , clSustainType          :: Int
                           , clSustainEndTime       :: Int
                           , clSustainNextThink     :: Int
                           , clSustainThinkInterval :: Int
                           , clSustainOrg           :: V3 Float
                           , clSustainDir           :: V3 Float
                           , clSustainColor         :: Int
                           , clSustainCount         :: Int
                           , clSustainMagnitude     :: Int
                           , clSustainThink         :: IO () -- TODO: ???
                           }

data ClientState = ClientState { clientStateTimeOutCount           :: Int
                               , clientStateTimeDemoFrames         :: Int
                               , clientStateTimeDemoStart          :: Int
                               , clientStateRefreshPrepped         :: Bool
                               , clientStateSoundPrepped           :: Bool
                               , clientStateForceRefDef            :: Bool
                               , clientStateParseEntities          :: Int
                               , clientStateCmd                    :: UserCmd
                               , clientStateCmds                   :: UV.Vector UserCmd
                               , clientStateCmdTime                :: UV.Vector Int
                               , clientStatePredictedOrigins       :: UV.Vector (V3 Int16)
                               , clientStatePredictedStep          :: Float
                               , clientStatePredictedStepTime      :: Int
                               , clientStatePredictedOrigin        :: V3 Float
                               , clientStatePredictedAngles        :: V3 Float
                               , clientStatePredictionError        :: V3 Float
                               , clientStateFrame                  :: Frame
                               , clientStateSurpressCount          :: Int
                               , clientStateFrames                 :: UV.Vector Frame
                               , clientStateViewAngles             :: V3 Float
                               , clientStateTime                   :: Int
                               , clientStateLerpFrac               :: Float
                               , clientStateRefDef                 :: RefDef
                               , clientStateVForward               :: V3 Float
                               , clientStateVRight                 :: V3 Float
                               , clientStateVUp                    :: V3 Float
                               , clientStateLayout                 :: B.ByteString
                               , clientStateInventory              :: UV.Vector Int
                               , clientStateCinematicFile          :: IO () -- TODO: ???
                               , clientStateCinematicTime          :: Int
                               , clientStateCinematicFrame         :: Int
                               , clientStateCinematicPalette       :: UV.Vector Int8
                               , clientStateCinematicPaletteActive :: Bool
                               , clientStateAttractLoop            :: Bool
                               , clientStateServerCount            :: Int
                               , clientStateGameDir                :: B.ByteString
                               , clientStatePlayerNum              :: Int
                               , clientStateConfigStrings          :: UV.Vector B.ByteString
                               , clientStateModelDraw              :: UV.Vector Model
                               , clientStateModelClip              :: CModel
                               , clientStateSoundPrecache          :: UV.Vector SFX
                               , clientStateImagePrecache          :: UV.Vector Image
                               , clientStateClientInfo             :: UV.Vector ClientInfo
                               , clientStateBaseClientInfo         :: ClientInfo
                               }

data ClientStatic = ClientStatic { clientStaticState              :: Int
                                 , clientStaticKeyDest            :: Int
                                 , clientStaticFrameCount         :: Int
                                 , clientStaticRealTime           :: Int
                                 , clientStaticFrameTime          :: Float
                                 , clientStaticDisableScreen      :: Float
                                 , clientStaticDisableServerCount :: Int
                                 , clientStaticServerName         :: B.ByteString
                                 , clientStaticConnectTime        :: Float
                                 , clientStaticQuakePort          :: Int
                                 , clientStaticNetChan            :: NetChan
                                 , clientStaticServerProtocol     :: Int
                                 , clientStaticChallenge          :: Int
                                 , clientStaticDownload           :: FilePath -- TODO: ???
                                 , clientStaticDownloadTempName   :: B.ByteString
                                 , clientStaticDownloadName       :: B.ByteString
                                 , clientStaticDownloadNumber     :: Int
                                 , clientStaticDownloadType       :: Int
                                 , clientStaticDownloadPercent    :: Int
                                 , clientStaticDemoRecording      :: Bool
                                 , clientStaticDemoWaiting        :: Bool
                                 , clientStaticDemoFile           :: FilePath -- TODO: ???
                                 }

data ClientInfo = ClientInfo { clientInfoName        :: B.ByteString
                             , clientInfoCInfo       :: B.ByteString
                             , clientInfoSkin        :: Image
                             , clientInfoIcon        :: Image
                             , clientInfoIconName    :: B.ByteString
                             , clientInfoModel       :: Model
                             , clientInfoWeaponModel :: UV.Vector Model
                             }

data Console = Console { consoleInitialized :: Bool
                       , consoleText        :: UV.Vector Int8
                       , consoleCurrent     :: Int
                       , consoleX           :: Int
                       , consoleDisplay     :: Int
                       , consoleOrMask      :: Int
                       , consoleLineWidth   :: Int
                       , consoleTotalLines  :: Int
                       , consoleCursorSpeed :: Float
                       , consoleVisLines    :: Int
                       , consoleTimes       :: UV.Vector Float
                       }

data CParticle = CParticle { cParticleNext     :: Maybe CParticle
                           , cParticleTime     :: Float
                           , cParticleOrg      :: V3 Float
                           , cParticleVel      :: V3 Float
                           , cParticleAccel    :: V3 Float
                           , cParticleColor    :: Float
                           , cParticleAlpha    :: Float
                           , cParticleAlphaVel :: Float
                           }

data DLight = DLight { dLightOrigin    :: V3 Float
                     , dLightColor     :: V3 Float
                     , dLightIntensity :: V3 Float
                     }

data Entity = Entity { entityModel      :: Model
                     , entityAngles     :: V3 Float
                     , entityOrigin     :: V3 Float
                     , entityFrame      :: Int
                     , entityOldOrigin  :: V3 Float
                     , entityOldFrame   :: Int
                     , entityBackLerp   :: Float
                     , entitySkinNum    :: Int
                     , entityLightStyle :: Int
                     , entityAlpha      :: Float
                     , entitySkin       :: Image
                     , entityFlags      :: Int
                     }

data Frame = Frame { frameValid         :: Bool
                   , frameServerFrame   :: Int
                   , frameDeltaFrame    :: Int
                   , frameAreaBits      :: UV.Vector Int8
                   , framePlayerState   :: PlayerState
                   , frameNumEntities   :: Int
                   , frameParseEntities :: Int
                   }

data KButton = KButton { kButtonDown     :: (Int, Int)
                       , kButtonDownTime :: Int64
                       , kButtonMsec     :: Int64
                       , kButtonState    :: Int
                       }

data LightStyle = LightStyle { lightStyleRGB   :: V3 Float
                             , lightStyleWhite :: Float
                             }

data Particle = Particle { particleColors :: UV.Vector Int8
                         , particleVertexArray :: UV.Vector Float
                         , particleColorTable :: UV.Vector Int
                         , particleColorArray :: UV.Vector Int
                         }

data RefDef = RefDef { refDefX            :: Int
                     , refDefY            :: Int
                     , refDefWidth        :: Int
                     , refDefHeight       :: Int
                     , refDefFovX         :: Float
                     , refDefFovY         :: Float
                     , refDefViewOrg      :: V3 Float
                     , refDefViewAngles   :: V3 Float
                     , refDefBlend        :: V4 Float
                     , refDefTime         :: Float
                     , refDefRdFlags      :: Int
                     , refDefAreaBits     :: UV.Vector Int8
                     , refDefLightStyles  :: UV.Vector LightStyle
                     , refDefNumEntities  :: Int
                     , refDefEntities     :: UV.Vector Entity
                     , refDefNumDLights   :: Int
                     , refDefDLights      :: UV.Vector DLight
                     , refDefNumParticles :: Int
                     }

data RefExport = RefExport { init                :: Int -> Int -> IO Bool
                           , shutDown            :: IO ()
                           , beginRegistration   :: B.ByteString -> IO ()
                           , registerModel       :: B.ByteString -> IO Model
                           , registerSkin        :: B.ByteString -> IO Image
                           , registerPic         :: B.ByteString -> IO Image
                           , setSky              :: B.ByteString -> Float -> V3 Float -> IO ()
                           , endRegistration     :: IO ()
                           , renderFrame         :: RefDef -> IO ()
                           , drawGetPicSize      :: Int -> Int -> B.ByteString -> IO ()
                           , drawPic             :: Int -> Int -> B.ByteString -> IO ()
                           , drawStretchPic      :: Int -> Int -> Int -> Int -> B.ByteString -> IO ()
                           , drawChar            :: Int -> Int -> Char -> IO ()
                           , drawTileClear       :: Int -> Int -> Int -> Int -> B.ByteString -> IO ()
                           , drawFill            :: Int -> Int -> Int -> Int -> Int -> IO ()
                           , drawFadeScreen      :: IO ()
                           , drawStretchRaw      :: Int -> Int -> Int -> Int -> Int -> Int -> UV.Vector Int8 -> IO ()
                           , cinematicSetPalette :: UV.Vector Int8 -> IO ()
                           , beginFrame          :: Float -> IO ()
                           , endFrame            :: IO ()
                           , appActivate         :: Bool -> IO ()
                           , updateScreen        :: XCommand -> IO ()
                           , apiVersion          :: Int
                           , getModelList        :: UV.Vector Int8 -- TODO: ???
                           , getKeyboardHandler  :: Int -- TODO: ???
                           }

data VidDef = VidDef { vidDefWidth     :: Int
                     , vidDefHeight    :: Int
                     , vidDefNewWidth  :: Int
                     , vidDefNewHeight :: Int
                     }

data VidMode = VidMode { vidModeDescription :: B.ByteString
                       , vidModeWidth       :: Int
                       , vidModeHeight      :: Int
                       , vidModeMode        :: Int
                       }

data VRect = VRect { vRectX      :: Int
                   , vRectY      :: Int
                   , vRectWidth  :: Int
                   , vRectHeight :: Int
                   , vRectPNext  :: Maybe VRect
                   }
