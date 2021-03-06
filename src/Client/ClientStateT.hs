{-# LANGUAGE TemplateHaskell #-}
module Client.ClientStateT where

import           Control.Lens        (makeLenses)
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
import           Linear              (V3(..))

import           Client.ClientInfoT
import           Client.FrameT
import           Client.RefDefT
import qualified Constants
import           Game.UserCmdT
import           Render.ImageT
import           Render.ModelT
import           Sound.SfxT
import           Types

makeLenses ''ClientStateT

newClientStateT :: ClientStateT
newClientStateT = ClientStateT
    { _csTimeOutCount           = 0
    , _csTimeDemoFrames         = 0
    , _csTimeDemoStart          = 0
    , _csRefreshPrepped         = False
    , _csSoundPrepped           = False
    , _csForceRefDef            = False
    , _csParseEntities          = 0
    , _csCmd                    = newUserCmdT
    , _csCmds                   = V.replicate Constants.cmdBackup newUserCmdT
    , _csCmdTime                = UV.replicate Constants.cmdBackup 0
    , _csPredictedOrigins       = UV.replicate Constants.cmdBackup (V3 0 0 0)
    , _csPredictedStep          = 0
    , _csPredictedStepTime      = 0
    , _csPredictedOrigin        = V3 0 0 0
    , _csPredictedAngles        = V3 0 0 0
    , _csPredictionError        = V3 0 0 0
    , _csFrame                  = newFrameT
    , _csSurpressCount          = 0
    , _csFrames                 = V.replicate Constants.updateBackup newFrameT
    , _csViewAngles             = V3 0 0 0
    , _csTime                   = 0
    , _csLerpFrac               = 0
    , _csRefDef                 = newRefDefT
    , _csVForward               = V3 0 0 0
    , _csVRight                 = V3 0 0 0
    , _csVUp                    = V3 0 0 0
    , _csLayout                 = ""
    , _csInventory              = UV.replicate Constants.maxItems 0
    , _csCinematicFile          = Nothing
    , _csCinematicTime          = 0
    , _csCinematicFrame         = 0
    , _csCinematicPalette       = "" -- size 768
    , _csCinematicPaletteActive = False
    , _csAttractLoop            = False
    , _csServerCount            = 0
    , _csGameDir                = ""
    , _csPlayerNum              = 0
    , _csConfigStrings          = V.replicate Constants.maxConfigStrings ""
    , _csModelDraw              = V.replicate Constants.maxModels Nothing
    , _csModelClip              = V.replicate Constants.maxModels Nothing
    , _csSoundPrecache          = V.replicate Constants.maxSounds Nothing
    , _csImagePrecache          = V.replicate Constants.maxImages Nothing
    , _csClientInfo             = V.replicate Constants.maxClients newClientInfoT
    , _csBaseClientInfo         = newClientInfoT
    }
