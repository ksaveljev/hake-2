{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.ClientStateT ( ClientStateT(..)
                           , module Client.ClientStateT
                           ) where

import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Client.ClientInfoT
import Client.FrameT
import Client.RefDefT
import Game.UserCmdT
import Render.ImageT
import Render.ModelT
import Sound.SfxT
import qualified Constants

makeLenses ''ClientStateT

newClientStateT :: ClientStateT
newClientStateT =
  ClientStateT { _csTimeOutCount           = 0
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
               , _csCinematicFile          = ""
               , _csCinematicTime          = 0
               , _csCinematicFrame         = 0
               , _csCinematicPalette       = "" -- size 768
               , _csCinematicPaletteActive = False
               , _csAttractLoop            = False
               , _csServerCount            = 0
               , _csGameDir                = ""
               , _csPlayerNum              = 0
               , _csConfigStrings          = V.replicate Constants.maxConfigStrings ""
               , _csModelDraw              = V.replicate Constants.maxModels newModelT
               , _csModelClip              = V.replicate Constants.maxModels (CModelReference (-1))
               , _csSoundPrecache          = V.replicate Constants.maxSounds newSfxT
               , _csImagePrecache          = V.replicate Constants.maxImages newImageT
               , _csClientInfo             = V.replicate Constants.maxClients newClientInfoT
               , _csBaseClientInfo         = newClientInfoT
               }
