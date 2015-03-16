{-# LANGUAGE TemplateHaskell #-}
module Server.SVGlobals ( SVGlobals(..)
                        , module Server.SVGlobals
                        , module Game.CVarT
                        , module Server.ClientT
                        , module Server.ServerT
                        , module Server.ServerStaticT
                        )where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Game.CVarT
import Game.EdictT
import QCommon.NetAdrT
import Server.ClientT
import Server.ServerT
import Server.ServerStaticT
import qualified Constants

makeLenses ''SVGlobals

initialSVGlobals :: SVGlobals
initialSVGlobals =
  SVGlobals { _svMasterAdr            = V.replicate Constants.maxMasters newNetAdrT
            , _svClient               = newClientT
            , _svPaused               = newCVarT
            , _svTimeDemo             = newCVarT
            , _svEnforceTime          = newCVarT
            , _svTimeout              = newCVarT
            , _svZombieTime           = newCVarT
            , _svRconPassword         = newCVarT
            , _svAllowDownload        = newCVarT
            , _svAllowDownloadPlayers = newCVarT
            , _svAllowDownloadModels  = newCVarT
            , _svAllowDownloadSounds  = newCVarT
            , _svAllowDownloadMaps    = newCVarT
            , _svAirAccelerate        = newCVarT
            , _svNoReload             = newCVarT
            , _svMaxClients           = newCVarT
            , _svShowClamp            = newCVarT
            , _svHostname             = newCVarT
            , _svPublicServer         = newCVarT
            , _svReconnectLimit       = newCVarT
            , _svServer               = newServerT
            , _svServerStatic         = newServerStaticT
            , _svPlayer               = newEdictT
            }
