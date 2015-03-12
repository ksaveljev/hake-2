{-# LANGUAGE TemplateHaskell #-}
module Server.SVGlobals ( SVGlobals(..)
                        , module Server.SVGlobals
                        , module Game.CVarT
                        , module Server.ClientT
                        , module Server.ServerT
                        , module Server.ServerStaticT
                        )where

import Control.Lens (makeLenses)
import qualified Data.Sequence as Seq

import Internal
import Game.CVarT
import Server.ClientT
import Server.ServerT
import Server.ServerStaticT

makeLenses ''SVGlobals

initialSVGlobals :: SVGlobals
initialSVGlobals =
  SVGlobals { _svMasterAdr            = Seq.empty -- TODO
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
            }
