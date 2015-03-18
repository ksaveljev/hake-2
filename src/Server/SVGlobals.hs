{-# LANGUAGE TemplateHaskell #-}
module Server.SVGlobals ( SVGlobals(..)
                        , module Server.SVGlobals
                        , module Server.ClientT
                        , module Server.ServerT
                        , module Server.ServerStaticT
                        )where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
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
            , _svServer               = newServerT
            , _svServerStatic         = newServerStaticT
            , _svPlayer               = newEdictT
            }
