{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.SVGlobals ( SVGlobals(..)
                        , module Server.SVGlobals
                        , module Server.ClientT
                        , module Server.ServerT
                        , module Server.ServerStaticT
                        )where

import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Game.EdictT
import Game.LinkT
import QCommon.NetAdrT
import Server.AreaNodeT
import Server.ClientT
import Server.ServerT
import Server.ServerStaticT
import qualified Constants

makeLenses ''SVGlobals

initialSVGlobals :: SVGlobals
initialSVGlobals =
  SVGlobals { _svMasterAdr            = V.replicate Constants.maxMasters newNetAdrT
            , _svClient               = Nothing
            , _svServer               = newServerT
            , _svServerStatic         = newServerStaticT
            , _svPlayer               = Nothing
            , _svFirstMap             = ""
            , _svMsgBuf               = ""
            , _svNumAreaNodes         = 0
            , _svAreaNodes            = V.generate Constants.areaNodes newAreaNodeT
            , _svAreaMins             = V3 0 0 0
            , _svAreaMaxs             = V3 0 0 0
            , _svAreaList             = UV.empty -- index of gameBaseGlobals.gbGEdicts
            , _svAreaCount            = 0
            , _svAreaMaxCount         = 0
            , _svAreaType             = 0
            , _svLeafs                = UV.replicate 128 0 -- 128 is MAX_TOTAL_ENT_LEAFS
            , _svClusters             = UV.replicate 128 0 -- 128 is MAX_TOTAL_ENT_LEAFS
            , _svTouch                = UV.replicate Constants.maxEdicts (-1) -- index of gameBaseGlobals.gbGEdicts
            , _svTouchList            = UV.replicate Constants.maxEdicts (-1) -- index of gameBaseGlobals.gbGEdicts
            , _svLinks                = V.generate (2 * Constants.areaNodes + Constants.maxEdicts) newLinkT
            }
