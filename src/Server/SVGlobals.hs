{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.SVGlobals ( SVGlobals(..)
                        , module Game.EdictT
                        , module Game.LinkT
                        , module QCommon.NetAdrT
                        , module Server.AreaNodeT
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
            , _svAreaList             = V.empty
            , _svAreaCount            = 0
            , _svAreaMaxCount         = 0
            , _svAreaType             = 0
            , _svLeafs                = UV.replicate 128 0 -- 128 is MAX_TOTAL_ENT_LEAFS
            , _svClusters             = UV.replicate 128 0 -- 128 is MAX_TOTAL_ENT_LEAFS
            , _svTouch                = V.replicate Constants.maxEdicts (EdictReference (-1))
            , _svTouchList            = V.replicate Constants.maxEdicts (EdictReference (-1))
            , _svLinks                = V.generate Constants.maxLinks (\i -> if i >= 2 * Constants.areaNodes then (newLinkT i) { _lEdict = Just (EdictReference (i - 2 * Constants.areaNodes)) } else newLinkT i)
            , _svMsg                  = newSizeBufT
            }
