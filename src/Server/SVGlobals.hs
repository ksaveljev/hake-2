{-# LANGUAGE TemplateHaskell #-}
module Server.SVGlobals where

import           Control.Lens         (makeLenses)
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as UV
import           Linear               (V3(..))

import qualified Constants
import           Game.LinkT
import           QCommon.NetAdrT
import           QCommon.SizeBufT
import           Server.AreaNodeT
import           Server.ServerT
import           Server.ServerStaticT
import           Types

makeLenses ''SVGlobals

initialSVGlobals :: SVGlobals
initialSVGlobals = SVGlobals
    { _svMasterAdr            = V.replicate Constants.maxMasters newNetAdrT
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
    , _svTouch                = V.replicate Constants.maxEdicts (Ref (-1))
    , _svTouchList            = V.replicate Constants.maxEdicts (Ref (-1))
    , _svLinks                = V.generate Constants.maxLinks (\i -> if i >= 2 * Constants.areaNodes then (newLinkT i) { _lEdict = Just (Ref (i - 2 * Constants.areaNodes)) } else newLinkT i)
    , _svMsg                  = newSizeBufT
    , _svLeafsTmp             = UV.replicate 64 0
    , _svFatPVS               = UV.replicate (65536 `div` 8) 0
    }