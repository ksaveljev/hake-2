{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.CMGlobals where

import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Game.CModelT
import Game.CPlaneT
import Game.MapSurfaceT
import Game.TraceT
import QCommon.CAreaT
import QCommon.CBrushSideT
import QCommon.CBrushT
import QCommon.CLeafT
import QCommon.CNodeT
import QCommon.QFiles.BSP.DAreaPortalT
import QCommon.QFiles.BSP.DVisT
import qualified Constants

makeLenses ''CMGlobals

initialCMGlobals :: CMGlobals
initialCMGlobals =
  CMGlobals { _cmCheckCount      = 0
            , _cmMapName         = ""
            , _cmNumBrushSides   = 0
            , _cmMapBrushSides   = V.replicate Constants.maxMapBrushSides newCBrushSideT
            , _cmNumTexInfo      = 0
            , _cmMapSurfaces     = V.replicate Constants.maxMapTexInfo newMapSurfaceT
            , _cmNumPlanes       = 0
            , _cmMapPlanes       = V.replicate (Constants.maxMapPlanes + 6) newCPlaneT
            , _cmNumNodes        = 0
            , _cmMapNodes        = V.replicate (Constants.maxMapNodes + 6) newCNodeT
            , _cmNumLeafs        = 1
            , _cmMapLeafs        = V.replicate Constants.maxMapLeafs newCLeafT
            , _cmEmptyLeaf       = 0
            , _cmSolidLeaf       = 0
            , _cmNumLeafBrushes  = 0
            , _cmMapLeafBrushes  = UV.replicate Constants.maxMapLeafBrushes 0
            , _cmNumCModels      = 0
            , _cmMapCModels      = V.replicate Constants.maxMapModels newCModelT
            , _cmNumBrushes      = 0
            , _cmMapBrushes      = V.replicate Constants.maxMapBrushes newCBrushT
            , _cmNumVisibility   = 0
            , _cmMapVisibility   = "" -- size is Constants.maxMapVisibility
            , _cmMapVis          = emptyDVisT
            , _cmNumEntityChars  = 0
            , _cmMapEntityString = ""
            , _cmNumAreas        = 1
            , _cmMapAreas        = V.replicate Constants.maxMapAreas newCAreaT
            , _cmNumAreaPortals  = 0
            , _cmMapAreaPortals  = V.replicate Constants.maxMapAreaPortals emptyDAreaPortalT
            , _cmNumClusters     = 1
            , _cmFloodValid      = 0
            , _cmPortalOpen      = UV.replicate Constants.maxMapAreaPortals False
            , _cmCModBase        = Nothing
            , _cmChecksum        = 0
            , _cmLastChecksum    = 0
            , _cmDebugLoadMap    = False
            , _cmBoxHeadNode     = 0
            , _cmLeafCount       = 0
            , _cmLeafMaxCount    = 0
            , _cmLeafMins        = V3 0 0 0
            , _cmLeafMaxs        = V3 0 0 0
            , _cmLeafTopNode     = 0
            , _cmTraceStart      = V3 0 0 0
            , _cmTraceEnd        = V3 0 0 0
            , _cmTraceMins       = V3 0 0 0
            , _cmTraceMaxs       = V3 0 0 0
            , _cmTraceExtents    = V3 0 0 0
            , _cmTraceTrace      = newTraceT
            , _cmTraceContents   = 0
            , _cmTraceIsPoint    = False
            , _cmLeafs           = UV.replicate 1024 0
            }
