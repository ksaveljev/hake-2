module Client.SCRShared
    ( addDirtyPoint
    , dirtyScreen
    , drawCrosshair
    , sbNums1
    , touchPics
    ) where

import           Control.Applicative   (liftA)
import           Control.Lens          (use, (^.), (.=), (%=), (&), (.~), (%~))
import           Control.Monad         (join, unless, when)
import           Data.Bits             (shiftR)
import qualified Data.ByteString       as B
import qualified Data.Vector           as V

import           Client.DirtyT
import           Client.RefExportT
import           Client.VidDefT
import           Client.VRectT
import           Game.CVarT
import           QCommon.CVarVariables
import           QuakeState
import           Render.Renderer
import           Types
import           Util.Binary           (encode)

import {-# SOURCE #-} qualified QCommon.Com  as Com
import {-# SOURCE #-} qualified QCommon.CVar as CVar

sbNums1 :: V.Vector B.ByteString
sbNums1 = V.fromList
    [ "num_0" , "num_1" , "num_2"
    , "num_3" , "num_4" , "num_5"
    , "num_6" , "num_7" , "num_8"
    , "num_9" , "num_minus"
    ]

addDirtyPoint :: Int -> Int -> Quake ()
addDirtyPoint x y =
    scrGlobals.scrDirty %= (\v -> v & x1 %~ (min x)
                                    & x2 %~ (max x)
                                    & y1 %~ (min y)
                                    & y2 %~ (max y))

drawCrosshair :: Quake ()
drawCrosshair = do
    crosshair <- crosshairCVar
    unless ((crosshair^.cvValue) == 0) $ do
        when (crosshair^.cvModified) $ do
            CVar.update (crosshair & cvModified .~ False)
            touchPics =<< use (globals.gRenderer)
        crosshairPic <- use (scrGlobals.scrCrosshairPic)
        unless (B.null crosshairPic) $ do
            vrect <- use (globals.gScrVRect)
            renderer <- use (globals.gRenderer)
            crosshairWidth <- use (scrGlobals.scrCrosshairWidth)
            crosshairHeight <- use (scrGlobals.scrCrosshairHeight)
            (renderer^.rRefExport.reDrawPic) ((vrect^.vrX) + ((vrect^.vrWidth) - crosshairWidth) `shiftR` 1)
                                             ((vrect^.vrY) + ((vrect^.vrHeight) - crosshairHeight) `shiftR` 1)
                                             crosshairPic

dirtyScreen :: Quake ()
dirtyScreen = do
    vidDef <- use (globals.gVidDef)
    addDirtyPoint 0 0
    addDirtyPoint ((vidDef^.vdWidth) - 1) ((vidDef^.vdHeight) - 1)

touchPics :: Renderer -> Quake ()
touchPics renderer = do
    V.mapM_ (renderer^.rRefExport.reRegisterPic) sbNums1
    processCrosshair renderer =<< crosshairCVar

processCrosshair :: Renderer -> CVarT -> Quake ()
processCrosshair renderer crosshair
    | (crosshair^.cvValue) == 0 = return ()
    | otherwise = join (liftA (updateCrosshair renderer) crossHairValue)
  where
    crossHairValue
        | (crosshair^.cvValue) > 3 || (crosshair^.cvValue) < 0 = do
            CVar.update (crosshair & cvValue .~ 3)
            return 3
        | otherwise = return (crosshair^.cvValue)

updateCrosshair :: Renderer -> Float -> Quake ()
updateCrosshair renderer v = do
    scrGlobals.scrCrosshairPic .= pic
    dim <- (renderer^.rRefExport.reDrawGetPicSize) pic
    maybe updateCrosshairError setDimensions dim
  where
    i = truncate v :: Int
    pic = "ch" `B.append` (encode i)
    updateCrosshairError = Com.fatalError "SCR.updateCrosshair pic size is Nothing"
    setDimensions :: (Int, Int) -> Quake ()
    setDimensions (width, height) = do
        scrGlobals.scrCrosshairWidth .= width
        scrGlobals.scrCrosshairHeight .= height
        when (width == 0) $ scrGlobals.scrCrosshairPic .= ""

