{-# LANGUAGE TemplateHaskell #-}
module Client.RefExportT ( RefExportT(..)
                         , module Client.RefExportT
                         , module Sys.KBD
                         ) where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Internal
import Sys.KBD

makeLenses ''RefExportT

dummyRenderer :: RefExportT
dummyRenderer =
  RefExportT { _reInit                = (\_ _ -> return False)
             , _reShutDown            = return ()
             , _reBeginRegistration   = (\_ -> return ())
             , _reRegisterModel       = (\_ -> return Nothing)
             , _reRegisterSkin        = (\_ -> return Nothing)
             , _reRegisterPic         = (\_ -> return Nothing)
             , _reSetSky              = (\_ _ _ -> return ())
             , _reEndRegistration     = return ()
             , _reRenderFrame         = (\_ -> return ())
             , _reDrawGetPicSize      = (\_ _ _ -> return ())
             , _reDrawPic             = (\_ _ _ -> return ())
             , _reDrawStretchPic      = (\_ _ _ _ _ -> return ())
             , _reDrawChar            = (\_ _ _ -> return ())
             , _reDrawTileClear       = (\_ _ _ _ _ -> return ())
             , _reDrawFill            = (\_ _ _ _ _ -> return ())
             , _reDrawFadeScreen      = return ()
             , _reDrawStretchRaw      = (\_ _ _ _ _ _ _ -> return ())
             , _reCinematicSetPalette = (\_ -> return ())
             , _reBeginFrame          = (\_ -> return ())
             , _reEndFrame            = return ()
             , _reAppActivate         = (\_ -> return ())
             , _reUpdateScreen        = (\callback -> callback)
             , _reApiVersion          = 0
             , _reGetModeList         = UV.empty
             , _reGetKeyboardHandler  = Nothing
             }
