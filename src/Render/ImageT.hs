{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.ImageT ( ImageT(..)
                     , module Render.ImageT
                     ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''ImageT

newImageT :: Int -> ImageT
newImageT idx =
  ImageT { _iId                   = idx
         , _iName                 = ""
         , _iType                 = 0
         , _iWidth                = 0
         , _iHeight               = 0
         , _iUploadWidth          = 0
         , _iUploadHeight         = 0
         , _iRegistrationSequence = 0
         , _iTextureChain         = Nothing
         , _iTexNum               = 0
         , _iSL                   = 0
         , _iTL                   = 0
         , _iSH                   = 0
         , _iTH                   = 0
         , _iScrap                = False
         , _iHasAlpha             = False
         , _iPaletted             = False
         }
