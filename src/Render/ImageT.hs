{-# LANGUAGE TemplateHaskell #-}
module Render.ImageT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Render.MSurfaceT

data ImageT =
  ImageT { _iId                   :: Int
         , _iName                 :: B.ByteString
         , _iType                 :: Int
         , _iWidth                :: Int
         , _iHeight               :: Int
         , _iUploadWidth          :: Int
         , _iUploadHeight         :: Int
         , _iRegistrationSequence :: Int
         , _iTextureChain         :: MSurfaceT
         , _iTexNum               :: Int
         , _iSL                   :: Float
         , _iTL                   :: Float
         , _iSH                   :: Float
         , _iTH                   :: Float
         , _iScrap                :: Bool
         , _iHasAlpha             :: Bool
         , _iPaletted             :: Bool
         }

makeLenses ''ImageT
