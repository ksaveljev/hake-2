{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.Renderer ( Renderer(..)
                       , module Render.Renderer
                       , module Client.RefExportT
                       ) where

import Control.Lens (makeLenses)

import qualified Data.ByteString as B
import qualified Debug.Trace as DT

import Internal
import Client.RefExportT

makeLenses ''Renderer

getDefaultName :: B.ByteString
getDefaultName = "UNDEFINED RENDERER NAME" -- TODO

getPreferredName :: B.ByteString
getPreferredName = "UNDEFINED RENDERER NAME" -- TODO

getDriver :: B.ByteString -> Bool -> Maybe Renderer
getDriver _ _ = DT.trace "Renderer.getDriver" undefined -- TODO

getDriverNames :: [B.ByteString]
getDriverNames = DT.trace "Renderer.getDriverNames" undefined -- TODO
