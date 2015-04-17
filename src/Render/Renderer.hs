{-# LANGUAGE OverloadedStrings #-}
module Render.Renderer where

import qualified Data.ByteString as B
import qualified Debug.Trace as DT

import Client.RefExportT

getDefaultName :: B.ByteString
getDefaultName = "UNDEFINED RENDERER NAME" -- TODO

getPreferredName :: B.ByteString
getPreferredName = "UNDEFINED RENDERER NAME" -- TODO

getDriver :: B.ByteString -> Bool -> Maybe RefExportT
getDriver _ _ = DT.trace "Renderer.getDriver" undefined -- TODO

getDriverNames :: [B.ByteString]
getDriverNames = DT.trace "Renderer.getDriverNames" undefined -- TODO
