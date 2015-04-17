module Render.Basic.BasicRenderAPI where

import qualified Debug.Trace as DT

import Render.RenderAPI

basicRenderAPI :: RenderAPI
basicRenderAPI = DT.trace "BasicRenderAPI.basicRenderAPI" undefined -- TODO
