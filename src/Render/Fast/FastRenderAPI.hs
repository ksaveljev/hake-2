module Render.Fast.FastRenderAPI where

import qualified Debug.Trace as DT

import Render.RenderAPI

fastRenderAPI :: RenderAPI
fastRenderAPI = DT.trace "FastRenderAPI.fastRenderAPI" undefined -- TODO
