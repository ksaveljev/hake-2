module Render.QRenderer
  ( getPreferredName
  ) where

import           Render.Renderer
import           Render.GLFWbRenderer (glfwbRenderer)
import           Types

import           Control.Lens ((^.))
import qualified Data.ByteString as B
import qualified Data.Vector as V

drivers :: V.Vector Renderer
drivers = V.fromList [glfwbRenderer]

getPreferredName :: B.ByteString
getPreferredName = V.last drivers ^. rName