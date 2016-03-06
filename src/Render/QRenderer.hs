module Render.QRenderer
  ( getDefaultName
  , getDriver
  , getDriverNames
  , getPreferredName
  ) where

import           Render.Basic.BasicRenderAPI (basicRenderAPI)
import           Render.Fast.FastRenderAPI (fastRenderAPI)
import           Render.Renderer
import           Render.GLFWbRenderer (glfwbRefExport, glfwbRenderer)
import           Types

import           Control.Lens ((^.), (&), (.~))
import qualified Data.ByteString as B
import qualified Data.Vector as V

drivers :: V.Vector Renderer
drivers = V.fromList [glfwbRenderer]

getDefaultName :: B.ByteString
getDefaultName = V.head drivers ^. rName

getPreferredName :: B.ByteString
getPreferredName = V.last drivers ^. rName

getDriver :: B.ByteString -> Bool -> Maybe Renderer
getDriver name fast =
  maybe Nothing returnDriver foundDriver
  where foundDriver = V.find (\d -> (d^.rName) == name) drivers
        returnDriver driver =
          Just (driver & rRefExport .~ glfwbRefExport renderApi)
        renderApi | fast = fastRenderAPI
                  | otherwise = basicRenderAPI

getDriverNames :: V.Vector B.ByteString
getDriverNames = V.map (^.rName) drivers
