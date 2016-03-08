module Render.QRenderer
  ( getDefaultName
  , getDriver
  , getDriverNames
  , getPreferredName
  ) where

import           Render.Basic.BasicRenderAPI (basicRenderAPI)
import           Render.DummyRenderer (dummyRefExport, dummyRenderer)
import           Render.Fast.FastRenderAPI (fastRenderAPI)
import           Render.Renderer
import           Render.GLFWbRenderer (glfwbRefExport, glfwbRenderer)
import           Types

import           Control.Lens ((^.), (&), (.~), _1)
import qualified Data.ByteString as B
import qualified Data.Vector as V

drivers :: V.Vector (Renderer, RenderAPI -> RefExportT)
drivers = V.fromList [ (dummyRenderer, dummyRefExport)
                     , (glfwbRenderer, glfwbRefExport)
                     ]

getDefaultName :: B.ByteString
getDefaultName = V.head drivers ^. _1.rName

getPreferredName :: B.ByteString
getPreferredName = V.last drivers ^. _1.rName

getDriver :: B.ByteString -> Bool -> Maybe Renderer
getDriver name fast =
  maybe Nothing returnDriver foundDriver
  where foundDriver = V.find (\(d, _) -> (d^.rName) == name) drivers
        returnDriver (driver, refExport) =
          Just (driver & rRefExport .~ refExport renderApi)
        renderApi -- | name == "DUMMY" = dummyRenderAPI
                  | fast = fastRenderAPI
                  | otherwise = basicRenderAPI

getDriverNames :: V.Vector B.ByteString
getDriverNames = V.map (^._1.rName) drivers
