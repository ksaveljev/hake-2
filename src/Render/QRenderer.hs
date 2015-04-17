{-# LANGUAGE OverloadedStrings #-}
module Render.QRenderer where

import Control.Lens ((^.))
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Render.Basic.BasicRenderAPI
import Render.Fast.FastRenderAPI
import Render.GLFWbRenderer
import Render.Renderer

drivers :: V.Vector Renderer
drivers = V.fromList [glfwbRenderer]

getDefaultName :: B.ByteString
getDefaultName = V.head drivers ^. rName

getPreferredName :: B.ByteString
getPreferredName = V.last drivers ^. rName

getDriver :: B.ByteString -> Bool -> Maybe Renderer
getDriver driverName fast = do
    let foundDriver = V.find (\d -> (d^.rName) == driverName) drivers

    case foundDriver of
      Nothing -> Nothing
      Just driver -> Just (driver { _rRefExport = glfwbRefExport (if fast then fastRenderAPI else basicRenderAPI) })

getDriverNames :: V.Vector B.ByteString
getDriverNames = V.map (^.rName) drivers
