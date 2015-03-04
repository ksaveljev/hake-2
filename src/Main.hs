{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)

import Quake
import qualified QCommon.CVar as CVar

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg [] = False

main :: IO ()
main = do
    runQuake undefined $ do
      args <- io getArgs
      let dedicated = isDedicatedCmdArg args
      CVar.get "dedicated" "0" 1
      -- check if we start in dedicated mode
      -- set dedicated value
      -- if not dedicated then init our client window
      -- strip some args and call QCommon.init
      -- grab current time
      -- forever loop calling QCommon.frame
      undefined
    return ()
