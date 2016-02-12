module Main where

import QuakeIO (runQuake)

import System.Environment (getArgs)
import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import System.Random (newStdGen)

main :: IO ()
main =
  do args <- getArgs
     stdGen <- newStdGen
     hSetBuffering stdout NoBuffering
     runQuake args stdGen
