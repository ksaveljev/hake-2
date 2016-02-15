module Main where

import QuakeIO (runQuake)

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     runQuake