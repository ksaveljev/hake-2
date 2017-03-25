module Main where

import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import           Quake     (runQuake)

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     runQuake