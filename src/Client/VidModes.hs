module Client.VidModes where

import Types

import qualified Data.Vector as V

vidModes :: V.Vector VidModeT
vidModes = V.fromList
  [ VidModeT "Mode 0: 320x240"     320  240  0
  , VidModeT "Mode 1: 400x300"     400  300  1
  , VidModeT "Mode 2: 512x384"     512  384  2
  , VidModeT "Mode 3: 640x480"     640  480  3
  , VidModeT "Mode 4: 800x600"     800  600  4
  , VidModeT "Mode 5: 960x720"     960  720  5
  , VidModeT "Mode 6: 1024x768"   1024  768  6
  , VidModeT "Mode 7: 1152x864"   1152  864  7
  , VidModeT "Mode 8: 1280x1024"  1280 1024  8
  , VidModeT "Mode 9: 1600x1200"  1600 1200  9
  , VidModeT "Mode 10: 2048x1536" 2048 1536 10
  , VidModeT "Mode 11: user"       640  480 11
  ]
