{-# LANGUAGE TemplateHaskell #-}
module Server.SVGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''SVGlobals

initialSVGlobals :: SVGlobals
initialSVGlobals = undefined -- TODO
