{-# LANGUAGE TemplateHaskell #-}
module Render.GLPolyT where

import Control.Lens (makeLenses)

import Quake

data GLPolyT =
  GLPolyT { _glpNext           :: GLPolyT
          , _glpChain          :: GLPolyT
          , _glpNumVerts       :: Int
          , _glpFlags          :: Int
          , _glpPos            :: Int
          , _glpGetX           :: Int -> Quake Float
          , _glpSetX           :: Int -> Float -> Quake ()
          , _glpGetY           :: Int -> Quake Float
          , _glpSetY           :: Int -> Float -> Quake ()
          , _glpGetZ           :: Int -> Quake Float
          , _glpSetZ           :: Int -> Float -> Quake ()
          , _glpGetS1          :: Int -> Quake Float
          , _glpSetS1          :: Int -> Float -> Quake ()
          , _glpGetT1          :: Int -> Quake Float
          , _glpSetT1          :: Int -> Float -> Quake ()
          , _glpGetS2          :: Int -> Quake Float
          , _glpSetS2          :: Int -> Float -> Quake ()
          , _glpGetT2          :: Int -> Quake Float
          , _glpSetT2          :: Int -> Float -> Quake ()
          , _glpBeginScrolling :: Float -> Quake ()
          , _glpEndScrolling   :: Quake ()
          }

makeLenses ''GLPolyT
