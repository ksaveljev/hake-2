{-# LANGUAGE Rank2Types #-}
module QCommon.MSG
  ( writeByteI
  ) where

import Types

import Control.Lens (Traversal')

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI = error "MSG.writeByteI" -- TODO