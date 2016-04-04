module Util.Vector
  ( pipeToVector
  ) where

-- https://www.reddit.com/r/haskell/comments/4d57bx/pipeproducer_to_vector_via_vectors_own_fusion/

import Control.Monad.Primitive (PrimMonad)
import Pipes (Producer, next)
import Data.Vector.Generic (Vector, unsafeFreeze)
import Data.Vector.Generic.Mutable (munstream)
import Data.Vector.Fusion.Bundle.Monadic (unfoldrM)

pipeToVector :: (PrimMonad m, Vector v a) => Producer a m r -> m (v a)
pipeToVector p = munstream (unfoldrM nextMaybe p) >>= unsafeFreeze

nextMaybe :: Monad m => Producer a m r -> m (Maybe (a, Producer a m r))
nextMaybe p = discardLeft <$> next p

discardLeft :: Either r a -> Maybe a
discardLeft x
  | Right a <- x = Just a
  | otherwise    = Nothing