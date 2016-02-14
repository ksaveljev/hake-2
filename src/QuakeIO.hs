module QuakeIO
  (runQuake)
  where

import Quake
import QuakeIOState
import QuakeState
import Types

import Control.Monad.Coroutine (resume)
import Control.Monad.State (runState, evalStateT)

runQuake :: IO ()
runQuake = evalStateT (quakeIO quake initialQuakeState) initialQuakeIOState

quakeIO :: Quake a -> QuakeState -> QuakeIO ()
quakeIO q state =
  do let (req, state') = runState (resume q) state
     case req of
       Left (RunIO cmd q') -> do
         result <- cmd
         quakeIO (q' result) state'
       Right _ -> return ()
