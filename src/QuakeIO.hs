module QuakeIO
  (runQuake)
  where

import Quake
import QuakeIOState
import QuakeState
import Types

import Control.Monad.Coroutine (resume)
import Control.Monad.Coroutine.SuspensionFunctors (Request(..))
import Control.Monad.State (runState, runStateT)
import System.Random (StdGen)

runQuake :: [String] -> StdGen -> IO ()
runQuake args stdGen =
  run (quake args stdGen) initialQuakeState (unQuakeIO quakeIO) initialQuakeIOState
  where run q state qIO stateIO =
          do let (request, state') = runState (resume q) state
             case request of
               Left (Request cmd q') ->
                 do (response, stateIO') <- runStateT qIO stateIO
                    run (q' response) state' qIO stateIO'
               Right v -> return ()

quakeIO :: QuakeIO Int
quakeIO = undefined
