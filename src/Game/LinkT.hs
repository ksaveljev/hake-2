module Game.LinkT where

data LinkT =
  LinkT { lPrev :: Maybe LinkT
        , lNext :: Maybe LinkT
        --, object :: ??? -- TODO: is it needed?
        }
