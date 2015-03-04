module Game.Link where

data Link = Link { linkPrev :: Maybe Link
                 , linkNext :: Maybe Link
                 --, object :: ??? -- TODO: is it needed?
                 }
