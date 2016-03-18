module Server.SVUser
  ( executeClientMessage
  ) where

import Types

executeClientMessage :: Ref ClientT -> Quake ()
executeClientMessage = error "SVUser.executeClientMessage" -- TODO