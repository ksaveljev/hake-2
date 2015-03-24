module Game.SuperAdapter where

import qualified Data.ByteString as B

class SuperAdapter a where
    getID :: a -> B.ByteString
