module Game.GameItemList ( itemList
                         , module Game.GItemT
                         ) where

import qualified Data.Vector as V

import Types
import Game.GItemT

itemList :: V.Vector GItemT
