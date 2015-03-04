module Game.CVar where

import qualified Data.ByteString as B

data CVar = CVar { cVarName          :: B.ByteString
                 , cVarString        :: B.ByteString
                 , cVarLatchedString :: B.ByteString
                 , cVarFlags         :: Int
                 , cVarModified      :: Bool
                 , cVarValue         :: Float
                 , cVarNext          :: Maybe CVar
                 }
