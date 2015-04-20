{-# LANGUAGE TemplateHaskell #-}
module Client.MenuListS where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Client.MenuCommonS

data MenuListS =
  MenuListS { _mlGeneric   :: MenuCommonS
            , _mlCurValue  :: Int
            , _mlItemNames :: Maybe (V.Vector B.ByteString)
            }

makeLenses ''MenuListS

newMenuListS :: MenuListS
newMenuListS =
  MenuListS { _mlGeneric   = newMenuCommonS
            , _mlCurValue  = 0
            , _mlItemNames = Nothing
            }
