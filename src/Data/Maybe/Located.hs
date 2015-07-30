{-# LANGUAGE ImplicitParams #-}
module Data.Maybe.Located
  ( module Data.Maybe
  , fromJust
  ) where

import Data.Maybe hiding (fromJust)
import qualified GHC.Err.Located as L
import GHC.Stack

fromJust :: (?callStack :: CallStack)
         => Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = L.error "Maybe.fromJust: Nothing"
