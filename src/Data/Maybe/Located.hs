{-# LANGUAGE FlexibleContexts #-}
module Data.Maybe.Located
  ( module Data.Maybe
  , fromJust
  ) where

import Data.Maybe hiding (fromJust)
import qualified GHC.Err.Located as L

fromJust :: L.HasCallStack
         => Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = L.error "Maybe.fromJust: Nothing"
