{-# LANGUAGE FlexibleContexts #-}
module Data.Either.Located
  ( module E
  , fromRight, fromLeft
  ) where

import qualified Data.Either as E
import qualified GHC.Err.Located as L

fromRight :: L.HasCallStack
          => Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = L.error "Either.fromRight: Left"

fromLeft :: L.HasCallStack
         => Either a b -> a
fromLeft (Left x)  = x
fromLeft (Right _) = L.error "Either.fromLeft: Right"
