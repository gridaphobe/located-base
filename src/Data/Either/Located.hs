{-# LANGUAGE ImplicitParams #-}
module Data.Either.Located
  ( module Data.Either
  , fromRight, fromLeft
  ) where

import Data.Either
import qualified GHC.Err.Located as L
import GHC.Stack

fromRight :: (?callStack :: CallStack)
          => Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = L.error "Either.fromRight: Left"

fromLeft :: (?callStack :: CallStack)
         => Either a b -> a
fromLeft (Left x)  = x
fromLeft (Right _) = L.error "Either.fromLeft: Right"
