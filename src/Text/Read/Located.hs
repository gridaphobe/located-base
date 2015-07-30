{-# LANGUAGE ImplicitParams #-}
module Text.Read.Located
  ( module Text.Read
  , read
  ) where

import qualified GHC.Err.Located as L
import GHC.Stack
import Prelude hiding (read)
import Text.Read hiding (read)

read :: (?callStack :: CallStack, Read a) => String -> a
read s = either L.error id (readEither s)
