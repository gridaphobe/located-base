{-# LANGUAGE ImplicitParams #-}
module GHC.Err.Located where

import GHC.SrcLoc
import GHC.Stack (CallStack, getCallStack)
import Prelude hiding (error, undefined)
import qualified Prelude
import Text.Printf

error :: (?callStack :: CallStack) => String -> a
error msg = Prelude.error (msg ++ "\n" ++ showCallStack ?callStack)

undefined :: (?callStack :: CallStack) => a
undefined = error "Prelude.undefined"

showCallStack :: CallStack -> String
showCallStack stk = case getCallStack stk of
  _:locs -> unlines $ "Callstack:" : map format locs
  _ -> Prelude.error "showCallStack: empty call-stack"
  where
  unlines = foldr1 (\x y -> x ++ "\n" ++ y)
  format (fn, loc) = printf "  %s, called at %s" fn (showSrcLoc loc)
