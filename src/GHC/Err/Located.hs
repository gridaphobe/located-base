{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
module GHC.Err.Located (error, undefined, HasCallStack, showCallStack) where

#if __GLASGOW_HASKELL__ < 800

import GHC.SrcLoc
import GHC.Stack (CallStack, getCallStack)
import Prelude hiding (error, undefined)
import qualified Prelude
import Text.Printf

type HasCallStack = (?callStack :: CallStack)

error :: HasCallStack => String -> a
error msg = Prelude.error (msg ++ "\n" ++ showCallStack ?callStack)

undefined :: HasCallStack => a
undefined = error "Prelude.undefined"

showCallStack :: CallStack -> String
showCallStack stk = case getCallStack stk of
  _:locs -> unlines $ "Callstack:" : map format locs
  _ -> Prelude.error "showCallStack: empty call-stack"
  where
  format (fn, loc) = printf "  %s, called at %s" fn (showSrcLoc loc)

#else

import GHC.Stack

{-# DEPRECATED showCallStack "use GHC.Stack.prettyCallStack instead" #-}
showCallStack :: CallStack -> String
showCallStack = prettyCallStack

#endif
