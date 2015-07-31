{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import Criterion.Main

import Control.Exception
import qualified Data.List.Located as L
import qualified GHC.Err.Located as L
import GHC.Stack
import System.IO.Unsafe

main = defaultMain
  [ bgroup "head" [ bench "noloc" $ whnfIO $ throws $ head []
                  , bench "loc"   $ whnfIO $ throws $ L.head []
                  ]
  , bgroup "loop" [ bench "noloc" $ whnfIO $ throws $ loop 5
                  , bench "loc"   $ whnfIO $ throws $ loopL 5
                  ]
  ]

throws :: a -> IO (Either SomeException a)
throws = try . evaluate

loop :: Int -> Int
loop 0 = undefined
loop n = loop (n-1)

loopL :: (?callStack :: CallStack) => Int -> Int
loopL 0 = L.undefined
loopL n = loopL (n-1)
