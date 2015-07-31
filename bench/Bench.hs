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
  [ bgroup "head"
    [ bgroup "bad"
      [ bench "noloc" $ whnfIO $ throws $ myhead []
      , bench "loc"   $ whnfIO $ throws $ myheadL []
      ]
    , bgroup "good"
      [ bench "noloc" $ whnf myhead [5]
      , bench "loc"   $ whnf myheadL [5]
      ]
    ]
  , bgroup "loop"
    [ bgroup "bad"
      [ bench "noloc" $ whnfIO $ throws $ loop 5 undefined
      , bench "loc"   $ whnfIO $ throws $ loopL 5 L.undefined
      ]
    , bgroup "good"
      [ bench "noloc" $ whnf (loop 5) 0
      , bench "loc"   $ whnf (loopL 5) 0
      ]
    ]
  ]

throws :: a -> IO (Either SomeException a)
throws = try . evaluate

myhead :: [a] -> a
myhead [x] = x
myhead [] = undefined

myheadL :: (?callStack :: CallStack) => [a] -> a
myheadL [x] = x
myheadL [] = L.undefined

loop :: Int -> Int -> Int
loop 0 z = z
loop n z = loop (n-1) z

loopL :: (?callStack :: CallStack) => Int -> Int -> Int
loopL 0 z = z
loopL n z = loopL (n-1) z
