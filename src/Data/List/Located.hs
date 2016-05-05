{-# LANGUAGE FlexibleContexts #-}
module Data.List.Located
  ( module Data.List
  , head, tail, last, init, foldr1, foldl1, foldl1'
  , maximum, minimum, cycle, (!!)
  ) where

import Data.List hiding ( head, tail, last, init, foldr1, foldl1, foldl1'
                        , maximum, minimum, cycle, (!!)
                        )
import qualified GHC.Err.Located as L
import Prelude hiding ( head, tail, last, init, foldr1, foldl1
                      , maximum, minimum, cycle, (!!)
                      )

head :: L.HasCallStack => [a] -> a
head (x:_) = x
head []    = L.error "Prelude.head: empty list"

tail :: L.HasCallStack => [a] -> [a]
tail (_:xs) = xs
tail []     = L.error "Prelude.tail: empty list"

last :: L.HasCallStack => [a] -> a
last [x]    = x
last (_:xs) = last xs
last []     = L.error "Prelude.last: empty list"

init :: L.HasCallStack => [a] -> [a]
init [_]    = []
init (x:xs) = x : init xs
init []     = L.error "Prelude.init: empty list"

foldr1 :: L.HasCallStack => (a -> a -> a) -> [a] -> a
foldr1 _ [x]    = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 _ []     = L.error "Prelude.foldr1: empty list"

foldl1 :: L.HasCallStack => (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ []     = L.error "Prelude.foldl1: empty list"

foldl1' :: L.HasCallStack => (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl' f x xs
foldl1' _ []     = L.error "Prelude.foldl1': empty list"

maximum :: (L.HasCallStack, Ord a) => [a] -> a
maximum [] = L.error "Prelude.maximum: empty list"
maximum xs = foldl1 max xs

minimum :: (L.HasCallStack, Ord a) => [a] -> a
minimum [] = L.error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

cycle :: L.HasCallStack => [a] -> [a]
cycle [] = L.error "Prelude.cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

(!!) :: L.HasCallStack => [a] -> Int -> a
_      !! n | n < 0 = L.error "Prelude.!!: negative index"
[]     !! _         = L.error "Prelude.!!: index too large"
(x:_)  !! 0         = x
(_:xs) !! n         = xs !! (n-1)
