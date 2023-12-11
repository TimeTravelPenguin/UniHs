module Helpers.Sequence (rights, nextMissingNonNegative) where

import Data.IntSet (IntSet, member)
import Data.Sequence (Seq (..))

rights :: Seq (Either a b) -> Seq b
rights Empty = Empty
rights (Left _ :<| xs) = rights xs
rights (Right x :<| xs) = x :<| rights xs
{-# INLINEABLE rights #-}

nextMissingNonNegative :: IntSet -> Int
nextMissingNonNegative = go 0
 where
  go n nums =
    if member n nums
      then go (n + 1) nums
      else n
