module Helpers.Math (posMod, kronecker) where

posMod :: Integral a => a -> a -> a
posMod x n = (x `mod` n + n) `mod` n

kronecker :: Integral a => a -> a -> a
kronecker a b = if a == b then 1 else 0