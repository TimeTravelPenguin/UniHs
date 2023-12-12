module Helpers.Matrix (shift, shiftRight, shiftUp, horizontalPad, verticalPad) where

import Control.Monad (forM_)
import Data.Foldable (foldr')
import Data.Matrix (Matrix, (!), (<->), (<|>))
import Data.Matrix qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V
import Helpers.Math (kronecker, posMod)

shift :: Int -> Int -> Matrix a -> Matrix a
shift r c m = flip M.mapPos m $
  \(j, i) _ ->
    let y = 1 + (j + r - 1) `posMod` h
        x = 1 + (i - c - 1) `posMod` w
     in m ! (y, x)
 where
  h = M.nrows m
  w = M.ncols m

shiftRight :: Int -> Matrix a -> Matrix a
shiftRight = shift 0

shiftUp :: Int -> Matrix a -> Matrix a
shiftUp n = shift n 0

horizontalPad :: a -> Int -> Matrix a -> Matrix a
horizontalPad x n m =
  shiftRight (pad + n) m'
 where
  w = 2 * n + M.ncols m
  pad = if odd w then 1 else 0
  m' = M.extendTo x 0 w m

verticalPad :: a -> Int -> Matrix a -> Matrix a
verticalPad x n m =
  shiftUp (pad - n) m'
 where
  h = 2 * n + M.nrows m
  -- Using `even` because `shiftUp` negates. It is effectively a double negative.
  pad = if even h then 1 else 0
  m' = M.extendTo x h 0 m
