module Helpers.Maybe (catMaybesOrDefault) where

import Data.Maybe (catMaybes)

catMaybesOrDefault :: [a] -> [Maybe a] -> [a]
catMaybesOrDefault def xs =
  let justs = catMaybes xs
   in case justs of
        [] -> def
        _ -> justs
