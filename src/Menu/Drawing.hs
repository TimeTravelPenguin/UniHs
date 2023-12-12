module Menu.Drawing where

import Data.Foldable (foldr')
import Data.Matrix (Matrix, (<|>))
import Data.Matrix qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V

data LineStyle = Single | Double

data AdjacentStyles = SameStyle LineStyle | DifferentStyles LineStyle LineStyle

data BoxEdgeStyle = BoxEdgeStyle (Vector Integer) AdjacentStyles

data Box a = Box
  { shape :: Matrix a
  , blOffset :: (Int, Int)
  }

mkBox :: Int -> Int -> (Int, Int) -> Box Int
mkBox h w (x, y) = Box mat (x, y)
 where
  col = M.colVector $ V.replicate h 1
  mat = foldr' (<|>) col $ replicate w col