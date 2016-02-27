module Triangulate(
    triangulate
  ) where

import Linear

import qualified Graphics.Triangulation.Delaunay as D
import qualified Data.Vector.V2 as Vec
import qualified Data.Vector as V
import Data.Vector (Vector)

import Plane

triangulate :: Vector (V3 Float) -> Vector (V3 Float, V3 Float, V3 Float)
triangulate vs
  | V.length vs < 3 = V.empty
  | colinearAll vs = V.empty
  | otherwise =
      fmap (\(v1, v2, v3) -> (fromV2 v1, fromV2 v2, fromV2 v3))
    . V.fromList . D.triangulate $ v2s
    where
    plane = planeFromPoints (V.unsafeIndex vs 0) (V.unsafeIndex vs 1) (V.unsafeIndex vs 2)
    v2s = V.toList $ toV2 . planeProject plane <$> vs
    toV2 (V2 x y) = Vec.Vector2 (realToFrac x) (realToFrac y)
    fromV2 (Vec.Vector2 x y) = planePoint plane $ V2 (realToFrac x) (realToFrac y)
