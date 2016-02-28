module Triangulate(
    triangulate
  ) where

import Linear
import Control.Lens

import qualified Graphics.Triangulation.Delaunay as D
import qualified Data.Vector.V2 as Vec
import qualified Data.Vector as V
import Data.Vector (Vector)

import Plane
import Debug.Trace

-- | Normalize coords of given vectors to fit [0 .. 1] range and return info
-- how to denormalize them back.
normVecs :: Vector (V2 Float) -> (V2 Float -> V2 Float, Vector (V2 Float))
normVecs vs = (\v -> v * dv + minv, (\v -> (v - minv) / dv) <$> vs)
  where
  minx = minimum . fmap (^. _x) $ vs
  miny = minimum . fmap (^. _y) $ vs
  minv = V2 minx miny

  maxx = maximum . fmap (^. _x) $ vs
  maxy = maximum . fmap (^. _y) $ vs
  maxv = V2 maxx maxy

  dv = fmap (\v -> if v `approxEq` 0 then 1.0 else v) $ maxv - minv

triangulate :: Vector (V3 Float) -> Vector (V3 Float, V3 Float, V3 Float)
triangulate vs
  | V.length vs < 3 = V.empty
  | colinearAll vs = {-traceShow ("colinear", vs)-} V.empty
  | otherwise = {-traceShow ("triangulate", vsnorm) $ -}
      fmap (\(v1, v2, v3) -> (fromV2 v1, fromV2 v2, fromV2 v3))
    . V.fromList . D.triangulate . V.toList . fmap toV2 $ v2snorm
    where
    plane = planeFromPoints (V.unsafeIndex vs 0) (V.unsafeIndex vs 1) (V.unsafeIndex vs 2)
    (denormVec, v2snorm) = normVecs v2s
    v2s = planeProject plane <$> vs
    toV2 (V2 x y) = Vec.Vector2 (realToFrac x) (realToFrac y)
    fromV2 (Vec.Vector2 x y) = planePoint plane . denormVec $ V2 (realToFrac x) (realToFrac y)
