module Plane where

import Linear 
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as V 
import qualified Data.HashMap.Strict as H 
import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Data.List (sortBy, nubBy)
import Data.Ord (comparing)

import Debug.Trace

v3 :: a -> V3 a 
v3 a = V3 a a a 

approxEq :: V3 Float -> V3 Float -> Bool 
approxEq v1 v2 = norm (v2 - v1) < 0.000001

nubVecs :: Vector (V3 Float) -> Vector (V3 Float) 
nubVecs = V.fromList . nubBy approxEq . V.toList

signedAngle :: V3 Float -> V3 Float -> Float 
signedAngle a b = s * acos (a' `dot` b')
  where
    a' = normalize a 
    b' = normalize b 
    s  = signum $ triple a b (a `cross` b)
    
catMaybes :: Vector (Maybe a) -> Vector a 
catMaybes = fmap fromJust . V.filter isJust

data Plane = Plane {
  planeOrigin :: !(V3 Float)
, planeNormal :: !(V3 Float)
, planeTangent :: !(V3 Float)
}

-- | Bitangent vector of plane (second coplanar vector of plane)
planeBitangent :: Plane -> V3 Float
planeBitangent Plane{..} = normalize $ planeTangent `cross` planeNormal

-- | Construct plane from thre points
planeFromPoints :: V3 Float -> V3 Float -> V3 Float -> Plane 
planeFromPoints v1 v2 v3 = Plane v1 n t
  where
  t = normalize $ v2 - v1
  n = normalize $ t `cross` (v3 - v1)

-- | Convert plane point into 3D points
planePoint :: Plane -> V2 Float -> V3 Float 
planePoint p (V2 x y) = planeOrigin p + fmap (x *) (planeTangent p) + fmap (y *) (planeBitangent p)

-- | Calculate distance from point to nearest point of plane
distanceToPlane :: Plane -> V3 Float -> Float 
distanceToPlane Plane{..} v = norm $ project planeNormal (v - planeOrigin)

-- | Test if point belongs to the plane
pointInPlane :: V3 Float -> Plane -> Bool 
pointInPlane v p = distanceToPlane p v < 0.0001

data CubeSide = CubeLeft | CubeRight | CubeFront | CubeBack | CubeBottom | CubeTop 
  deriving (Eq, Show)

-- | All planes of axis aligned cube
cubePlanes :: V3 Float -> Float -> Vector (Plane, CubeSide)
cubePlanes o d = V.fromList [
    (Plane o (V3 1 0 0) (V3 0 0 1), CubeLeft)
  , (Plane o (V3 0 0 1) (V3 0 1 0), CubeFront)
  , (Plane o (V3 0 1 0) (V3 1 0 0), CubeBottom)
  , (Plane (o + v3 d) (V3 (-1) 0 0) (V3 0 (-1) 0), CubeRight)
  , (Plane (o + v3 d) (V3 0 0 (-1)) (V3 (-1) 0 0), CubeBack)
  , (Plane (o + v3 d) (V3 0 (-1) 0) (V3 0 0 (-1)), CubeTop)
  ]

-- | Transform cube side to same side but within external cube
swapSide :: CubeSide -> CubeSide 
swapSide cs = case cs of 
  CubeLeft -> CubeRight 
  CubeRight -> CubeLeft 
  CubeFront -> CubeBack 
  CubeBack -> CubeFront 
  CubeBottom -> CubeTop 
  CubeTop -> CubeBottom

data Line = Line {
  lineOrigin :: !(V3 Float)
, lineTangent :: !(V3 Float)
} deriving (Show)

linePoint :: Line -> Float -> V3 Float 
linePoint Line{..} a = lineOrigin + fmap (a *) lineTangent

-- | Check if given line is completly belongs to plane
lineInPlane :: Line -> Plane -> Bool 
lineInPlane l@Line{..} p@Plane{..} = (lineOrigin `pointInPlane` p) && cosAngle < 0.001
  where
    cosAngle = abs $ planeNormal `dot` lineTangent

-- | Return all edges of cube with given origin and size
gridEdges :: V3 Float -> Float -> Vector Line 
gridEdges o d = V.fromList [
    Line o (V3 1 0 0)
  , Line o (V3 0 1 0)
  , Line o (V3 0 0 1)
  , Line (o + v3 d) (V3 (-1) 0 0)
  , Line (o + v3 d) (V3 0 (-1) 0)
  , Line (o + v3 d) (V3 0 0 (-1))
  , Line (o + V3 0 d 0) (V3 1 0 0)
  , Line (o + V3 0 d 0) (V3 0 0 1)
  , Line (o + V3 d 0 d) (V3 (-1) 0 0)
  , Line (o + V3 d 0 d) (V3 0 0 (-1))
  , Line (o + V3 d 0 0) (V3 0 1 0)
  , Line (o + V3 0 0 d) (V3 0 1 0)
  ]

-- | Return cube cut for cube with given origin and size
gridCut :: Plane -> V3 Float -> Float -> Vector (V3 Float)
gridCut plane o d = catMaybes $ intersect <$> gridEdges o d
  where 
    intersect l = if l `lineInPlane` plane then Nothing else let 
      (a, _, _) = lineCrossPlane l plane
      in if a >= 0 && a <= d then Just (linePoint l a) else Nothing 

-- | Project 3D point into plane
planeProject :: Plane -> V3 Float -> V2 Float 
planeProject p@Plane{..} (V3 x y z) = V2 b c
  where 
  V3 tx ty tz = planeTangent
  V3 btx bty btz = planeBitangent p
  V3 nx ny nz = planeNormal
  V3 p0x p0y p0z = planeOrigin
  b = negate $ (btx*ny*p0z-btx*ny*z-btx*nz*p0y+btx*nz*y-bty*nx*p0z+bty*nx*z+bty*nz*p0x-bty*nz*x+btz*nx*p0y-btz*nx*y-btz*ny*p0x+btz*ny*x)/(btx*ny*tz-btx*nz*ty-bty*nx*tz+bty*nz*tx+btz*nx*ty-btz*ny*tx)
  c = (nx*p0y*tz-nx*p0z*ty+nx*ty*z-nx*tz*y-ny*p0x*tz+ny*p0z*tx-ny*tx*z+ny*tz*x+nz*p0x*ty-nz*p0y*tx+nz*tx*y-nz*ty*x)/(btx*ny*tz-btx*nz*ty-bty*nx*tz+bty*nz*tx+btz*nx*ty-btz*ny*tx)

isInTriangle2D :: V2 Float -- ^ First point
  -> V2 Float -- ^ Second point
  -> V2 Float -- ^ Third point
  -> V2 Float -- ^ Test point
  -> Bool 
isInTriangle2D v1 v2 v3 vt = (b1 == b2) && (b2 == b3)
  where
  signp (V2 p1x p1y) (V2 p2x p2y) (V2 p3x p3y) = (p1x - p3x) * (p2y - p3y) - (p2x - p3x) * (p1y - p3y)
  b1 = signp vt v1 v2 <= 0 
  b2 = signp vt v2 v3 <= 0 
  b3 = signp vt v3 v1 <= 0 

isInTriangle :: V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> V3 Float -- ^ Test point
  -> Bool 
isInTriangle a b c v = isInTriangle2D a' b' c' v'
  where
  plane = planeFromPoints a b c
  a' = V2 0 0
  b' = planeProject plane b 
  c' = planeProject plane c 
  v' = planeProject plane v 

-- | Points that triangle cut cube (doesn't include borders of triangle)
triangleCut :: V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> V3 Float -- ^ Cube origin 
  -> Float -- ^ Cube size
  -> Vector (V3 Float)
triangleCut a b c o d = V.filter (isInTriangle a b c) $ gridCut plane o d
  where 
  plane = planeFromPoints a b c

-- | Construct line from two points
lineFromPoints :: V3 Float -> V3 Float -> Line 
lineFromPoints lbegin lend = Line lbegin (normalize $ lend - lbegin)

-- | Calclulate line and plane parameters for crossing point
lineCrossPlane :: Line -> Plane -> (Float, Float, Float)
lineCrossPlane Line{..} p@Plane{..} = (a, b, c)
  where
  V3 vlx vly vlz = lineOrigin
  V3 tlx tly tlz = lineTangent
  V3 vp0x vp0y vp0z = planeOrigin
  V3 tpx tpy tpz = planeTangent 
  V3 btpx btpy btpz = planeBitangent p
  a = (btpx*tpy*vlz-btpx*tpy*vp0z-btpx*tpz*vly+btpx*tpz*vp0y-btpy*tpx*vlz+btpy*tpx*vp0z+btpy*tpz*vlx-btpy*tpz*vp0x+btpz*tpx*vly-btpz*tpx*vp0y-btpz*tpy*vlx+btpz*tpy*vp0x)/(btpx*tly*tpz-btpx*tlz*tpy-btpy*tlx*tpz+btpy*tlz*tpx+btpz*tlx*tpy-btpz*tly*tpx)
  b = (btpx*tly*vlz-btpx*tly*vp0z-btpx*tlz*vly+btpx*tlz*vp0y-btpy*tlx*vlz+btpy*tlx*vp0z+btpy*tlz*vlx-btpy*tlz*vp0x+btpz*tlx*vly-btpz*tlx*vp0y-btpz*tly*vlx+btpz*tly*vp0x)/(btpx*tly*tpz-btpx*tlz*tpy-btpy*tlx*tpz+btpy*tlz*tpx+btpz*tlx*tpy-btpz*tly*tpx)
  c = (tlx*tpy*vlz-tlx*tpy*vp0z-tlx*tpz*vly+tlx*tpz*vp0y-tly*tpx*vlz+tly*tpx*vp0z+tly*tpz*vlx-tly*tpz*vp0x+tlz*tpx*vly-tlz*tpx*vp0y-tlz*tpy*vlx+tlz*tpy*vp0x)/(btpx*tly*tpz-btpx*tlz*tpy-btpy*tlx*tpz+btpy*tlz*tpx+btpz*tlx*tpy-btpz*tly*tpx)

-- | Cross line and plane with given restriction for tangent lengths
lineCrossPlaneRestrict :: Line -> Float -> Plane -> Float -> Maybe (V3 Float)
lineCrossPlaneRestrict l lineLength p planeLength = let 
  (a, b, c) = lineCrossPlane l p
  in if a > 0 && a <= lineLength && b > 0 && b <= planeLength && c > 0 && c <= planeLength
        then Just $ lineOrigin l + fmap (a *) (lineTangent l)
        else Nothing

-- | Detect cross line and axis aligned box
lineCrossBoxRestrict :: Line -> Float -> V3 Float -> Float -> Maybe (V3 Float, CubeSide)
lineCrossBoxRestrict l lineLength boxOrigin boxSize = let
  tries = try <$> cubePlanes boxOrigin boxSize
  try (p, s) = if l `lineInPlane` p then traceShow "!!!!" Nothing
    else case lineCrossPlaneRestrict l lineLength p boxSize of 
      Nothing -> Nothing
      Just v -> Just (v, s)
  succs = catMaybes tries 
  in succs V.!? 0

-- | Returns True if all points lies on single line
colinear :: V3 Float -> V3 Float -> V3 Float -> Bool
colinear a b c = let s = norm ((b - a) `cross` (c - a)) in s < 0.00001

-- | Test if all vectors in the vector are not forming triangles
colinearAll :: Vector (V3 Float) -> Bool
colinearAll vs 
  | V.length vs < 3 = False 
  | otherwise = go (V.unsafeIndex vs 0) (V.unsafeIndex vs 1) (V.unsafeIndex vs 2) (V.drop 3 vs)
  where
    go a b c vs' 
      | V.length vs' == 0 = colinear a b c
      | otherwise = if colinear a b c then True else go b c (V.unsafeIndex vs' 0) (V.unsafeTail vs')