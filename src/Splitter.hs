module Splitter(
    splitMesh
  ) where

import Linear 
import Codec.Wavefront hiding (Line)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as V 
import qualified Data.HashMap.Strict as H 
import Data.Maybe (isJust, fromJust)
import Data.Monoid

v3 :: a -> V3 a 
v3 a = V3 a a a 

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

data CubeSide = CubeLeft | CubeRight | CubeFront | CubeBack | CubeBottom | CubeTop 
  deriving (Eq, Show)

-- | All planes of axis aligned cube
cubePlanes :: V3 Float -> Float -> Vector (Plane, CubeSide)
cubePlanes o d = V.fromList [
    (Plane o (V3 1 0 0) (V3 0 1 0), CubeLeft)
  , (Plane o (V3 0 0 1) (V3 0 1 0), CubeFront)
  , (Plane o (V3 0 1 0) (V3 1 0 0), CubeBottom)
  , (Plane (o + v3 d) (V3 (-1) 0 0) (V3 0 (-1) 0), CubeRight)
  , (Plane (o + v3 d) (V3 0 0 (-1)) (V3 (-1) 0 0), CubeBack)
  , (Plane (o + v3 d) (V3 0 (-1) 0) (V3 (-1) 0 0), CubeTop)
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
}

linePoint :: Line -> Float -> V3 Float 
linePoint Line{..} a = lineOrigin + fmap (a *) lineTangent

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
    intersect l = let (a, _, _) = lineCrossPlane l plane
      in if a >= 0 && a < d then Just (linePoint l a) else Nothing 

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
  b1 = signp vt v1 v2 < 0 
  b2 = signp vt v2 v3 < 0 
  b3 = signp vt v3 v1 < 0 

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

{-
-- | Get lines of grid sector
gridSideEdges :: 
  CubeSide -- ^ Side of grid sector
  -> V3 Float -- ^ Grid cube origin 
  -> Float -- ^ Grid size
  -> Vector Line -- ^ 4 lines that encloses that cube side
gridSideEdges cs o d = case cs of 
  CubeLeft -> V.fromList [Line o (V3 0 0 1), Line o (V3 0 1 0), Line (o + v3 d) (V3 0 0 (-1)), Line (o + v3 d) (V3 0 (-1) 0)]
  CubeRight -> V.fromList [Line (o + v3 d) (V3 0 0 (-1)), Line (o + v3 d) (V3 0 (-1) 0), Line (o + V3 d 0 0) (V3 0 0 1), Line (o + V3 d 0 0) (V3 0 1 0)]
  CubeFront -> V.fromList [Line o (V3 1 0 0), Line o (V3 0 1 0), Line (o + V3 d d 0) (V3 (-1) 0 0), Line (o + V3 d d 0) (V3 0 (-1) 0)]
  CubeBack -> V.fromList [Line (o + v3 d) (V3 (-1) 0 0), Line (o + v3 d) (V3 0 (-1) 0), Line (o + V3 0 0 d) (V3 1 0 0), Line (o + V3 0 0 d) (V3 0 1 0)]
  CubeBottom -> V.fromList [Line o (V3 1 0 0), Line o (V3 0 0 1), Line (o + V3 d 0 d) (V3 (-1) 0 0), Line (o + V3 d 0 d) (V3 0 0 (-1))]
  CubeTop -> V.fromList [Line (o + v3 d) (V3 (-1) 0 0), Line (o + v3 d) (V3 0 0 (-1)), Line (o + V3 0 d 0) (V3 1 0 0), Line (o + V3 0 d 0) (V3 0 0 1)]

gridSidesEdge :: CubeSide -- ^ First side
  -> CubeSide -- ^ Second side
  -> V3 Float -- ^ Grid cube origin 
  -> Float -- ^ Grid size
  -> Maybe Line -- ^ Not defined if same sides or planar sides
gridSidesEdge cs1 cs2 o d = case (cs1, cs2) of 
  (CubeLeft, CubeLeft) -> Nothing 
  (CubeLeft, CubeRight) -> Nothing 
  (CubeLeft, CubeFront) -> Just $ Line o (V3 0 1 0)
  (CubeLeft, CubeBack) -> Just $ Line (o + V3 0 0 d) (V3 0 1 0)
  (CubeLeft, CubeBottom) -> Just $ Line o (V3 0 0 1)
  (CubeLeft, CubeTop) -> Just $ Line (o + V3 0 d 0) (V3 0 0 1)

  (CubeRight, CubeLeft) -> Nothing
  (CubeRight, CubeRight) -> Nothing
  (CubeRight, CubeFront) -> Just $ Line (o + V3 d 0 0) (V3 0 1 0)
  (CubeRight, CubeBack) -> Just $ Line (o + V3 d 0 d) (V3 0 1 0)
  (CubeRight, CubeBottom) -> Just $ Line (o + V3 d 0 0) (V3 0 0 1)
  (CubeRight, CubeTop) -> Just $ Line (o + V3 d d 0) (V3 1 0 0)

  (CubeFront, CubeLeft) -> Just $ Line o (V3 0 1 0)
  (CubeFront, CubeRight) -> Just $ Line (o + V3 d 0 0) (V3 0 1 0)
  (CubeFront, CubeFront) -> Nothing
  (CubeFront, CubeBack) -> Nothing
  (CubeFront, CubeBottom) -> Just $ Line o (V3 1 0 0)
  (CubeFront, CubeTop) -> Just $ Line (o + V3 0 d 0) (V3 1 0 0)

  (CubeBack, CubeLeft) -> Just $ Line (o + V3 0 0 d) (V3 0 1 0)
  (CubeBack, CubeRight) -> Just $ Line (o + V3 d 0 d) (V3 0 1 0)
  (CubeBack, CubeFront) -> Nothing
  (CubeBack, CubeBack) -> Nothing
  (CubeBack, CubeBottom) -> Just $ Line (o + V3 0 0 d) (V3 1 0 0)
  (CubeBack, CubeTop) -> Just $ Line (o + V3 0 d d) (V3 1 0 0)

  (CubeBottom, CubeLeft) -> Just $ Line o (V3 0 0 1)
  (CubeBottom, CubeRight) -> Just $ Line (o + V3 d 0 0) (V3 0 0 1)
  (CubeBottom, CubeFront) -> Just $ Line o (V3 1 0 0)
  (CubeBottom, CubeBack) -> Just $ Line (o + V3 0 0 d) (V3 1 0 0)
  (CubeBottom, CubeBottom) -> Nothing
  (CubeBottom, CubeTop) -> Nothing

  (CubeTop, CubeLeft) -> Just $ Line (o + V3 0 d 0) (V3 0 0 1)
  (CubeTop, CubeRight) -> Just $ Line (o + V3 d d 0) (V3 1 0 0)
  (CubeTop, CubeFront) -> Just $ Line (o + V3 0 d 0) (V3 1 0 0)
  (CubeTop, CubeBack) -> Just $ Line (o + V3 0 d d) (V3 1 0 0)
  (CubeTop, CubeBottom) -> Nothing
  (CubeTop, CubeTop) -> Nothing
-}

-- | Construct line from two points
lineFromPoints :: V3 Float -> V3 Float -> Line 
lineFromPoints lbegin lend = Line lbegin (normalize $ lend - lbegin)

-- | Fast test to determine if line crosses given plane at positive coords of line
lineCrossesPlane :: Line -> Plane -> Bool
lineCrossesPlane Line{..} Plane{..} = lineTangent `dot` planeNormal < 0

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
lineCrossPlaneRestrict l lineLength p planeLength = if l `lineCrossesPlane` p 
  then let (a, b, c) = lineCrossPlane l p
    in if a > 0 && a <= lineLength && b > 0 && b <= planeLength && c > 0 && c <= planeLength
         then Just $ lineOrigin l + fmap (a *) (lineTangent l)
         else Nothing
  else Nothing

-- | Detect cross line and axis aligned box
lineCrossBoxRestrict :: Line -> Float -> V3 Float -> Float -> Maybe (V3 Float, CubeSide)
lineCrossBoxRestrict l lineLength boxOrigin boxSize = let
  tries = try <$> cubePlanes boxOrigin boxSize
  try (p, s) = case lineCrossPlaneRestrict l lineLength p boxSize of 
    Nothing -> Nothing
    Just v -> Just (v, s)
  succs = catMaybes tries 
  in succs V.!? 0

-- | Detect cross for line and grid
lineCrossGrid :: 
     V3 Float -- ^ Line origin
  -> V3 Float -- ^ Line end 
  -> Float -- ^ Grid size
  -> Maybe (V3 Float, CubeSide) -- ^ Intersection
lineCrossGrid lbeg lend gsize = lineCrossBoxRestrict l ll gridOrigin gsize
  where
    l = lineFromPoints lbeg lend
    ll = norm $ lend - lbeg
    gridOrigin = fmap ((gsize *) . fromIntegral) $ toGridOrigin lbeg gsize

toGridOrigin :: V3 Float -- ^ Point 
  -> Float -- ^ Grid size
  -> V3 Int -- ^ Origin of grid cube that contains the point
toGridOrigin v gsize = fmap (floor . (/ gsize)) v

-- | Split line into parts by grid
splitLine :: V3 Float -- ^ First point
  -> V3 Float -- ^ Second point 
  -> Float -- ^ Grid size 
  -> HashMap (V3 Int) (Vector (V3 Float)) -- ^ Separated points, points are duplicated at edges for each box
splitLine v1 v2 gsize = go H.empty (toGridOrigin v1 gsize) v1
  where
  go :: HashMap (V3 Int) (Vector (V3 Float)) -> V3 Int -> V3 Float -> HashMap (V3 Int) (Vector (V3 Float))
  go acc i v = case lineCrossGrid v v2 gsize of 
    Nothing -> append i v2 . append i v $ acc 
    Just (v', _) -> go (append i v' . append i v $ acc) (toGridOrigin v' gsize) v'

  append :: V3 Int -> V3 Float -> HashMap (V3 Int) (Vector (V3 Float)) -> HashMap (V3 Int) (Vector (V3 Float))
  append k v m = case H.lookup k m of 
    Nothing -> H.insert k (V.singleton v) m 
    Just vs -> H.insert k (vs `V.snoc` v) m  


-- | Splits triangle by grid to several triangles
--
-- Note: points should be passed in CCW order
splitTriangle :: 
     V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> Float -- ^ Grid size
  -> HashMap (V3 Int) (Vector (V3 Float, V3 Float, V3 Float)) -- ^ Triangles by grid boxes
splitTriangle v1 v2 v3 gsize = H.mapWithKey triangulate . H.mapWithKey addCut $ 
  splitLine v1 v2 gsize `merge` splitLine v2 v3 gsize `merge` splitLine v3 v1 gsize
  where
  merge = H.unionWith (<>)

  triangulate :: V3 Int -> Vector (V3 Float) -> Vector (V3 Float, V3 Float, V3 Float)
  triangulate i vs 
    | V.length vs < 3 = V.empty
    | otherwise = mkFan V.empty (V.unsafeHead vs) (V.unsafeTail vs)
    where
    mkFan :: Vector (V3 Float, V3 Float, V3 Float) -> V3 Float -> Vector (V3 Float) -> Vector (V3 Float, V3 Float, V3 Float)
    mkFan acc v0 vs' 
      | V.length vs' < 2 = acc
      | otherwise = mkFan (acc `V.snoc` (v0, V.unsafeIndex vs' 0, V.unsafeIndex vs' 1)) v0 $ V.unsafeTail vs'

  addCut :: V3 Int -> Vector (V3 Float) -> Vector (V3 Float)
  addCut i vs = vs <> triangleCut v1 v2 v3 ((* gsize) . fromIntegral <$> i) gsize

splitMesh :: Float -> WavefrontOBJ -> WavefrontOBJ
splitMesh gsize w@WavefrontOBJ{..} = w { 
    objFaces = faces
  , objLocations = locs
  , objNormals = normals 
  , objTexCoords = uvs
  }
  where
  (faces, locs, uvs, normals) = V.foldl' merge (V.empty, V.empty, V.empty, V.empty) $ splitFace <$> objFaces

  merge (afs, als, auvs, ans) (fs, ls, uvs, ns) = (shiftIndecies <$> fs, als <> ls, auvs <> uvs, ans <> ns)
    where 
    shiftIndecies e = e { elValue = shiftIndecies' $ elValue e}
    shiftIndecies' (Face i1 i2 i3 is) = Face (shiftIndex i1) (shiftIndex i2) (shiftIndex i3) (shiftIndex <$> is)
    shiftIndex FaceIndex{..} = FaceIndex {
        faceLocIndex = faceLocIndex + V.length als 
      , faceTexCoordIndex = fmap (+ V.length auvs) faceTexCoordIndex
      , faceNorIndex = fmap (+ V.length ans) faceNorIndex
      }

  splitFace :: Element Face -> (Vector (Element Face), Vector Location, Vector TexCoord, Vector Normal)
  splitFace ef = faces
    where
    indexVertex FaceIndex{..} = case objLocations V.!? (faceLocIndex - 1) of 
      Nothing -> error $ "Cannot find vertex with id " ++ show faceLocIndex 
      Just (Location x y z _) -> case faceTexCoordIndex of 
        Nothing -> error "Model without uv, unsupported"
        Just uvi -> case objTexCoords V.!? (uvi - 1) of 
          Nothing -> error $ "Cannot find uv with id " ++ show uvi
          Just (TexCoord t u _) -> case faceNorIndex of 
            Nothing -> error "Model without normal, unsupported"
            Just ni -> case objNormals V.!? (ni - 1) of 
              Nothing -> error $ "Cannot find normal with id " ++ show ni 
              Just (Normal nx ny nz) -> (V3 x y z, V2 t u, V3 nx ny nz)

    ((v1, uv1, n1), (v2, uv2, n2), (v3, uv3, n3)) = case elValue ef of 
      Triangle i1 i2 i3 -> (indexVertex i1, indexVertex i2, indexVertex i3)
      _ -> error "Model is not triangulated"

    vs :: Vector (V3 Float, V3 Float, V3 Float)
    vs = V.foldl' (<>) V.empty . V.fromList . H.elems $ splitTriangle v1 v2 v3 gsize

    faces = V.foldl' mkFace (V.empty, V.empty, V.empty, V.empty) vs 

    wrapFace :: Face -> Element Face 
    wrapFace a = ef { elValue = a}

    mkFace :: (Vector (Element Face), Vector Location, Vector TexCoord, Vector Normal) 
      -> (V3 Float, V3 Float, V3 Float) 
      -> (Vector (Element Face), Vector Location, Vector TexCoord, Vector Normal) 
    mkFace (afs, als, auvs, ans) (v1, v2, v3) = (afs `V.snoc` ef', als', auvs', ans')
      where 
      l1 = mkLocation v1
      l2 = mkLocation v2 
      l3 = mkLocation v3
      als' = als `V.snoc` l1 `V.snoc` l2 `V.snoc` l3 
      u1 = mkTexCoord uv1 -- temp
      u2 = mkTexCoord uv1 -- temp
      u3 = mkTexCoord uv1 -- temp
      auvs' = auvs `V.snoc` u1 `V.snoc` u2 `V.snoc` u3
      n1' = mkNormal n1 -- temp
      n2' = mkNormal n1 -- temp
      n3' = mkNormal n1 -- temp
      ans' = ans `V.snoc` n1' `V.snoc` n2' `V.snoc` n3'

      ef' = wrapFace $ Triangle (mkIndex 0) (mkIndex 1) (mkIndex 2)
      mkIndex i = FaceIndex (V.length als + 1 + i) (Just $ V.length auvs + 1 + i) (Just $ V.length ans + 1 + i)

    mkLocation :: V3 Float -> Location 
    mkLocation (V3 x y z) = Location x y z 1

    mkTexCoord :: V2 Float -> TexCoord
    mkTexCoord (V2 x y) = TexCoord x y 0 

    mkNormal :: V3 Float -> Normal 
    mkNormal (V3 x y z) = Normal x y z