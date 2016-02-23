module Splitter(
    splitMesh
  , debugMesh
  ) where

import Linear 
import Codec.Wavefront hiding (Line)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as V 
import qualified Data.HashMap.Strict as H 
import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Data.List (sortBy, nubBy)
import Data.Ord (comparing)

import Debug.Trace 
import qualified LambdaCube.GL as LC
import qualified LambdaCube.GL.Mesh as LC
import qualified Data.Map as Map 

import Plane 
import Triangulate 

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
    Just (v', _) -> go (append i v' . append i v $ acc) (toGridOrigin (v' + dv) gsize) (v' + dv)
    where 
    dv = fmap (0.00001*) . normalize $ v2 - v

  append :: V3 Int -> V3 Float -> HashMap (V3 Int) (Vector (V3 Float)) -> HashMap (V3 Int) (Vector (V3 Float))
  append k v m = case H.lookup k m of 
    Nothing -> H.insert k (V.singleton v) m 
    Just vs -> case V.find (approxEq v) vs of 
      Nothing -> H.insert k (vs `V.snoc` v) m
      Just _ -> m

-- | Splits triangle by grid to several triangles
--
-- Note: points should be passed in CCW order
splitTriangle :: 
     V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> Float -- ^ Grid size
  -> HashMap (V3 Int) (Vector (V3 Float, V3 Float, V3 Float)) -- ^ Triangles by grid boxes
splitTriangle v1 v2 v3 gsize = fmap triangulate . H.mapWithKey addCut $ 
  splitLine v1 v2 gsize `merge` splitLine v2 v3 gsize `merge` splitLine v3 v1 gsize
  where
  merge = H.unionWith (<>)

  addCut :: V3 Int -> Vector (V3 Float) -> Vector (V3 Float)
  addCut i vs = nubVecs $ vs <> (triangleCut v1 v2 v3 ((* gsize) . fromIntegral <$> i) gsize)

-- | Simplified version without boxing to grid cells
splitTriangle' :: 
     V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> Float -- ^ Grid size
  -> Vector (V3 Float, V3 Float, V3 Float)
splitTriangle' v1 v2 v3 gsize = V.foldl' (<>) V.empty . V.fromList . H.elems $ splitTriangle v1 v2 v3 gsize

debugMesh :: Float -> LC.Mesh 
debugMesh gsize = LC.Mesh {
  LC.mAttributes = Map.fromList 
    [ ("position", LC.A_V3F $ fmap toLC vs)
    , ("normal",   LC.A_V3F $ fmap toLC $ V.fromList $ take (V.length vs) ns)
    ]
  , LC.mPrimitive = LC.P_Triangles
  }
  where 
    toLC (V3 x y z) = LC.V3 x y z
    v1 = V3 (-0.5) 0.5 0
    v2 = V3 1 1.5 1.1 
    v3 = V3 0.5 1 2
    vs = V.concat . V.toList $ fmap (\(a, b, c) -> V.fromList [a, b, c]) $ 
      splitTriangle' v1 v2 v3 gsize
    ns = repeat $ V3 0 1 0.0

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
    vs = splitTriangle' v1 v2 v3 gsize

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