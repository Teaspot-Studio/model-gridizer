module Splitter(
    splitMesh
  , debugMesh
  ) where

import Codec.Wavefront hiding (Line)
import Control.DeepSeq
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy, nubBy)
import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Data.Ord (comparing)
import Data.Vector (Vector)
import Linear
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Debug.Trace
import qualified LambdaCube.GL as LC
import qualified LambdaCube.GL.Mesh as LC
import qualified Data.Map as Map

import Plane
import Triangulate

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Test if given triple of points form conter clockwise triangle
isCCW :: V3 Float -- ^ Original normal
  -> V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> Bool -- ^ True if conter clockwise order
isCCW n v1 v2 v3 = (((v2 - v1) `cross` (v3 - v1)) `dot` n) >= 0

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
    gridOrigin = (gsize *) . fromIntegral <$> toGridOrigin lbeg (lend - lbeg) gsize

toGridOrigin :: V3 Float -- ^ Point
  -> V3 Float -- ^ Direction
  -> Float -- ^ Grid size
  -> V3 Int -- ^ Origin of grid cube that contains the point
toGridOrigin v@(V3 x y z) (V3 dx dy dz) gsize = V3 xi' yi' zi'
  where
    V3 xi yi zi = fmap (floor . (/ gsize)) v
    xi' | x - gsize * fromIntegral xi == 0.0 = if dx > 0 then xi else xi - 1
        | x - gsize * fromIntegral xi == 1.0 = if dx > 0 then xi + 1 else xi
        | otherwise = xi

    yi' | y - gsize * fromIntegral yi == 0.0 = if dy > 0 then yi else yi - 1
        | y - gsize * fromIntegral yi == 1.0 = if dy > 0 then yi + 1 else yi
        | otherwise = yi

    zi' | z - gsize * fromIntegral zi == 0.0 = if dz > 0 then zi else zi - 1
        | z - gsize * fromIntegral zi == 1.0 = if dz > 0 then zi + 1 else zi
        | otherwise = zi

-- | Split line into parts by grid
splitLine :: V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> Float -- ^ Grid size
  -> HashMap (V3 Int) (Vector (V3 Float)) -- ^ Separated points, points are duplicated at edges for each box
splitLine v1 v2 gsize = go H.empty (toGridOrigin v1 (v2 - v1) gsize) v1
  where

  go :: HashMap (V3 Int) (Vector (V3 Float)) -> V3 Int -> V3 Float -> HashMap (V3 Int) (Vector (V3 Float))
  go acc i v = case lineCrossGrid v v2 gsize of
    Nothing -> append i v2 . append i v $ acc
    Just (v', _) -> let
      origin = toGridOrigin v' dv gsize
      acc' = append i v' . append i v $ acc
      in go acc' origin v'
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
splitTriangle v1 v2 v3 gsize =
  fmap (V.filter (uncurry3 $ isCCW normal) . triangulate) . H.mapWithKey addCut $
    traceShow ("v1 v2", splitLine v1 v2 gsize ) splitLine v1 v2 gsize
    `merge`
    traceShow ("v2 v3", splitLine v2 v3 gsize )splitLine v2 v3 gsize
    `merge`
    traceShow ("v3 v1", splitLine v3 v1 gsize ) splitLine v3 v1 gsize
  where
  merge = H.unionWith (<>)
  normal = (v2 - v1) `cross` (v3 - v1)

  addCut :: V3 Int -> Vector (V3 Float) -> Vector (V3 Float)
  addCut i vs = nubVecs $ vs <> traceShow ("cuts", cuts) cuts
    where cuts = triangleCut v1 v2 v3 ((* gsize) . fromIntegral <$> i) gsize
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
    vs = V.concat . V.toList $ (\(a, b, c) -> V.fromList [a, b, c]) <$>
      splitTriangle' v1 v2 v3 gsize
    ns = repeat $ V3 0 1 0.0

splitMesh :: Float -> WavefrontOBJ -> WavefrontOBJ
splitMesh gsize w@WavefrontOBJ{..} = traceShow test $ w {
    objFaces = traceShow ("faces", V.length faces) faces
  , objLocations = locs
  , objNormals = normals
  , objTexCoords = uvs
  }
  where
  testPlane = planeFromPoints (V3 0 0 0) (V3 0 0 1) (V3 1 0 1)
  testLine = lineFromPoints (V3 0 0 0) (V3 1 0.0 0)
  test = pointInPlane (V3 0 0 0) testPlane
  (faces, locs, uvs, normals) = V.foldl' merge (V.empty, V.empty, V.empty, V.empty) $ splitFace <$> objFaces

  merge (afs, als, auvs, ans) (fs, ls, uvs, ns) = (afs <> fmap shiftIndecies fs, als <> ls, auvs <> uvs, ans <> ns)
    where
    shiftIndecies e = e { elValue = shiftIndecies' $ elValue e}
    shiftIndecies' (Face i1 i2 i3 is) = Face (shiftIndex i1) (shiftIndex i2) (shiftIndex i3) (shiftIndex <$> is)
    shiftIndex FaceIndex{..} = FaceIndex {
        faceLocIndex = faceLocIndex + V.length als
      , faceTexCoordIndex = fmap (+ V.length auvs) faceTexCoordIndex
      , faceNorIndex = fmap (+ V.length ans) faceNorIndex
      }

  splitFace :: Element Face -> (Vector (Element Face), Vector Location, Vector TexCoord, Vector Normal)
  splitFace ef = faces `deepseq` faces
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

instance NFData TexCoord where
  rnf (TexCoord t u w) = t `seq` u `seq` w `seq` ()

instance NFData Location where
  rnf (Location x y z w) = x `seq` y `seq` z `seq` w `seq` ()

instance NFData Normal where
  rnf (Normal x y z) = x `seq` y `seq` z `seq` ()

instance NFData a => NFData (Element a) where
  rnf Element{..} = elObject
    `deepseq` elGroups
    `deepseq` elMtl
    `deepseq` elSmoothingGroup
    `deepseq` elValue
    `deepseq` ()

instance NFData Face where
  rnf (Face a b c ds) = a `deepseq` b `deepseq` c `deepseq` ds `deepseq` ()

instance NFData FaceIndex where
  rnf FaceIndex{..} = faceLocIndex
    `deepseq` faceTexCoordIndex
    `deepseq` faceNorIndex
    `deepseq` ()
