module Loader(
    cubeMesh
  , gridMesh
  , loadObjMesh
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Foldable as F 
import Data.Monoid 

import Codec.Wavefront
import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL

loadObjMesh :: MonadIO m => FilePath -> m (Either String Mesh)
loadObjMesh objFile = do 
  eobj <- fromFile objFile
  return . join $ parseObjMesh <$> eobj

type VecNormAccum = (V.Vector (V3 Float), V.Vector (V3 Float))

parseObjMesh :: WavefrontOBJ -> Either String Mesh 
parseObjMesh WavefrontOBJ{..} = mkMesh <$> F.foldlM accumFaces (V.empty, V.empty) objFaces 
  where
    mkMesh :: VecNormAccum -> Mesh
    mkMesh (vs, ns) = Mesh {
      mAttributes = Map.fromList 
        [ ("position", A_V3F vs)
        , ("normal",   A_V3F ns)
        ]
      , mPrimitive = P_Triangles
      }

    accumFaces :: VecNormAccum -> Element Face -> Either String VecNormAccum
    accumFaces (vs, ns) f = do
      (vs', ns') <- mkFace . elValue $ f
      return $ (vs <> vs', ns <> ns')

    mkFace :: Face -> Either String (V.Vector (V3 Float), V.Vector (V3 Float))
    mkFace (Triangle f1 f2 f3) = do
      (v1, n1) <- mkVert f1 
      (v2, n2) <- mkVert f2 
      (v3, n3) <- mkVert f3 
      return (V.fromList [v1, v2, v3], V.fromList [n1, n2, n3])
    mkFace _ = Left "Converter support only triangles, triangulate your OBJ file"
    
    mkVert :: FaceIndex -> Either String (V3 Float, V3 Float)
    mkVert FaceIndex{..} = case faceNorIndex of 
      Nothing -> Left "OBJ model must have normals for each vertex"
      Just ni -> case objLocations V.!? faceLocIndex of 
        Nothing -> Left $ "Cannot find vertex with id " ++ show faceLocIndex 
        Just (Location lx ly lz _) -> case objNormals V.!? ni of
          Nothing -> Left $ "Cannot find normal with id " ++ show ni 
          Just (Normal nx ny nz) -> Right (V3 lx ly lz, V3 nx ny nz)

gridMesh :: Float -> Mesh 
gridMesh s = Mesh {
    mAttributes = Map.fromList 
      [ ("position", A_V3F $ vertecies)
      , ("normal",   A_V3F $ normals)
      ]
  , mPrimitive = P_Triangles
  }
  where
  d = fromIntegral n
  n = 3
  vertecies = planes <> zlines
  normals = V.replicate (length vertecies) (V3 1 0 0)

  planes = V.foldl' (\acc z -> acc <> ylines z <> xlines z) V.empty (V.fromList [-n .. n])
  ylines z = V.foldl' (\acc x -> acc <> yline (fromIntegral z) (fromIntegral x)) V.empty (V.fromList [-n .. n])
  xlines z = V.foldl' (\acc y -> acc <> xline (fromIntegral z) (fromIntegral y)) V.empty (V.fromList [-n .. n])
  zlines = foldl2D n n $ \acc x y -> acc <> zline (fromIntegral x) (fromIntegral y)

  yline z x = V.fromList [V3 (s*x) (-d) (s*z), V3 (s*x) d (s*z), V3 (s*x) d (s*z)]
  xline z y = V.fromList [V3 (-d) (s*y) (s*z), V3 d (s*y) (s*z), V3 d (s*y) (s*z)]
  zline x y = V.fromList [V3 (s*x) (s*y) (-d), V3 (s*x) (s*y) d, V3 (s*x) (s*y) d]

  foldl2D :: Int -> Int -> (V.Vector a -> Int -> Int -> V.Vector a) -> V.Vector a
  foldl2D nx ny f = V.foldl' (\accx x -> V.foldl' (\accy y -> f accy x y) accx (V.fromList [-ny .. ny])) V.empty (V.fromList [-nx .. nx])

-- geometry data: triangles
cubeMesh :: Mesh
cubeMesh = Mesh
  { mAttributes   = Map.fromList
      [ ("position",  A_V3F $ V.fromList vertecies)
      , ("normal",    A_V3F $ V.fromList normals)
      ]
  , mPrimitive    = P_Triangles
  }
  where 
  vertecies = [
      v3, v2, v1, v3, v1, v0
    , v4, v7, v6, v4, v6, v5
    , v0, v1, v7, v0, v7, v4
    , v5, v6, v2, v5, v2, v3
    , v2, v6, v7, v2, v7, v1
    , v5, v3, v0, v5, v0, v4
    ]
  normals = concat [
      replicate 6 n0
    , replicate 6 n1
    , replicate 6 n2
    , replicate 6 n3
    , replicate 6 n4
    , replicate 6 n5
    ]

  v0 = V3 (-1) (-1) (-1)
  v1 = V3 (-1)   1  (-1)
  v2 = V3   1    1  (-1)
  v3 = V3   1  (-1) (-1)
  v4 = V3 (-1) (-1)   1
  v5 = V3   1  (-1)   1
  v6 = V3   1    1    1
  v7 = V3 (-1)   1    1

  n0 = V3   0    0  (-1)
  n1 = V3   0    0    1 
  n2 = V3 (-1)   0    0
  n3 = V3   1    0    0 
  n4 = V3   0    1    0
  n5 = V3   0  (-1)   0