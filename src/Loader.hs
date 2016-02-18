module Loader(
    cubeMesh
  ) where

import Prelude hiding ((.), id)

import qualified Data.Map as Map
import qualified Data.Vector as V

--import Codec.Picture as Juicy
import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL
--import LambdaCube.Linear 


-- geometry data: triangles
cubeMesh :: LambdaCubeGL.Mesh
cubeMesh = Mesh
  { mAttributes   = Map.fromList
      [ ("position",  A_V3F $ V.fromList vertecies)
      , ("normal",    A_V3F $ V.fromList normals)
      , ("uv",        A_V2F $ V.fromList uvs)
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
  uvs = concat $ replicate 6 [u1, u2, u3, u1, u3, u0]

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

  u0 = V2 0 0 
  u1 = V2 1 0 
  u2 = V2 1 1 
  u3 = V2 0 1