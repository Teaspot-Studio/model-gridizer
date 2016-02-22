module Camera(
    Camera(..)
  , cameraMoveForward
  , cameraMoveLeft
  , cameraMoveRight
  , cameraRotateYaw
  , cameraRotatePitch
  ) where

import Control.DeepSeq
import GHC.Generics 
import Linear

data Camera = Camera {
    cameraUp :: !(V3 Float)
  , cameraForward :: !(V3 Float)
  , cameraEye :: !(V3 Float)
  } deriving (Generic)

instance NFData Camera

cameraRight :: Camera -> V3 Float 
cameraRight Camera{..} = normalize $ cameraForward `cross` cameraUp 

cameraLeft :: Camera -> V3 Float 
cameraLeft Camera{..} = normalize $ cameraUp `cross` cameraForward

cameraMoveForward :: Float -> Camera -> Camera 
cameraMoveForward v c = c { cameraEye = cameraEye c + fmap (v*) (cameraForward c) }

cameraMoveLeft :: Float -> Camera -> Camera 
cameraMoveLeft v c = c { cameraEye = cameraEye c + fmap (v*) (cameraLeft c) }

cameraMoveRight :: Float -> Camera -> Camera 
cameraMoveRight v c = c { cameraEye = cameraEye c + fmap (v*) (cameraRight c) }

cameraRotateYaw :: Float -> Camera -> Camera 
cameraRotateYaw v c = c { cameraForward = rotate (axisAngle (cameraUp c) (-v)) $ cameraForward c }

cameraRotatePitch :: Float -> Camera -> Camera 
cameraRotatePitch v c = c { cameraForward = rotate (axisAngle (cameraRight c) v) $ cameraForward c }