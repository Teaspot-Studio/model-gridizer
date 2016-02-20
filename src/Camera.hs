module Camera(
    Camera(..)
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