module Matrix(
    modelMatrix
  , cameraMatrix
  , projMatrix
  , gridModelMatrix
  ) where

import Linear
import qualified LambdaCube.Linear as LC 
import Game.GoreAndAsh.Math

import Camera 

-- | Convert from linear matrix format to LambdaCube format
convLC :: M44 Float -> LC.M44F 
convLC (V4 !a !b !c !d) =  LC.V4 (cv a) (cv b) (cv c) (cv d)
  where
    cv (V4 !x !y !z !w) = LC.V4 x y z w

-- | Model matrix, maps from local model coords to world coords
modelMatrix :: Float -> LC.M44F 
modelMatrix _ = convLC identity 

-- | Grid is static
gridModelMatrix :: Camera -> Float -> LC.M44F 
gridModelMatrix Camera{..} gridSize = convLC $ translate (fromIntegral . (round :: Float -> Int) . (/ gridSize) <$> cameraEye)

-- | Camera matrix, maps from world coords to camera coords
cameraMatrix :: Camera -> LC.M44F 
cameraMatrix Camera{..} = convLC $ lookAt cameraEye (cameraEye + cameraForward) cameraUp

-- | Projection matrix, maps from camera coords to device normalized coords
projMatrix :: Float -> LC.M44F 
projMatrix !aspect = convLC $ perspective (pi/3) aspect 0.1 100