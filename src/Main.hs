module Main where

import Control.DeepSeq
import GHC.Generics 

import Control.Monad (join)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Proxy
import System.Environment
import Text.Read

import Control.Wire 
import Prelude hiding ((.), id)

import Game.GoreAndAsh
import Game.GoreAndAsh.LambdaCube
import Game.GoreAndAsh.GLFW 

import Core 
import Matrix
import Loader
import Camera 

import qualified Graphics.UI.GLFW as GLFW 

import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL
import LambdaCube.Linear 

import qualified Linear as L 

mainPipeline :: PipelineId 
mainPipeline = "mainPipeline"

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  (gridSize, objMeshName) <- parseArgs
  gs <- newGameState $ mainWire gridSize objMeshName
  firstLoop gs `catch` errorExit
  where 
    parseArgs = do 
      args <- getArgs 
      case args of 
        [a, b] -> case readMaybe a of 
          Nothing -> fail "Failed to parse grid size, not float"
          Just gridSize -> return (gridSize, b)
        _ -> fail "Expected two input arguments: grid size (float) and OBJ model path"

    firstLoop gs = do 
      (_, gs') <- stepGame gs $ do
        win <- liftIO $ initWindow "Model gridizer" 1024 1080
        setCurrentWindowM $ Just win 
        lambdacubeAddPipeline [".", "./shaders"] "Main.lc" mainPipeline $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V3F
            "normal"    @: Attribute_V3F
          defUniforms $ do
            "modelMat"       @: M44F
            "viewMat"        @: M44F
            "projMat"        @: M44F
            "lightPos"       @: V3F
            "wireOnly"       @: Bool

        return ()
      gameLoop gs'

    errorExit e = do 
      liftIO $ case e of 
        PipeLineCompileFailed _ _ msg -> putStrLn msg
        PipeLineAlreadyRegistered i -> putStrLn $ "Pipeline already registered: " ++ show i
        PipeLineNotFound i -> putStrLn $ "Pipeline is not found: " ++ show i 
        StorageNotFound i -> putStrLn $ "Storage is not found: " ++ show i 
        PipeLineIncompatible _ msg -> putStrLn $ "Pipeline incompatible: " ++ msg
      fail "terminate: fatal error"

    gameLoop gs = do
      (mg, gs') <- stepGame gs (return ())
      mg `deepseq` if fromMaybe False $ gameExit <$> join mg
        then cleanupGameState gs'
        else gameLoop gs'

initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    _ <- GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ GLFW.WindowHint'ContextVersionMajor 3
      , GLFW.WindowHint'ContextVersionMinor 3
      , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      , GLFW.WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win

data Game = Game {
    gameExit :: Bool
  }
  deriving (Generic)

instance NFData Game 

mainWire :: Float -> FilePath -> AppWire a (Maybe Game)
mainWire gridSize objMeshName = withInit (const initStorage) (renderWire gridSize objMeshName)

-- | Initalizes storage and then switches to rendering state
initStorage :: GameMonadT AppMonad GLStorage
initStorage = do 
  (sid, storage) <- lambdacubeCreateStorage mainPipeline
  lambdacubeRenderStorageLast sid
  return storage

-- | Infinitely render given storage
renderWire :: Float -> FilePath -> GLStorage -> AppWire a (Maybe Game)
renderWire gridSize objMeshName storage = (<|> pure Nothing) $ proc _ -> do
  w <- nothingInhibit . liftGameMonad getCurrentWindowM -< ()
  closed <- isWindowClosed -< ()
  aspect <- updateWinSize -< w
  t <- timeF -< ()
  globalUniforms -< (aspect, t)
  cam <- camera storage -< ()
  model storage objMeshName -< ()
  grid storage gridSize -< cam
  glfwFinishFrame -< w
  returnA -< Just $ Game closed
  where
  -- | Outputs True if user hits close button
  isWindowClosed :: AppWire a Bool
  isWindowClosed = hold . mapE (const True) . windowClosing <|> pure False

  -- | Updates LambdaCube window size
  updateWinSize :: AppWire GLFW.Window Float
  updateWinSize = liftGameMonad1 $ \win -> do 
    (w, h) <- liftIO $ GLFW.getWindowSize win
    lambdacubeUpdateSize (fromIntegral w) (fromIntegral h)
    return $ fromIntegral w / fromIntegral h

  -- | Updates storage uniforms
  globalUniforms :: AppWire (Float, Float) ()
  globalUniforms = liftGameMonad1 $ \(aspect, _) -> liftIO $ 
    LambdaCubeGL.updateUniforms storage $ do
      "projMat" @= return (projMatrix aspect)
      "lightPos" @= return (V3 3 3 3 :: V3F)

  -- | Swaps frame 
  glfwFinishFrame :: AppWire GLFW.Window ()
  glfwFinishFrame = liftGameMonad1 $ liftIO . GLFW.swapBuffers

-- | Intializes and renders obj mesh
model :: GLStorage -> FilePath -> AppWire a ()
model storage objMeshName = withInit (const initModel) renderModel
  where
  initModel :: GameMonadT AppMonad Object
  initModel = liftIO $ do 
    mmesh <- loadObjMesh objMeshName
    case mmesh of 
      Left er -> fail er 
      Right modelMesh -> do 
        gpuMesh <- liftIO $ LambdaCubeGL.uploadMeshToGPU modelMesh
        LambdaCubeGL.addMeshToObjectArray storage "objects" ["modelMat", "wireOnly"] gpuMesh
    
  -- | Update object specific uniforms
  renderModel :: Object -> AppWire a ()
  renderModel obj = (timeF >>>) $ liftGameMonad1 $ \t -> liftIO $ do 
    let setter = LambdaCubeGL.objectUniformSetter obj
    uniformM44F "modelMat" setter $ modelMatrix t
    uniformBool "wireOnly" setter False

-- | Intializes and renders grid
grid :: GLStorage -> Float -> AppWire Camera ()
grid storage size = withInit (const initGrid) renderGrid
  where
  initGrid :: GameMonadT AppMonad Object
  initGrid = liftIO $ do 
    gpuMesh <- liftIO $ LambdaCubeGL.uploadMeshToGPU $ gridMesh size
    LambdaCubeGL.addMeshToObjectArray storage "objects" ["modelMat", "wireOnly"] gpuMesh

  -- | Update object specific uniforms
  renderGrid :: Object -> AppWire Camera ()
  renderGrid obj = liftGameMonad1 $ \cam -> liftIO $ do 
    let setter = LambdaCubeGL.objectUniformSetter obj
    uniformM44F "modelMat" setter $ gridModelMatrix cam size
    uniformBool "wireOnly" setter True

-- | Camera control
camera :: GLStorage -> AppWire a Camera
camera storage = proc _ -> do 
  updUniforms -< initalCamera
  returnA -< initalCamera
  where
    initalCamera = Camera (L.V3 0 1 0) (L.normalize $ L.V3 (-5) (-2) (-5)) (L.V3 5 2 5)

    updUniforms = liftGameMonad1 $ \cam -> liftIO $ do
      LambdaCubeGL.updateUniforms storage $ do
        "viewMat" @= return (cameraMatrix cam)