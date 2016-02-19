module Main where

import Control.DeepSeq
import GHC.Generics 

import Control.Monad (join)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Proxy

import Control.Wire 
import Prelude hiding ((.), id)

import Game.GoreAndAsh
import Game.GoreAndAsh.LambdaCube
import Game.GoreAndAsh.GLFW 

import Core 
import Matrix
import Loader

import qualified Graphics.UI.GLFW as GLFW 

import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL
import LambdaCube.Linear 

mainPipeline :: PipelineId 
mainPipeline = "mainPipeline"

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  let gridSize = 1
  gs <- newGameState $ mainWire gridSize
  firstLoop gs `catch` errorExit
  where 
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

mainWire :: Float ->  AppWire a (Maybe Game)
mainWire gridSize = withInit (const initStorage) (renderWire gridSize)

-- | Initalizes storage and then switches to rendering state
initStorage :: GameMonadT AppMonad (GLStorage, GPUMesh)
initStorage = do 
  (sid, storage) <- lambdacubeCreateStorage mainPipeline
  gpuMesh <- liftIO $ LambdaCubeGL.uploadMeshToGPU cubeMesh
  lambdacubeRenderStorageLast sid
  return (storage, gpuMesh)

-- | Infinitely render given storage
renderWire :: Float -> (GLStorage, GPUMesh) -> AppWire a (Maybe Game)
renderWire gridSize (storage, gpuMesh) = (<|> pure Nothing) $ proc _ -> do
  w <- nothingInhibit . liftGameMonad getCurrentWindowM -< ()
  closed <- isWindowClosed -< ()
  aspect <- updateWinSize -< w
  t <- timeF -< ()
  globalUniforms -< (aspect, t)
  cube storage gpuMesh -< ()
  grid storage gpuMesh gridSize -< ()
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
  globalUniforms = liftGameMonad1 $ \(aspect, t) -> liftIO $ 
    LambdaCubeGL.updateUniforms storage $ do
      "viewMat" @= return (cameraMatrix t)
      "projMat" @= return (projMatrix aspect)
      "lightPos" @= return (V3 3 3 3 :: V3F)

  -- | Swaps frame 
  glfwFinishFrame :: AppWire GLFW.Window ()
  glfwFinishFrame = liftGameMonad1 $ liftIO . GLFW.swapBuffers

-- | Intializes and renders cube
cube :: GLStorage -> GPUMesh -> AppWire a ()
cube storage gpuMesh = withInit (const initCube) renderCube
  where
  initCube :: GameMonadT AppMonad Object
  initCube = do 
    -- upload geometry to GPU and add to pipeline input
    liftIO $
      LambdaCubeGL.addMeshToObjectArray storage "objects" ["modelMat", "wireOnly"] gpuMesh

  -- | Update object specific uniforms
  renderCube :: Object -> AppWire a ()
  renderCube obj = (timeF >>>) $ liftGameMonad1 $ \t -> liftIO $ do 
    let setter = LambdaCubeGL.objectUniformSetter obj
    uniformM44F "modelMat" setter $ modelMatrix t
    uniformBool "wireOnly" setter False

-- | Intializes and renders grid
grid :: GLStorage -> GPUMesh -> Float -> AppWire a ()
grid storage gpuMesh _ = withInit (const initGrid) renderGrid
  where
  initGrid :: GameMonadT AppMonad Object
  initGrid = do 
    -- upload geometry to GPU and add to pipeline input
    liftIO $
      LambdaCubeGL.addMeshToObjectArray storage "objects" ["modelMat", "wireOnly"] gpuMesh

  -- | Update object specific uniforms
  renderGrid :: Object -> AppWire a ()
  renderGrid obj = (timeF >>>) $ liftGameMonad1 $ \t -> liftIO $ do 
    let setter = LambdaCubeGL.objectUniformSetter obj
    uniformM44F "modelMat" setter $ modelMatrix t
    uniformBool "wireOnly" setter True