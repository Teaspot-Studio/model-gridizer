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

import Codec.Picture as Juicy
import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL
import LambdaCube.Linear 

mainPipeline :: PipelineId 
mainPipeline = "mainPipeline"

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState initStorage
  firstLoop gs `catch` errorExit
  where 
    firstLoop gs = do 
      (_, gs') <- stepGame gs $ do
        win <- liftIO $ initWindow "Model gridizer" 1024 1080
        setCurrentWindowM $ Just win 
        lambdacubeAddPipeline [".", "./shaders"] "Phong.lc" mainPipeline $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V3F
            "normal"    @: Attribute_V3F
            "uv"        @: Attribute_V2F
          defUniforms $ do
            "modelMat"       @: M44F
            "viewMat"        @: M44F
            "projMat"        @: M44F
            "diffuseTexture" @: FTexture2D
            "lightPos"       @: V3F

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

-- | Initalizes storage and then switches to rendering state
initStorage :: AppWire a (Maybe Game)
initStorage = mkGen $ \_ _ -> do 
  (sid, storage) <- lambdacubeCreateStorage mainPipeline
  textureData <- liftIO $ do 
    -- upload geometry to GPU and add to pipeline input
    _ <- LambdaCubeGL.uploadMeshToGPU cubeMesh >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
    
    -- load image and upload texture
    Right img <- Juicy.readImage "../shared/logo.png"
    LambdaCubeGL.uploadTexture2DToGPU img

  lambdacubeRenderStorageFirst sid
  return (Right Nothing, renderWire storage textureData)

-- | Infinitely render given storage
renderWire :: GLStorage -> TextureData -> AppWire a (Maybe Game)
renderWire storage textureData = (<|> pure Nothing) $ proc _ -> do
  w <- nothingInhibit . liftGameMonad getCurrentWindowM -< ()
  closed <- isWindowClosed -< ()
  aspect <- updateWinSize -< w
  renderStorage -< aspect
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
  renderStorage :: AppWire Float ()
  renderStorage = proc aspect -> do 
    t <- timeF -< ()
    fillUniforms -< (aspect, t)
    where
    fillUniforms :: AppWire (Float, Float) ()
    fillUniforms = liftGameMonad1 $ \(aspect, t) -> liftIO $ 
      LambdaCubeGL.updateUniforms storage $ do
        "diffuseTexture" @= return textureData
        "modelMat" @= return (modelMatrix t)
        "viewMat" @= return (cameraMatrix t)
        "projMat" @= return (projMatrix aspect)
        "lightPos" @= return (V3 3 3 3 :: V3F)

  -- | Swaps frame 
  glfwFinishFrame :: AppWire GLFW.Window ()
  glfwFinishFrame = liftGameMonad1 $ liftIO . GLFW.swapBuffers

-- | Inhibits if gets Nothing
nothingInhibit :: AppWire (Maybe a) a 
nothingInhibit = mkPure_ $ \ma -> case ma of 
  Nothing -> Left ()
  Just a -> Right a