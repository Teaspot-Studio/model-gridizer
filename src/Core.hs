module Core where

import Control.DeepSeq
import GHC.Generics 

import Control.Monad.Catch 
import Control.Monad.Fix 
import Control.Monad.IO.Class 
import Data.Proxy 

import Control.Wire 
import Prelude hiding ((.), id)

import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW 
import Game.GoreAndAsh.LambdaCube 

-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [GLFWT, LambdaCubeT] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadLambdaCube, MonadGLFW)

instance GameModule AppMonad AppState where 
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do 
    (a, s') <- runModule m s 
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s 

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b

-- | Helper to run initalization step for wire
-- TODO: move to core package
withInit :: (c -> GameMonadT AppMonad a) -> (a -> AppWire c b) -> AppWire c b 
withInit initStep nextStep = mkGen $ \s c -> do 
  a <- initStep c
  (mb, nextStep') <- stepWire (nextStep a) s (Right c)
  return (mb, nextStep')

-- | Inhibits if gets Nothing
nothingInhibit :: AppWire (Maybe a) a 
nothingInhibit = mkPure_ $ \ma -> case ma of 
  Nothing -> Left ()
  Just a -> Right a