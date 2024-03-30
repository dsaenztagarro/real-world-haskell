{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UglyStack where

import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig
  { cfgMaxDepth :: Int
  } deriving (Show)

data AppState = AppState
  { stDeepestReached :: Int
  } deriving (Show)

newtype MyApp a = MyA
  { runApp :: ReaderT AppConfig (StateT AppState IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

-- In Haskell, the `ReaderT` transformer itself does not have an instance of `MonadState`.
-- Instead, it relies on the underlying monad (in this case, `IO`) to provide the instance for `MonadState`.

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp app maxDepth =
  let config = AppConfig maxDepth
      st = AppState 0
  in runStateT (runReaderT (runApp app) config) st

constrainedCount :: Int -> FilePath -> MyApp [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $
          put st { stDeepestReached = newDepth }
        constrainedCount newDepth newPath
      else return []
  return $ (path, length contents) : concat rest
