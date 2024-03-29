module CountEntriesT where

import CountEntries (listDirectory)
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

-- WriterT implements MonadIO, that provides liftIO
-- liftIO :: MonadIO m => IO a -> m a

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

