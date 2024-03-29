module CountEntries (listDirectory, countEntries) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)

-- Variations: liftM, fmap, <$>

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter noDots) . getDirectoryContents
  where noDots p = p /= "." && p /= ".."

listDirectory' :: FilePath -> IO [String]
listDirectory' = fmap (filter noDots) . getDirectoryContents
  where noDots p = p /= "." && p /= ".."

listDirectory'' :: FilePath -> IO [String]
listDirectory'' path = filter noDots <$> getDirectoryContents path
  where noDots p = p /= "." && p /= ".."

countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntries newName
              else return []
  return $ (path, length contents) : concat rest
