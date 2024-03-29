module Main where

import Control.Monad (forM_)
import Control.Monad.Writer.Lazy (execWriterT)
import qualified CountEntries as CE
import qualified CountEntriesT as CET
import System.Directory (getCurrentDirectory)
import qualified System.Environment as Env

main :: IO ()
main = do
  args <- Env.getArgs
  dir <- getCurrentDirectory
  case args of
    ["CountEntries"] -> do
      tuples <- CE.countEntries dir
      forM_ tuples $ \(path, count) -> putStrLn $ path <> " " <> show count

    ["CountEntriesT"] -> do
      tuples <- execWriterT $ CET.countEntries dir
      forM_ tuples $ \(path, count) -> putStrLn $ path <> " " <> show count

    _ -> do
      putStrLn "Wrong arguments. Example: cabal exec ch18 -- CountEntries"
