module Main where

import Control.Monad (forM_)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer.Lazy (execWriterT)
import qualified CountEntries as CE
import qualified CountEntriesT as CET
import qualified LocalReader as LR
import qualified UglyStack as US
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

    ["LocalReader"] -> do
      putStrLn $ show $ runReader LR.localExample "Fred"

    ["UglyStack"] -> do
      tuples <- fst <$> US.runApp (US.constrainedCount 0 dir) 3
      forM_ tuples $ \(path, count) -> putStrLn $ path <> " " <> show count

    _ -> do
      putStrLn "Wrong arguments. Example: cabal exec ch18 -- CountEntries"
