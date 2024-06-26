module LocalReader where

import Control.Monad.Reader

myName :: String -> Reader String String
myName step = do
  name <- ask
  return $ step <> ", I am " <> name

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++ "dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)

