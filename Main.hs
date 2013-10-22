module Main where

import System.Environment (getArgs)
import Codec.Picture

main :: IO ()
main = do
  argv <- getArgs
  print $ show argv
