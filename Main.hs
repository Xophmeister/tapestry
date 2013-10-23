module Main where

import System.Environment (getArgs)
import Codec.Picture

import Illuminate

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> putStrLn "No arguments"
    (filename : _) -> do
      dynImage <- readImage filename
      case dynImage of
        Left  err -> putStrLn err
        Right img -> print $ toLuminance img
