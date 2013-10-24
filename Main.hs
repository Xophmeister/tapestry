module Main where

import System.Environment (getArgs)
import Codec.Picture (readImage)
import Illuminate
import Encode

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> error "No arguments"
    (filename : _) -> do
      dynImage <- readImage filename
      case dynImage of
        Left  err -> error err
        Right img -> putStr $ (unlines . encodeLuminance . processImage) img
