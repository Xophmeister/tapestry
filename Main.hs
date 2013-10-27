module Main where

import Codec.Picture (readImage)
import Options
import Illuminate
import Encode

main :: IO ()
main = do
  settings <- getSettings
  dynImage <- readImage $ inputFile settings
  case dynImage of
    Left  err -> error err
    Right img -> putStr $ (unlines . encodeLuminance . processImage) img
