module Main where

import Options
import Image
import Palette
import Quantise
import Encode

main :: IO ()
main = do
  settings <- getSettings
  image    <- serialiseImage (inputFile settings) (colourSpace settings)
  palette  <- getPalette (paletteFile settings) (imageColourspace image)
  print $ quantise palette image

{- Old stuff
  dynImage <- readImage $ inputFile settings
  case dynImage of
    Left  err -> error err
    Right img -> putStr $ (unlines . encodeLuminance . processImage) img
-}
