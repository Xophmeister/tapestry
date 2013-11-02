module Main where

import Options
import Image
import Palette hiding (tokenise)
import Quantise
import Encode

main :: IO ()
main = do
  settings <- getSettings
  image    <- serialiseImage (inputFile settings) (colourSpace settings)
  palette  <- getPalette (paletteFile settings) (imageColourspace image)
  putStr $ encode palette (quantise palette image)
