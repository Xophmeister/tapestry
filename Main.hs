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
  writeFile (outputFile settings) $ encode palette (quantise palette image)
