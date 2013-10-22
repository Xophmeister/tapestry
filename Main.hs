module Main where

import System.Environment (getArgs)
import Codec.Picture

showDetails :: DynamicImage -> IO ()
showDetails (ImageRGBA8 img) = do
  print $ imageData img
--showDetails img = promoteImage img
showDetails _ = print "Unhandled colourspace"

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> putStrLn "No arguments"
    (filename : _) -> do
      dynImage <- readImage filename
      case dynImage of
        Left  err -> putStrLn err
        Right img -> showDetails img
