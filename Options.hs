module Options ( ColourSpace(..),
                 Settings(..),
                 getSettings ) where

-- This has been cribbed from:
-- http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt

-- At the current stage of my Haskell development, this is basically
-- black magic to me :P I'll figure it out eventually!

import System.Environment (getArgs)
import System.Exit
import System.Console.GetOpt
import System.FilePath (replaceExtension)
import Data.Ratio

data ColourSpace = Adaptive | Greyscale | Colour

data Settings = Settings { inputFile   :: FilePath,
                           paletteFile :: Maybe FilePath,
                           dpiScale    :: Rational,
                           outputFile  :: FilePath,
                           colourSpace :: ColourSpace }

settingsHelp :: ExitCode -> IO a 
settingsHelp status = do
  putStrLn $ usageInfo "Usage: tapestry [OPTIONS] FILENAME\n" options
  exitWith status

options :: [OptDescr (Settings -> IO Settings)]
options =
  [ Option "p" ["palette"]
      (ReqArg (\x i -> return i { paletteFile = Just x }) "FILENAME")
      "Palette description file",

    Option "a" ["dpi-in"]
      (ReqArg (\x i -> let scale = dpiScale i in
                       return i { dpiScale = scale * (1 % (read x :: Integer)) }) "DPI")
      "Input resolution",
      
    Option "b" ["dpi-out"]
      (ReqArg (\x i -> let scale = dpiScale i in
                       return i { dpiScale = scale * ((read x :: Integer) % 1) }) "DPI")
      "Output resolution",

    Option "o" ["output"]
      (ReqArg (\x i -> return i { outputFile = x }) "FILENAME")
      "Output file",

    Option [] ["grey"]
      (NoArg (\i -> return i { colourSpace = Greyscale }))
      "Force greyscale",

    Option [] ["colour"]
      (NoArg (\i -> return i { colourSpace = Colour }))
      "Force colour",

    Option "h" ["help"]
      (NoArg (\_ -> settingsHelp ExitSuccess)) 
      "Show this help...useful, huh?" ]

getSettings :: IO Settings
getSettings = do
  args <- getArgs
  let (actions, inputFiles, _) = getOpt Permute options args
  if null inputFiles then
    settingsHelp $ ExitFailure 1

  else do
    let defaults = Settings { inputFile   = filename,
                              paletteFile = Nothing,
                              dpiScale    = 1,
                              outputFile  = replaceExtension filename "html",
                              colourSpace = Adaptive }

                   where filename = head inputFiles

    foldl (>>=) (return defaults) actions
