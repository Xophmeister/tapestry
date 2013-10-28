module Options ( ColourSpace,
                 Settings(..),
                 getSettings ) where

-- This has been cribbed from:
-- http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt

-- At the current stage of my Haskell development, this is basically
-- black magic to me :P I'll figure it out eventually!

import System.Environment (getArgs)
import System.Exit
import System.Console.GetOpt

data ColourSpace = Adaptive | ForceGrey | ForceColour

data Settings = Settings { inputFile   :: String,
                           paletteFile :: Maybe String,
                           inputDPI    :: Int,
                           outputDPI   :: Int,
                           outputFile  :: String,
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
      (ReqArg (\x i -> return i { inputDPI = read x :: Int }) "DPI")
      "Input resolution",
      
    Option "b" ["dpi-out"]
      (ReqArg (\x i -> return i { outputDPI = read x :: Int }) "DPI")
      "Output resolution",

    Option "o" ["output"]
      (ReqArg (\x i -> return i { outputFile = x }) "FILENAME")
      "Output file",

    Option [] ["grey"]
      (NoArg (\i -> return i { colourSpace = ForceGrey }))
      "Force greyscale",

    Option [] ["colour"]
      (NoArg (\i -> return i { colourSpace = ForceColour }))
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
    let defaults = Settings { inputFile   = head inputFiles,
                              paletteFile = Nothing,
                              inputDPI    = 1,
                              outputDPI   = 1,
                              outputFile  = "tapestry.html",
                              colourSpace = Adaptive }
    foldl (>>=) (return defaults) actions
