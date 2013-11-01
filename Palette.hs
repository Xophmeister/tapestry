{-# LANGUAGE FlexibleInstances #-}
module Palette ( Palette(..),
                 getPalette,
                 tokenise ) where

import System.Exit
import Data.List
import Codec.Picture.Types
import Options (ColourSpace(..))
import Image (ColourStream(..))

type Tokens  = [Char]
data Palette = Palette Tokens ColourStream

-- Default palette is logarithmic white to black
defaultPalette :: Palette
defaultPalette = Palette ['\9633', '\9675', '\9678', '\9673', '\9632', '\9679', '\10070', '\9733'] 
                  (Greys [   247,     230,     210,     185,     156,     121,       78,      28 ])

convertPalette :: Palette -> ColourSpace -> Palette
convertPalette palette@(Palette _ (Greys   _))    Greyscale = palette
convertPalette palette@(Palette _ (Colours _))    Colour    = palette
convertPalette (Palette tokens (Greys   colours)) Colour    = Palette tokens (Colours $ map promotePixel colours)
convertPalette (Palette tokens (Colours colours)) Greyscale = Palette tokens (Greys   $ map computeLuma  colours)

getPalette :: Maybe FilePath -> ColourSpace -> IO Palette
getPalette Nothing colourspace = return $ convertPalette defaultPalette colourspace
getPalette (Just _) _ = do putStrLn "Palette loading not yet implemented!"
                           exitFailure

class Colour a where
  tokenise :: Palette -> a -> Maybe Char 

instance Colour Pixel8 where
  tokenise (Palette tokens (Greys palette)) = getToken palette tokens

instance Colour PixelRGB8 where
  tokenise (Palette tokens (Colours palette)) = getToken palette tokens

getToken :: (Eq a, Colour a) => [a] -> [Char] -> a -> Maybe Char
getToken palette tokens colour = case elemIndex colour palette of
  Just i  -> Just (tokens !! i)
  Nothing -> Nothing
