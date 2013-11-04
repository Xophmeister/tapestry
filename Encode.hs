{-# LANGUAGE FlexibleInstances #-}
module Encode (encode) where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import Codec.Picture.Types
import Image (ColourStream(..), ImageData(..))
import Palette (Palette(..), tokenise)

normaliseChar :: Maybe Char -> Char
normaliseChar (Just x) = x
normaliseChar Nothing  = '?'

class Colour a where
  encodeImage :: Palette -> [a] -> [Char]
  encodePalette :: [Char] -> [a] -> [(Char, String)]

instance Colour Pixel8 where
  encodeImage palette = map (normaliseChar . tokenise palette)
  encodePalette tokens colours = zip tokens $ map (\y -> cssRGB y y y) colours

instance Colour PixelRGB8 where
  encodeImage palette = map (normaliseChar . tokenise palette)
  encodePalette tokens colours = zip tokens $ map (\(PixelRGB8 r g b) -> cssRGB r g b) colours

outputPipeline :: Int -> Int -> [Char] -> [(Char, String)] -> String
outputPipeline w h imgTokens paletteTokens =
  let (tokens, css) = unzip paletteTokens
      htmlImage     = (table "tapestry" . unlines . map (tr . concat) . chunksOf w . map (td . htmlEntity)) imgTokens
      htmlPalette   = table "palette" $ (tr . concat . map (td . htmlEntity)) tokens  ++
                                        (tr . concat . map (\x -> htmlTag "td" [("style", "background: " ++ x)] "&nbsp;")) css

  in boilerplate $ htmlImage ++ htmlPalette

-- This is as ugly as balls :P
-- Methinks my approach needs *serious* work!
encode :: Palette -> ImageData -> String
encode palette@(Palette tokens (Greys   pStream)) (ImageData w h (Greys   imgStream)) = outputPipeline w h (encodeImage palette imgStream) (encodePalette tokens pStream)
encode palette@(Palette tokens (Colours pStream)) (ImageData w h (Colours imgStream)) = outputPipeline w h (encodeImage palette imgStream) (encodePalette tokens pStream)

-- HTML encoding stuff
-- This should probably be in its own module...
htmlEntity :: Char -> String
htmlEntity x = "&#" ++ (show $ ord x) ++ ";"

serialise :: [(String, String)] -> String
serialise [] = ""
serialise ((key, value) : xs) = " " ++ key ++ "=\"" ++ value ++ "\"" ++ serialise xs

htmlTag :: String -> [(String, String)] -> String -> String
htmlTag selector attributes innerHTML = concat [
  "<", selector, serialise attributes, ">",
  innerHTML,
  "</", selector, ">"]

simpleTag :: String -> String -> String
simpleTag selector = htmlTag selector []

cssRGB :: (Integral a, Show a) => a -> a -> a -> String
cssRGB r g b = concat ["rgb(", show r, ", ", show g, ", ", show b, ")"]

td :: String -> String
td = simpleTag "td"

tr :: String -> String
tr = simpleTag "tr"

table :: String -> String -> String
table cssClass = htmlTag "table" [("class", cssClass)]

boilerplate :: String -> String
boilerplate innerHTML =
  "<!DOCTYPE html>\n" ++
  simpleTag "html" (
    simpleTag "head" (
      simpleTag "title" "Tapestry" ++
      htmlTag "style" [("type", "text/css")] (
        "body { background: White }"
      )
    ) ++
    simpleTag "body" 
      innerHTML
  )
