{-# LANGUAGE FlexibleInstances #-}
module Encode (encode) where

import Data.Char (ord)
import Data.List (group, sort)
import Data.List.Split (chunksOf)
import Codec.Picture.Types
import Image (ColourStream(..), ImageData(..))
import Palette (Palette(..), tokenise)

normaliseChar :: Maybe Char -> Char
normaliseChar (Just x) = x
normaliseChar Nothing  = '?'

normaliseInt :: Maybe Int -> Int
normaliseInt (Just x) = x
normaliseInt Nothing  = 0

class Colour a where
  encodeImage :: Palette -> [a] -> [Char]
  encodePalette :: [Char] -> [a] -> [(Char, String)]

instance Colour Pixel8 where
  encodeImage palette = map (normaliseChar . tokenise palette)
  encodePalette tokens colours = zip tokens $ map (\y -> cssRGB y y y) colours

instance Colour PixelRGB8 where
  encodeImage palette = map (normaliseChar . tokenise palette)
  encodePalette tokens colours = zip tokens $ map (\(PixelRGB8 r g b) -> cssRGB r g b) colours

getCounts :: [(Char, String)] -> [Char] -> [(Char, Int, String)]
getCounts paletteTokens imgTokens =
  let imgHistogram = (map (\x -> (head x, length x)) . group . sort) imgTokens
  in map (\(t, css) -> (t, normaliseInt $ lookup t imgHistogram, css)) paletteTokens

outputPipeline :: Int -> Int -> [Char] -> [(Char, String)] -> String
outputPipeline w h imgTokens paletteTokens =
  let (tokens, histogram, css) = unzip3 (getCounts paletteTokens imgTokens)
      htmlImage   = (table "tapestry" . unlines . map (tr . concat) . chunksOf w . map (td . htmlEntity)) imgTokens
      htmlPalette = table "palette" $ tr (td "Tokens"   ++ concatMap (td . htmlEntity) tokens) ++
                                      tr (td "Stiches"  ++ concatMap (td . show) histogram) ++
                                      tr (td "Swatches" ++ concatMap (\x -> htmlTag "td" [("style", "background: " ++ x)] "&nbsp;") css)

  in boilerplate $ h1 "Plan" ++ htmlImage ++ h1 "Palette" ++ htmlPalette

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

h1 :: String -> String
h1 = simpleTag "h1"

boilerplate :: String -> String
boilerplate innerHTML =
  "<!DOCTYPE html>\n" ++
  simpleTag "html" (
    simpleTag "head" (
      simpleTag "title" "Tapestry" ++
      htmlTag "style" [("type", "text/css")] outputCSS
    ) ++
    simpleTag "body" 
      innerHTML
  )

  where outputCSS = unlines [ "body { margin: 1cm; }", 
                              "body * { font-family: \"Lucida Sans Unicode\"; }", 
                              "h1 { font-size: 20pt; margin: 0.5cm 0cm; }", 
                              "table.tapestry td { min-width: 0.75cm; min-height: 0.75cm; max-width: 0.75cm; max-height: 0.75cm; text-align: center; border-right: 1px dotted DarkGrey; font-size: 14pt; }", 
                              "table.tapestry td:nth-of-type(10n) { border-right: 1px solid DarkGrey; }", 
                              "table.tapestry tr { border-bottom: 1px dotted DarkGrey; }", 
                              "table.tapestry tr:nth-of-type(10n) { border-bottom: 1px solid DarkGrey; }", 
                              "table.tapestry td:last-child { border-right: 1px solid black; }", 
                              "table.tapestry { border: 1px solid Black; border-collapse: collapse; }", 
                              "table.tapestry tr:last-child { border-bottom: black; }", 
                              "table.palette { border-spacing: 0.1cm; }", 
                              "table.palette td { text-align: center; min-width: 2cm; max-width: 2cm; }", 
                              "table.palette td:first-child { text-align: left; padding-right: 0.5cm; padding-left: 0cm; vertical-align: top; }", 
                              "table.palette tr:last-child { height: 2cm; }" ]
