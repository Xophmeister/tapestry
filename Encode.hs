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

instance Colour Pixel8 where
  encodeImage palette = map (normaliseChar . tokenise palette)

instance Colour PixelRGB8 where
  encodeImage palette = map (normaliseChar . tokenise palette)

tableImage :: Int -> [Char] -> [String]
tableImage w tokens = map (tr . concat) $ chunksOf w $ map (td . htmlEntity) tokens

-- This is as ugly as balls :P Methinks my approach needs work!
encode :: Palette -> ImageData -> String
encode palette (ImageData w _ (Greys   imgStream)) = (boilerplate . table "tapestry" . unlines . tableImage w . encodeImage palette) imgStream
encode palette (ImageData w _ (Colours imgStream)) = (boilerplate . table "tapestry" . unlines . tableImage w . encodeImage palette) imgStream

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
