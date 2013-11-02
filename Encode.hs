{-# LANGUAGE FlexibleInstances #-}
module Encode (encode) where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import Codec.Picture.Types
import Image (ColourStream(..), ImageData(..))
import Palette (Palette(..), tokenise)

normalise :: Maybe Char -> Char
normalise (Just x) = x
normalise Nothing  = '?'

class Colour a where
  encodeImage :: Palette -> [a] -> [Char]

instance Colour Pixel8 where
  encodeImage palette = map (normalise . tokenise palette)

instance Colour PixelRGB8 where
  encodeImage palette = map (normalise . tokenise palette)

htmlEntity :: Char -> String
htmlEntity x = "&#" ++ (show $ ord x) ++ ";"

htmlTag :: String -> Maybe String -> String -> String
htmlTag selector cssClass innerHTML = concat [
   "<",
   selector,
   case cssClass of
     Nothing  -> ""
     Just css -> " class=\"" ++ css ++ "\"",
   ">",
   innerHTML,
   "</",
   selector,
   ">"]

td :: String -> String
td = htmlTag "td" Nothing

tr :: String -> String
tr = htmlTag "tr" Nothing

table :: String -> String
table = htmlTag "table" (Just "tapestry")

tableImage :: Int -> [Char] -> [String]
tableImage w tokens = map (tr . concat) $ chunksOf w $ map (td . htmlEntity) tokens

encode :: Palette -> ImageData -> String
encode palette (ImageData w _ (Greys   stream)) = (table . unlines . tableImage w . encodeImage palette) stream 
encode palette (ImageData w _ (Colours stream)) = (table . unlines . tableImage w . encodeImage palette) stream 
