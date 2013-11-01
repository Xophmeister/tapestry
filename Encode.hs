{-# LANGUAGE FlexibleInstances #-}
module Encode (encode) where

import Data.List.Split (chunksOf)
import Codec.Picture.Types
import Image (ColourStream(..), ImageData(..))
import Palette (Palette(..), tokenise)

normalise :: Maybe Char -> Char
normalise (Just x) = x
normalise Nothing  = '?'

class Colour a where
  encodeImage :: Palette -> Int -> [a] -> [[Char]]

instance Colour Pixel8 where
  encodeImage palette w stream = chunksOf w $ map (normalise . tokenise palette) stream

instance Colour PixelRGB8 where
  encodeImage palette w stream = chunksOf w $ map (normalise . tokenise palette) stream

encode :: Palette -> ImageData -> [[Char]]
encode palette (ImageData w _ (Greys   stream)) = encodeImage palette w stream 
encode palette (ImageData w _ (Colours stream)) = encodeImage palette w stream 
