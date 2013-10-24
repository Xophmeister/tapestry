module Encode (encodeLuminance) where

import Data.List.Split (chunksOf)
import Types

palettise :: Palette -> Float -> Char
palettise palette luminance
  | luminance < 0.125 = palette !! 7 -- Pure black
  | luminance < 0.25  = palette !! 6
  | luminance < 0.375 = palette !! 5
  | luminance < 0.5   = palette !! 4
  | luminance < 0.625 = palette !! 3
  | luminance < 0.75  = palette !! 2
  | luminance < 0.875 = palette !! 1
  | otherwise         = head palette -- Pure white

encodeLuminance :: ImageData -> [String]
encodeLuminance (ImageData w _ stream) = chunksOf w $ map (palettise " .:oO%8#") stream
