{-# LANGUAGE FlexibleInstances #-}
module Quantise (quantise) where

import Codec.Picture.Types
import Image (ImageData(..), ColourStream(..))
import Palette (Palette(..))

-- n-dimensional Euclidean metric
-- We need to find the distance between colours
-- n.b., Pixel8 is a synonym for Word8, so we need FlexibleInstances
-- Also Word8 wraps on overflow, so we need explicit fromIntegral
euclidean :: Integral a => [a] -> [a] -> Double
euclidean a b = sqrt . sum $ zipWith (\u v -> (u - v)^2) x y
                where x = map fromIntegral a
                      y = map fromIntegral b

class Colour a where
  d :: a -> a -> Double
  extractStreamData :: ColourStream -> [a]
  buildColourStream :: [a] -> ColourStream

instance Colour Pixel8 where
  d a b = euclidean [a] [b]
  extractStreamData (Greys x) = x
  buildColourStream = Greys

instance Colour PixelRGB8 where
  d (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = euclidean [r1, g1, b1] [r2, g2, b2]
  extractStreamData (Colours x) = x
  buildColourStream = Colours

-- Nearest-neighbour quantisation
-- TODO Probabilistic nearest-neighbour...
quantise :: Palette -> ImageData -> ImageData
quantise (Palette _ pStream) (ImageData w h imgStream) = undefined
