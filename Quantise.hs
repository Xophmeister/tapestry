{-# LANGUAGE FlexibleInstances #-}
module Quantise ( quantise ) where

import Codec.Picture.Types
import Image (ImageData(..))
import Palette (Palette(..))

-- We need to find the distance between colours
-- n.b., Pixel8 is a synonym for Word8, so we need FlexibleInstances
-- Also Word8 wraps on overflow, so we need explicit fromIntegral
class Metric a where
  d :: a -> a -> Double

-- n-dimensional Euclidean metric
euclidean :: Integral a => [a] -> [a] -> Double
euclidean a b = sqrt . sum $ zipWith (\u v -> (u - v)^2) x y
                where x = map fromIntegral a
                      y = map fromIntegral b

instance Metric Pixel8 where
  d a b = euclidean [a] [b]

instance Metric PixelRGB8 where
  d (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) =
    euclidean [r1, g1, b1] [r2, g2, b2]

quantise :: Palette -> ImageData -> ImageData
quantise = undefined
