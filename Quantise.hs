{-# LANGUAGE FlexibleInstances #-}
module Quantise (quantise) where

import Data.List
import Data.Ord
import Codec.Picture.Types
import Image (ImageData(..), ColourStream(..))
import Palette (Palette(..))

-- n-dimensional Euclidean metric, to find the "distance" between colours
-- n.b., Word8 wraps on overflow, so we need explicit fromIntegral
--       Also, Pixel8 is a type synonym, so we need FlexibleInstances
euclidean :: Integral a => [a] -> [a] -> Double
euclidean a b = sqrt . sum $ zipWith (\u v -> (u - v)^2) x y
                where x = map fromIntegral a
                      y = map fromIntegral b

class Metric a where
  d :: a -> a -> Double

instance Metric Pixel8 where
  d a b = euclidean [a] [b]

instance Metric PixelRGB8 where
  d (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) =
    euclidean [r1, g1, b1] [r2, g2, b2]

quantise :: Palette -> ImageData -> ImageData

quantise (Palette _ (Greys paletteStream)) (ImageData w h (Greys imgStream)) =
  ImageData w h $ Greys (nearestNeighbour paletteStream imgStream)

quantise (Palette _ (Colours paletteStream)) (ImageData w h (Colours imgStream)) =
  ImageData w h $ Colours (nearestNeighbour paletteStream imgStream)

-- Nearest-neighbour quantisation
-- TODO Make this look less like line noise :P
-- TODO Probabilistic nearest-neighbour...
nearestNeighbour :: (Metric a) => [a] -> [a] -> [a]
nearestNeighbour palette = 
  map (\c -> snd (c, fst $ minimumBy (comparing snd) $ map (\x -> (x, d c x)) palette))
