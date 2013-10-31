module Quantise (quantise) where

import Data.List
import Data.Ord
import Codec.Picture.Types
import Image (ImageData(..), ColourStream(..))
import Palette (Palette(..))

-- n-dimensional Euclidean metric, to find the "distance" between colours
-- n.b., Word8 wraps on overflow, so we need explicit fromIntegral
euclidean :: Integral a => [a] -> [a] -> Double
euclidean a b = sqrt . sum $ zipWith (\u v -> (u - v)^2) x y
                where x = map fromIntegral a
                      y = map fromIntegral b

quantise :: Palette -> ImageData -> ImageData

quantise (Palette _ (Greys   paletteStream)) (ImageData w h (Greys   imgStream)) =
  ImageData w h $ Greys (nearestNeighbourGrey paletteStream imgStream)

quantise (Palette _ (Colours paletteStream)) (ImageData w h (Colours imgStream)) =
  ImageData w h $ Colours (nearestNeighbourRGB paletteStream imgStream)

-- Nearest-neighbour quantisation
-- TODO Probabilistic nearest-neighbour...
nearestNeighbourGrey :: [Pixel8] -> [Pixel8] -> [Pixel8]
nearestNeighbourGrey palette = 
  map (\c -> snd (c, fst $ minimumBy (comparing snd) $ map (\x -> (x, d c x)) palette))
  where d a b = euclidean [a] [b]

nearestNeighbourRGB :: [PixelRGB8] -> [PixelRGB8] -> [PixelRGB8]
nearestNeighbourRGB palette = 
  map (\c -> snd (c, fst $ minimumBy (comparing snd) $ map (\x -> (x, d c x)) palette))
  where d (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = euclidean [r1, g1, b1] [r2, g2, b2]
