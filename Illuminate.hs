module Illuminate where

import Codec.Picture
import Codec.Picture.Types

toRGB8 :: DynamicImage -> Image PixelRGBA8
toRGB8 (ImageRGB8  img) = promoteImage img
toRGB8 (ImageRGBA8 img) = img

toLuminance :: DynamicImage -> String
toLuminance image = show $ imageData $ toRGB8 image
