module Illuminate where

import Codec.Picture

toLuminance :: DynamicImage -> Either String String
toLuminance (ImageRGBA8 img) = Right (show $ imageData img)
toLuminance _ = Left "Unhandled colourspace"
