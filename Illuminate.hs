module Illuminate (
  getImageData
) where

import Data.Vector.Storable
import Codec.Picture.Types

-- Convert generic image to greyscale
-- n.b., We don't consider 16-bit images, but we
-- use PixelF (Floats) so we can potentially
illuminate :: DynamicImage -> Image PixelF
illuminate image = promoteImage $ toGrey image
  where toGrey :: DynamicImage -> Image Pixel8
        toGrey (ImageY8     image) = image
        toGrey (ImageYA8    image) = pixelMap dropTransparency image
        toGrey (ImageRGB8   image) = pixelMap computeLuma image
        toGrey (ImageRGBA8  image) = pixelMap computeLuma image
        toGrey (ImageYCbCr8 image) = pixelMap computeLuma image
        toGrey (ImageCMYK8  image) = pixelMap computeLuma $ toRGB image
          where toRGB = convertImage :: Image PixelCMYK8 -> Image PixelRGB8
        toGrey _                   = error "Unhandled colourspace"

-- Width::Int Height::Int Data::[Float]
data ImageData = ImageData Int Int [Float]
  deriving (Show)

-- Illuminate image and return data
getImageData :: DynamicImage -> ImageData
getImageData image = ImageData (imageWidth lumImage) (imageHeight lumImage) (toList $ imageData lumImage)
  where lumImage = illuminate image
