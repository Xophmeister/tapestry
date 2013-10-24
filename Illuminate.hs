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
        toGrey (ImageY8     img) = img
        toGrey (ImageYA8    img) = pixelMap dropTransparency img
        toGrey (ImageRGB8   img) = pixelMap computeLuma img
        toGrey (ImageRGBA8  img) = pixelMap computeLuma img
        toGrey (ImageYCbCr8 img) = pixelMap computeLuma img
        toGrey (ImageCMYK8  img) = pixelMap computeLuma $ toRGB img
          where toRGB = convertImage :: Image PixelCMYK8 -> Image PixelRGB8
        toGrey _                   = error "Unhandled colourspace"

-- Width::Int Height::Int Data::[Float]
data ImageData = ImageData Int Int [Float]
  deriving (Show)

-- Illuminate image and return data
getImageData :: DynamicImage -> ImageData
getImageData image = ImageData (imageWidth lumImage) (imageHeight lumImage) (toList $ imageData lumImage)
  where lumImage = illuminate image
