module Image ( ImageData(..),
               imageColourspace,
               serialiseImage ) where

import System.Exit
import Data.Vector.Storable (toList)
import Codec.Picture
import Codec.Picture.Types
import Options (ColourSpace(..))

type Width = Int
type Height = Int
data ImageData a = ImageData ColourSpace Width Height a 

imageColourspace :: ImageData a -> ColourSpace
imageColourspace (ImageData colourspace _ _ _) = colourspace

dynamicColourspace :: DynamicImage -> ColourSpace
dynamicColourspace (ImageY8 _)  = Greyscale
dynamicColourspace (ImageYA8 _) = Greyscale
dynamicColourspace _            = Colour

serialiseImage :: Num a => FilePath -> ColourSpace -> IO (ImageData a)
serialiseImage file colourspace = do
  dynImage <- readImage file

  case dynImage of
    Left errStatus -> do 
      putStrLn $ "Failed to load '" ++ file ++ "': " ++ errStatus
      exitFailure

    Right image ->
      case colourspace of
        Greyscale ->
          return $ ImageData Greyscale 10 10 1
          
        Colour ->
          return $ ImageData Colour 10 10 1

        Adaptive ->
          return $ ImageData (dynamicColourspace image) 10 10 1


{- OLD STUFF

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

-- Illuminate image and return data
processImage :: DynamicImage -> ImageData
processImage image = ImageData (imageWidth lumImage) (imageHeight lumImage) (toList $ imageData lumImage)
  where lumImage = illuminate image

-}
