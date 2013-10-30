module Image ( ColourStream(..),
               ImageData(..),
               imageColourspace,
               serialiseImage ) where

import System.Exit
import Data.Vector.Storable (toList)
import Codec.Picture
import Codec.Picture.Types
import Options (ColourSpace(..))

data ColourStream = Greys   [Pixel8]
                  | Colours [PixelRGB8]

type Width     = Int
type Height    = Int
data ImageData = ImageData Width Height ColourStream

imageColourspace :: ImageData -> ColourSpace
imageColourspace (ImageData _ _ (Greys _))   = Greyscale
imageColourspace (ImageData _ _ (Colours _)) = Colour

dynamicColourspace :: DynamicImage -> ColourSpace
dynamicColourspace (ImageY8 _)  = Greyscale
dynamicColourspace (ImageYA8 _) = Greyscale
dynamicColourspace _            = Colour

-- Open image and serialise meta data and pixels
serialiseImage :: FilePath -> ColourSpace -> IO ImageData
serialiseImage file colourspace = do
  dynImage <- readImage file

  case dynImage of
    Left errStatus -> do putStrLn $ "Failed to load '" ++ file ++ "': " ++ errStatus
                         exitFailure

    Right image    -> normaliseImage image colourspace

-- Normalise image to either 8-bit greyscale or 24-bit RGB
normaliseImage :: DynamicImage -> ColourSpace -> IO ImageData
normaliseImage image colourspace = case colourspace of

  Greyscale -> do
    normalisedImage <- toGrey image
    return $ ImageData (imageWidth normalisedImage)
                       (imageHeight normalisedImage)
                       (Greys $ (toList . imageData) normalisedImage)
    
  Colour -> do
    normalisedImage <- toRGB image
    return $ ImageData (imageWidth normalisedImage)
                       (imageHeight normalisedImage)
                       (Colours $ (toColours . toList . imageData) normalisedImage)

      where toColours :: [Pixel8] -> [PixelRGB8]
            toColours (r:g:b:xs) = PixelRGB8 r g b : toColours xs
            toColours _          = []

  Adaptive ->
    normaliseImage image (dynamicColourspace image)

cannotNormalise :: IO a
cannotNormalise = do putStrLn "Cannot normalise image: Unhandled colourspace"
                     exitFailure

-- Convert image to 8-bit greyscale
toGrey :: DynamicImage -> IO (Image Pixel8)
toGrey (ImageY8     img) = return img
toGrey (ImageYA8    img) = return $ pixelMap dropTransparency img
toGrey (ImageRGB8   img) = return $ pixelMap computeLuma img
toGrey (ImageRGBA8  img) = return $ pixelMap computeLuma img
toGrey (ImageYCbCr8 img) = return $ pixelMap computeLuma img
toGrey (ImageCMYK8  img) = return $ pixelMap computeLuma $ convertCMYK img
  where convertCMYK = convertImage :: Image PixelCMYK8 -> Image PixelRGB8
toGrey _                 = cannotNormalise

-- Convert image to 24-bit (RGB) colour
toRGB :: DynamicImage -> IO (Image PixelRGB8)
toRGB (ImageY8     img) = return $ promoteImage img
toRGB (ImageYA8    img) = return $ promoteImage img
toRGB (ImageRGB8   img) = return img
toRGB (ImageRGBA8  img) = return $ pixelMap dropTransparency img
toRGB (ImageYCbCr8 img) = return $ convertImage img
toRGB (ImageCMYK8  img) = return $ convertImage img
toRGB _                 = cannotNormalise
