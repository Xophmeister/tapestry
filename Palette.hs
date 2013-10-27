module Palette where

data Grey = Grey Int
data RGB  = RGB  Int Int Int

data PaletteItem a = PaletteItem Char a

type Palette a = [PaletteItem a]

defaultPalette :: Palette Grey
defaultPalette = [ PaletteItem '\9633'  (Grey 255),  -- Pure white
                   PaletteItem '\9675'  (Grey 218),
                   PaletteItem '\9678'  (Grey 182),
                   PaletteItem '\9673'  (Grey 145),
                   PaletteItem '\9632'  (Grey 109),
                   PaletteItem '\9679'  (Grey  72),
                   PaletteItem '\10070' (Grey  36),
                   PaletteItem '\9733'  (Grey   0) ] -- Pure black

greyToRGB :: PaletteItem Grey -> PaletteItem RGB
greyToRGB (PaletteItem c (Grey x)) = PaletteItem c (RGB x x x)

class Colour a where
  getPalette :: Maybe String -> Palette a 
  d :: a -> a -> Double

instance Colour Grey where
  getPalette Nothing = defaultPalette
  getPalette (Just paletteFile) = undefined

  d (Grey x) (Grey y) = fromIntegral $ abs $ x - y

instance Colour RGB where
  getPalette Nothing = map greyToRGB defaultPalette
  getPalette (Just paletteFile) = undefined

  d (RGB r1 g1 b1) (RGB r2 g2 b2) = sqrt $ fromIntegral $ (r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2
