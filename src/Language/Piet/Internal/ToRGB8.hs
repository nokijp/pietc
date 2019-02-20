module Language.Piet.Internal.ToRGB8
  ( ToRGB8(..)
  , toRGB8ImageM
  ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.Fail
import Data.Bits
import Prelude hiding (fail)

toRGB8ImageM :: MonadFail m => DynamicImage -> m (Image PixelRGB8)
toRGB8ImageM (ImageY8     _)     = failConversion
toRGB8ImageM (ImageY16    _)     = failConversion
toRGB8ImageM (ImageYF     _)     = failConversion
toRGB8ImageM (ImageYA8    _)     = failConversion
toRGB8ImageM (ImageYA16   _)     = failConversion
toRGB8ImageM (ImageRGB8   image) = return $ toRGB8Image image
toRGB8ImageM (ImageRGB16  image) = return $ toRGB8Image image
toRGB8ImageM (ImageRGBF   image) = return $ toRGB8Image image
toRGB8ImageM (ImageRGBA8  image) = return $ toRGB8Image image
toRGB8ImageM (ImageRGBA16 image) = return $ toRGB8Image image
toRGB8ImageM (ImageYCbCr8 image) = return $ toRGB8Image image
toRGB8ImageM (ImageCMYK8  image) = return $ toRGB8Image image
toRGB8ImageM (ImageCMYK16 image) = return $ toRGB8Image image

failConversion :: MonadFail m => m a
failConversion = fail "can't convert from grayscale images"

class Pixel a => ToRGB8 a where
  toRGB8Pixel :: a -> PixelRGB8
  toRGB8Image :: Image a -> Image PixelRGB8
  toRGB8Image = pixelMap toRGB8Pixel

instance ToRGB8 PixelRGB8 where
  toRGB8Pixel = id

instance ToRGB8 PixelRGB16 where
  toRGB8Pixel (PixelRGB16 r g b) = PixelRGB8 (drop8 r) (drop8 g) (drop8 b) where
    drop8 = toEnum . fromEnum . (`shiftR` 8)

instance ToRGB8 PixelRGBF where
  toRGB8Pixel (PixelRGBF r g b) = PixelRGB8 (toI r) (toI g) (toI b) where
    toI = floor . (* 255) . max 0 . min 1

instance ToRGB8 PixelRGBA8 where
  toRGB8Pixel = dropTransparency

instance ToRGB8 PixelRGBA16 where
  toRGB8Pixel = toRGB8Pixel . dropTransparency

instance ToRGB8 PixelCMYK8 where
  toRGB8Pixel = convertPixel

instance ToRGB8 PixelCMYK16 where
  toRGB8Pixel = toRGB8Pixel . (convertPixel :: PixelCMYK16 -> PixelRGB16)

instance ToRGB8 PixelYCbCr8 where
  toRGB8Pixel = convertPixel
