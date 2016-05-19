module Futurice.App.Avatar.Logic (avatar) where

import Prelude        ()
import Prelude.Compat

import qualified Data.ByteString.Lazy as LBS

import Codec.Picture          (DynamicImage (..), Image (..), PixelRGBA8,
                               convertRGBA8, decodeImage)
import Codec.Picture.ScaleDCT (scale)
import Codec.Picture.Types    (extractLumaPlane)

import           Control.Monad                (forM_)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign.Marshal.Utils        (copyBytes)
import           Foreign.Ptr                  (plusPtr)
import           System.IO.Unsafe             (unsafePerformIO)

transformImage :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
transformImage avatarSize = scale (avatarSize, avatarSize) . cropImage

cropImage :: Image PixelRGBA8 -> Image PixelRGBA8
cropImage img@(Image w h _) = trimImage img wh tl
  where
    (wh, tl) = toSquare (w, h)

toSquare :: (Int, Int) -> ((Int, Int), (Int, Int))
toSquare (w, h)
    | h > w     = ((w, w), (0              , ((h-w) `div` 5)))
    | otherwise = ((h, h), (((w-h) `div` 2), 0))

avatar :: Int -> Bool -> LBS.ByteString -> Either String DynamicImage
avatar avatarSize grey contents
    = constr
    . transformImage avatarSize
    . convertRGBA8
    <$> decodeImage (LBS.toStrict contents)
  where
    constr | grey      = ImageY8 . extractLumaPlane
           | otherwise = ImageRGBA8

-- | From JuicyPixels-util.
--  Copyright (C) 2012-2013 Fumiaki Kinoshita
trimImage :: Image PixelRGBA8
    -> (Int, Int) -- ^ width, height
    -> (Int, Int) -- ^ the left corner point
    -> Image PixelRGBA8
trimImage (Image w _ vec) (w', h') (x0, y0) = unsafePerformIO $ V.unsafeWith vec $ \ptr -> do
    mv <- MV.unsafeNew $ w' * h' * 4
    MV.unsafeWith mv $ \dst -> forM_ [0..h'-1] $ \y ->
        copyBytes (plusPtr dst $ y * w' * 4) (plusPtr ptr $ (*4) $ (y + y0) * w + x0) (4 * w')
    Image w' h' `fmap` V.unsafeFreeze mv
