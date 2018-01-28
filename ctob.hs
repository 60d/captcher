import Codec.Picture
import System.Environment
import Control.Monad

main :: IO ()
main = do
  [input] <- getArgs
  bmp <- readBitmap input
  case bmp of
    Left error  -> putStrLn "error"
    Right image -> forM_ [0..9] $ \i ->
                     forM_ [0..24] $ \j ->
                       case pix image i j of
                         True  -> putStr "0,"
                         False -> putStr "1,"
  where 
    pix :: DynamicImage -> Int -> Int -> Bool
    pix (ImageRGB8 image@(Image w h _)) a b = iseq (pixelAt image a b ) (pixelAt image 0 0 )

    iseq :: PixelRGB8 -> PixelRGB8 -> Bool 
    iseq (PixelRGB8 r g b) (PixelRGB8 fr fg fb) = r == fr  && g == fg && b == fb 
