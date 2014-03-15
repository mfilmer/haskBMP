import Data.Complex
import Data.Word

import Bitmap

type Coord = (Double,Double,Complex Double)
coords = [[(x,y,x:+y) | x <- [-2, -1.999 .. 1]] | y <- [1, 0.999 .. -1]] :: [[Coord]]

--stepMandel :: Complex Double -> Complex Double
stepMandel z0 z = z^2 + z0

--iterMandel :: Complex Double -> [Complex Double]
iterMandel z0 z = take 25 (takeWhile isBounded (iterate (stepMandel z0) z))

--isBounded z = xBound && yBound
--  where
--  xBound = realPart z >= -2 && realPart z < 1
--  yBound = abs (imagPart z) <= 1O
--isBounded :: Complex Double -> Bool
isBounded z = realPart (abs z) <= 2

escapeCount (_,_,z) = fromIntegral $ length $ iterMandel z z

mandelCounts = map (map escapeCount) coords

generatePixel x = Pixel8 (a,a,a)
  where a = if x == 25 then 0 else x*10

pixels = map (map generatePixel) mandelCounts

columns = length (head pixels)
rows = length pixels
imData = BMP columns rows 24 300 300 pixels

main = writeBitmap "mandelbrot.bmp" imData