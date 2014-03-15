{-# LANGUAGE OverloadedStrings #-}

module Bitmap where

import Data.Word
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as BL

-- Test inputs
fileName = "test.bmp"
image = [[Pixel8 (0,0,255), Pixel8 (255,255,0), Pixel8 (0,0,0)],
         [Pixel8 (255,0,0), Pixel8 (0,255,0), Pixel8 (255,255,255)]]
sampleData = BMP 3 2 24 300 300 image

image2 = [[Pixel8 (255,0,0), Pixel8 (255,255,255)],
          [Pixel8 (0,0,255), Pixel8 (0,255,0)]]
sampleData2 = BMP 2 2 24 300 300 image2

-- BMP Data TypeA
data Pixel = Pixel8 (Word8, Word8, Word8)
          | Pixel16 (Word16, Word16, Word16)
type ImageData = [[Pixel]]
data BMP = BMP { bmpWidth :: Int            -- 32 bit
               , bmpHeight :: Int           -- 32 bit
               , bitsPerPixel :: Int        -- 16 bit
               , horizRes :: Int            -- 32 bit
               , vertRes :: Int             -- 32 bit
               , imageData :: ImageData
               }

-- Constants
fileHeaderBytes = 14
infoHeaderBytes = 40

put2 = putWord16le . fromIntegral
put4 = putWord32le . fromIntegral

writeBitmap file bmpData = do
  BL.writeFile file $ runPut $ buildFile bmpData

buildFile bmpData = do
  buildHeader bmpData
  buildPixData $ imageData bmpData
  flush

buildHeader bmpData = do
  putLazyByteString "BM"
  put4 fileSize                                 -- Size of file
  put2 0                                        -- Reserved
  put2 0                                        -- Reserved
  put4 $ fileHeaderBytes + infoHeaderBytes      -- Offset to PixArray
  put4 infoHeaderBytes                          -- DIB Header length
  put4 $ bmpWidth bmpData                       -- Image Width
  put4 $ bmpHeight bmpData                      -- Image Height
  put2 1                                        -- Color Pane Count
  put2 $ bitsPerPixel bmpData                   -- Bits per Pixel
  put4 0                                        -- Compression Method
  put4 pixDataSize                              -- Size of pixel data
  put4 $ horizRes bmpData
  put4 $ vertRes bmpData
  put4 0                                        -- Colors in Palette
  put4 0                                        -- Important Colors
    where
      fileSize = pixDataSize + fileHeaderBytes + infoHeaderBytes
      pixDataSize =  bitsPerRow * bmpHeight bmpData
      bitsPerRow = (bitsPerPixel bmpData * bmpWidth bmpData + 31) `quot` 32 * 4

buildPixData pixData = mapM_ buildRow pixData
  where
  buildRow rowData = do
    mapM_ buildPixel rowData
    buildPadding (head rowData) (length rowData)
  buildPixel (Pixel8 (r,g,b)) = do
    putWord8 b
    putWord8 g
    putWord8 r
  buildPixel (Pixel16 (r,g,b)) = undefined
  buildPadding (Pixel8 (_,_,_)) count = mapM_ putWord8 (replicate extra 0)
    where extra = (4 - ((count * 3) `mod` 4)) `mod` 4
  buildPadding (Pixel16 (_,_,_)) count = undefined
