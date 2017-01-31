{- | This module contains some utility functions to convert from our Turtle language to
     something that HGL understands (e.g. TUnits to Pixel
-}
module HGLUtils (
  -- Window dimensions
  xWin,
  yWin,

  -- Conversion functions
  toWinCoords,
  tUnitsToPixels,
  pixelsToTUnits,
  toHGLColor
  ) where

import Pen
import Turtle

import qualified Graphics.HGL as HGL

type Pixel = Int

-- | Dimensions of the graphics window.
xWin :: Pixel
xWin = 600

yWin :: Pixel
yWin = 600

type Coord a = (a, a)

-- | Convert from Turtle coordinates to Window coordinates. Turtle coordinates' origin
--   is at (xWinH, yWin), with up/down being +/- y, and right/left being +/- x. Note that
--   in image coordinates, +/- y is down/up, while +/- x is right/left.
toWinCoords :: Coord TUnits -> Coord Pixel
toWinCoords (tx, ty) 
  = let (tpx, tpy) = (tUnitsToPixels tx, tUnitsToPixels ty)
    in  (tpx + xWinH, yWin - tpy)                   

-- | Converts from TUnits to Pixels
tUnitsToPixels :: TUnits -> Pixel
tUnitsToPixels = round . (* pixelsPerTUnit) 

-- | Converts from Pixels to TUnits
pixelsToTUnits :: Pixel -> TUnits
pixelsToTUnits = (/ pixelsPerTUnit) . fromIntegral 

-- | Will be one-to-one, there are as many PColors as HGL.Colors.
toHGLColor :: PColor -> HGL.Color
toHGLColor Black   = HGL.Black
toHGLColor Blue    = HGL.Blue
toHGLColor Green   = HGL.Green
toHGLColor Cyan    = HGL.Cyan
toHGLColor Red     = HGL.Red
toHGLColor Magenta = HGL.Magenta
toHGLColor Yellow  = HGL.Yellow
toHGLColor White   = HGL.White

-- Helper functons/constants

xWinH :: Pixel
xWinH = xWin `div` 2

-- | Number of Pixels per TUnit.
pixelsPerTUnit :: (Num a) => a
pixelsPerTUnit = 1
