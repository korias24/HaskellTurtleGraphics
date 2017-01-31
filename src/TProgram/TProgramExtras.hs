{- | This module contains some extra stuff that one can do with Turtle programs, including example
     programs to run.
-}
module TProgramExtras where

import TProgram

-- | The spiral example from the Logos primer
spiral :: TUnits -> Degrees -> TProgram
spiral size angle
  | size > 100 = idle
  | otherwise = forward size >*> right angle >*> spiral (size + 2) (angle)

-- Moves the Turtle to the center of the screen, without drawing anything.
center :: TProgram
center = penup >*> forward 125 >*> pendown

-- | Makes a square that has dimensions based on the passed in TUnits
square :: TUnits -> TProgram
square x = let sedge = forward x >*> right 90
           in  pendown >*> times 3 sedge >*> forward x

-- | The flower example from the Logos primer, with an arbitrary size
flower :: TUnits -> TProgram
flower = (center >*>) . (pendown >*>) . times 36 . (right 10 >*>) . square

-- | Creates the TriForce from Legend of Zelda using four turtles running in parallel.
--   Note that this could be done with one Turtle, I just wanted to demonstrate the
--   parallel part of the program.
triforce :: TProgram
triforce = leftTri <|> topTri <|> centTri <|> rightTri
 where
  setup = center >*> penup

  leftTri  = setup >*> left 90 >*> forward 50 >*> right 90 >*> filledEqTri Yellow 100
  rightTri = setup >*> right 90 >*> forward 50 >*> left 90 >*> filledEqTri Yellow 100
  topTri  = setup >*> forward (100*cos(pi/6)) >*> filledEqTri Yellow 100
  centTri = setup >*> forward (100*cos(pi/6) - 1.0) >*> right 180 >*> filledEqTri White 100

-- | Draws an equilateral triangle.
eqTri :: TUnits -> TProgram
eqTri s = let se = s / 2
              pat s = left 120 >*> forward s -- Common pattern in the program
          in right 90 >*> forward se >*> times 2 (pat s) >*> pat se >*> left 90


-- | Draws an equilateral triangle that is filled in with the passed in color.
filledEqTri :: PColor -> TUnits -> TProgram
filledEqTri clr s = pendown >*> color clr >*> filledEqTri' s
 where
  se = s / 2    -- Point where we end drawing the triangle
  ds = s / 20.0 -- How much we incrementally move next.

  filledEqTri' :: TUnits -> TProgram
  filledEqTri' s
    | s <= 0 = idle
    | otherwise = eqTri s >*> filledEqTri' (s - ds)  
   
