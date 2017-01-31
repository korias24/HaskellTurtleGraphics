{- | This module contains the animation function for the textual
     interface.
-}
module Textual (
  delayTextual,
  animateTextual,
  animateTextual'
  ) where
  
import Pen
import Turtle
import TSummary

import Data.Char (toLower)

import qualified Numeric as N


-- | Delay function for the textual interface.
delayTextual :: IO ()
delayTextual = return ()

-- | Animation function for the textual interface.
animateTextual :: Turtle -> TSummary -> IO ()

animateTextual tur s = do putStrLn $ showState tur
                          tur' <- animateTextual' tur s
                          putStrLn $ showState tur' ++ "\n"
                          


-- | Helper function. Its purpose is to animate the turtle,
--   outputting each operation that happens during the animation.
animateTextual' :: Turtle -> TSummary -> IO (Turtle)

animateTextual' tur Idle  = animateTurtle id "" tur -- Nothing happens in the Idle program   
animateTextual' tur Pause = animateTurtle id "Turtle pauses!\n" tur
animateTextual' tur Kill  = animateTurtle kill "Turtle died!\n" tur
animateTextual' tur Dying 
  = animateTurtle countdown "Decremented the turtle's life!\n" tur

animateTextual' tur (PState s)
  = let act = if s == True 
                then 
                  "placed down, ready to draw!" 
                else 
                  "picked up, cannot draw!"
    in  animateTurtle (penstate s) ("Turtle's pen was " ++ act ++ "\n") tur
    
animateTextual' tur (PColor c)
  = let cmsg = showPColor c ++ "!\n"
    in animateTurtle (pencolor c) ("Turtle's pen's color changed to " ++ cmsg) tur
    
animateTextual' tur (MarkTerminal t) =
  case (terminal tur) of  -- Is the Turtle already terminal?
      (Just t') -> 
        if t < t' -- Turtle is terminal at a later time than t, so we re-mark it
          then
            animateTurtle (markTerminal t) tmsg tur
          else
            return tur
      
      Nothing -> animateTurtle (markTerminal t) tmsg tur
  where
    tmsg = "Marked turtle for death in " ++ show t ++ " steps!\n"   

animateTextual' tur (RunLimited t)
  = animateTurtle 
      id 
      ("Just ran a limited program! " ++ show t ++ " steps left until it terminates!\n")
      tur
      
-- | If the program ended, then ps will be True. Otherwise if time was up, then
--   ps will be False. 
animateTextual' tur (EndLimited ps)
  | ps = animateTurtle id "Just finished a limited program all the way through!\n" tur
  | otherwise 
      = animateTurtle id "Times up! Current program will no longer be executed!\n" tur


animateTextual' tur (ts1 :>#> ts2) = do tur' <- animateTextual' tur ts1
                                        animateTextual' tur' ts2

-- | Can rotate the Turtle either left (CCW) or right (CW). Left is x < 0, right
--   is x >= 0                                    
animateTextual' tur (Rotate x)
  = let (dir, amt) = if x < 0 
                       then 
                         ("counter-clockwise", negate x) 
                       else 
                         ("clockwise", x)
    in animateTurtle 
         (rotate x) 
         ("Rotated the turtle " ++ dir ++ " by " ++ showFloat amt ++ " degrees!\n")
         tur

-- | When moving the Turtle forward, a line is also drawn if the Turtle's pen is down.
--   This function models that.         
animateTextual' tur (Forward x)
  = do tur' <- animateTurtle 
                 (move x) 
                 ("Moved turtle forward " ++ showFloat x ++ " units!\n")
                 tur
  
       let (tpens, tpenc)  = getp $ pen tur'
           opos            = pos tur
           npos            = pos tur'
       if tpens
         then
           do putStr (
                       "Drew a " ++
                       showPColor tpenc ++ 
                       " line from " ++ 
                       showPos opos ++ 
                       " to " ++ 
                       showPos npos ++ 
                       "!\n"
                     )
              return tur'
         else
           return tur'

 
-- | Helper functions
 
-- | Prints out a message indicating the animation that was performed, and then
--   animates the turtle.                        
animateTurtle :: (Turtle -> Turtle) -> String -> Turtle -> IO (Turtle)
animateTurtle f msg tur = do putStr msg
                             return (f tur)
                
-- Shows the pen's color                
showPColor :: PColor -> String
showPColor = map toLower . show

-- | Helper function to print the Turtle's state.
showState :: Turtle -> String
showState tur 
  = let tpos            = pos tur
        torient         = toDegrs $ orient tur
        tpen            = getp $ pen tur
        tstatus          = status tur
    in "TURTLE = " ++ 
       "(" ++ showPos tpos ++ 
       ", " ++ showFloat torient ++ 
       ", " ++ showPen tpen ++ 
       ", " ++ show tstatus ++
       ")"

-- | Shows the pen in a more readable format 
showPen :: (PState, PColor) -> String      
showPen (ps, pc) = let ps' = if ps then "Down" else "Up"
                   in show (ps', pc)
                 
-- | Shows the Turtle's position up to two decimal places.                 
showPos :: Position -> String    
showPos (x, y) = "(" ++ showFloat x ++ ", " ++ showFloat y ++ ")"

-- | Shows a floating point number with a precision of two decimal places.
showFloat :: Float -> String
showFloat x = N.showFFloat (Just precision) x ""

-- Converts from Radians to Degrees
toDegrs :: Float -> Float
toDegrs = (180 *) . (/ pi)

-- | The # of decimals we are going to print out (for the position and orientation)
precision :: Int
precision = 2

