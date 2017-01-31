{- | This module contains the animation function for the graphical
     interface.
-}
module Graphical (
  delayGraphical,
  animateGraphical
  ) where
  
import Pen
import Turtle
import TSummary

import HGLUtils

import Graphics.HGL

import Textual -- Some of the code will need textual output to state what's going on.

-- | Delay function for the graphical interface
delayGraphical :: Window -> IO ()
delayGraphical = getWindowTick

-- | Animation function for the graphical interface. Note that the only thing this really
--   requires is drawing the Turtle each time it moves forward. Outside of that, everything
--   else is explained in the Textual interface.
animateGraphical :: Window -> Turtle -> TSummary -> IO ()
animateGraphical w tur ts = do animateGraphical' tur ts
                               return ()
 where
  -- | Helper function to pass the Turtle around, important
  --   for sequenced operations.
  animateGraphical' :: Turtle -> TSummary -> IO (Turtle)

  animateGraphical' tur (Forward x)
   = let tur'          = move x tur
         (tpens, tpenc) = getp $ pen tur'
         opos           = pos tur
         npos           = pos tur'

     in if tpens
          then
            let owinc   = toWinCoords opos
                nwinc   = toWinCoords npos
                hgl_penc = toHGLColor tpenc

            in do drawInWindow w $ withColor hgl_penc $ line owinc nwinc
                  return tur'
          else
            return tur'

  animateGraphical' tur (ts1 :>#> ts2) = do tur' <- animateGraphical' tur ts1
                                            animateGraphical' tur' ts2

  animateGraphical' tur Kill = do putStrLn "Turtle died!"
                                  return (kill tur)

  animateGraphical' tur ts = return tur -- animateTextual' tur ts
