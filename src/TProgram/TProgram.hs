-- | This module stores the Program type as required in the assignment, except here
--   it is labeled as TProgram to stand for Turtle Program, since Program is a more
--   general type defined in TimeStepProgram.hs
module TProgram (
  -- * The turtle type(s)
  TProgram,
  TUnits,
  Degrees,
  PColor(..),
  
  -- * Primitive operations
  idle,
  pause,
  forward,
  right,
  left,
  penup,
  pendown,
  color,
  lifespan,
  die,
  limited,
  (>*>),
  (<|>),
  while,
  ifelse,

  -- * Derived operations
  backward,
  times,
  forever,

  -- * Run functions
  runTextual,
  runGraphical
  ) where

import TimeStepProgram (
  TimeStepProgram, 
  Program(..), 
  runStep, 
  (>@>), 
  execute,
  animateStep,
  step
  )


import TimeStepParallelProgram
import qualified TimeStepParallelProgram as TSPP

import TimeStepProgram hiding (idle, execute, while, ifelse, times, forever)
import qualified TimeStepProgram as TSP

import TimeStepNextAction

import TSummary
import Turtle hiding (countdown)
import qualified Turtle as T

import Pen (Pen, PState, PColor(..))

import Textual (delayTextual, animateTextual)
import Graphical (delayGraphical, animateGraphical)

import Data.Monoid((<>))

import HGLUtils (xWin, yWin, toHGLColor)

import Graphics.HGL hiding (Time, Black)
import qualified Graphics.HGL as HGL



-- | STProgram is meant to stand for Single Turtle Program.
type STProgram = Program Energy NextAction TSummary Turtle

-- | NATProgram is meant to stand for NextAction Turtle Program.
type NATProgram = NextAction STProgram Energy

-- | TProgram stands for Turtle program.
newtype TProgram = TProgram { getp :: PlProgram STProgram }

-- For simplicity, it is easy to associate energy with a decimal quantity
type Energy = Float


-- | The idle program.
idle :: TProgram
idle = TProgram $ makePl (TSP.idle :: STProgram)

-- | The pause program. Here, the turtle pauses its movement for one time slice.
pause :: TProgram
pause = tprogram $ Program $ \e tur ->
  ((finish 0) :: NATProgram, tur, Pause)

-- | Moves the turtle forward by some amount. 
forward :: TUnits -> TProgram
forward = motion Forward move unitcForward

-- | Rotates the turtle right by some amount. By right,
--   we are referring to clockwise rotation.
right :: Degrees -> TProgram
right = motion Rotate rotate unitcRotate

-- | Rotates the turtle left by some amount. By left,
--   we are referring to counter-clockwise rotation.
left :: Degrees -> TProgram
left = motion (Rotate . negate) (rotate . negate) unitcRotate

-- | Lifts the pen up (turns drawing off)
penup :: TProgram
penup = instant (penstate False) (PState False)

-- | Puts the pen down (turns drawing on)
pendown :: TProgram
pendown = instant (penstate True) (PState True)

-- | Changes the color of the turtle's pen
color :: PColor -> TProgram
color c = instant (pencolor c) (PColor c)

-- | Kills the turtle after the specified number of steps
lifespan :: Time -> TProgram
lifespan t
  | t <= 0    = die
  | otherwise = instant (markTerminal t) (MarkTerminal t)

-- | Kills the turtle immediately
die :: TProgram
die = instant kill Kill

-- | Runs the given program for the specified number of steps,
--   or until the program is finished. Does NOT work with parallel
--   programs.
limited :: Time -> TProgram -> TProgram
limited t p = tprogram $ Program $ \e tur ->
  case (t <= 0) of -- t == 0 means that our time is up
    True  -> (finish e, tur, EndLimited False) -- Time was up before program could finish
    
    False -> let (na, tur', s_t) = runStep (getsp $ getp p) e tur -- Time is OK, run one step
                 t' = t - 1 
                 (na', s_t')  -- Result of the program after running the other (t-1) steps
                   = case na of -- Check our previous step
                       DoNext e' -- Program finished before time was up
                         -> (DoNext e', EndLimited True)
                       Continue p' -- Program is not complete
                         -> let tp' = tprogram p' -- Run the continuation of the program for (t - 1) steps
                            in (Continue (getsp $ getp $ limited t' tp'), RunLimited t')
                                
             in (na', tur', s_t <> s_t') -- Overall result

-- | Sequences two programs together. Note that the implementation
--   takes into account of programs sequenced multiple times.
(>*>) :: TProgram -> TProgram -> TProgram
p1 >*> p2 = tprogram $ (getsp $ getp p1) >@> (getsp $ getp p2)

-- | Sequences two programs together and turns them into a parallel program.
(<|>) :: TProgram -> TProgram -> TProgram
(<|>) p1 p2 = TProgram $ (getp p1) <||> (getp p2)

-- | Runs the given program while the Turtle meets some condition
while :: (Turtle -> Bool) -> TProgram -> TProgram
while c = tprogram . TSP.while c . getsp . getp

-- | Creates a program that executes one program or the other based on whether
--   the turtle meets some passed in condition.
ifelse :: (Turtle -> Bool) -> TProgram -> TProgram -> TProgram
ifelse c p1 p2 = tprogram $ TSP.ifelse c (getsp $ getp p1) (getsp $ getp p2)

-- | Moves the turtle backwards. Based on the Logos primer, the turtle
--   is a primitive creature. So the actual program will first rotate
--   the turtle one hundred eighty degrees, and then move it forward.
backward :: TUnits -> TProgram
backward = (right 180 >*>) . forward

-- | Runs a program a specified number of times
times :: Int -> TProgram -> TProgram
times t = tprogram . TSP.times t . getsp . getp

-- | Runs the given program forever
forever :: TProgram -> TProgram
forever = tprogram . TSP.forever . getsp . getp

-- | Runs the textual interface for the given program
runTextual :: TProgram -> IO ()
runTextual p = do putStrLn "Turtle state is shown as (pos, orient, pen, status)"
                  TSPP.execute (delayTextual) animateTextual (getp p) maxEnergyPerStep initTurtle

-- Runs the graphical interface for the given program
runGraphical :: TProgram -> IO ()
runGraphical p = runGraphics $ do
  w <- openWindowEx "Turtle!" Nothing (xWin, yWin) DoubleBuffered (Just frameDelay)

  -- Draw the background
  let bgcolor = toHGLColor Black
  drawInWindow w $ withColor bgcolor (polygon [(0, 0), (0, yWin), (xWin, yWin), (xWin, 0)])
  
  TSPP.execute (delayGraphical w) (animateGraphical w) (getp p) maxEnergyPerStep initTurtle
  getKey w >> return ()




-- Helper functions and values

type TNextAction = NextAction TProgram Energy

-- Short constructor to avoid repeated composition
tprogram = TProgram . makePl

-- | Helper function to create a program that makes the Turtle move somehow.
--   Useful for moving forward or rotating. Takes in a constructor to indicate
--   the TSummary, an action to execute on the turtle, the energy cost of a single
--   unit of motion, and the actual amount of the motion.
motion :: (Float -> TSummary) 
       -> (Float -> Turtle -> Turtle)
       -> Float
       -> Float 
       -> TProgram
motion constr act unitc dist = tprogram $ Program $ \e tur ->
  let ecost = (assertPositive dist) * unitc
  in if ecost <= e -- Program is finished after this time step.
     then 
       let e' = e - ecost
       in (finish e', act dist tur, constr dist)
     else -- Program will be partially finished, so it starts again.
       let max = e / unitc
           nextProg = motion constr act unitc (dist - max)
       in (Continue (getsp $ getp nextProg), act max tur, constr max)

-- | Helper function to create an instantaneous Turtle program that
--   modifies some static aspect of the turtle (e.g. its pen color,
--   configuration, life span, etc.) Takes in a function to modify 
--   the turtle, and the turtle op that was executed.
instant :: (Turtle -> Turtle)
        -> TSummary
        -> TProgram
instant modify op = tprogram $ Program $ \e tur ->
  (finish e, modify tur, op)
       
-- | Asserts that the passed in number is positive. Throws an error if
--   the assertion fails.
assertPositive :: (Num a, Ord a) => a -> a
assertPositive x
  | x < 0 = error "Must have a positive argument!"
  | otherwise = x
 

-- | This represents the # of milliseconds that the program will wait
--   before re-animating the Turtle.
frameDelay :: HGL.Time
frameDelay = 50
 
-- | This represents the maximum energy of the Turtle per time step.
--   Forward movement and left/right rotations will consume energy,
--   while all other actions will be instantaneous.
maxEnergyPerStep :: Energy
maxEnergyPerStep = 1000.0

-- | This represents the maximum amount of steps that the Turtle can
--   move forward in a single time step.
maxForward :: TUnits
maxForward = 10.0
  
-- | This represents the energy cost per unit measure that the Turtle moves
--   forward.
unitcForward :: TUnits
unitcForward = unitCost maxForward
  
-- | This represents the maximum amount that the Turtle can rotate in
--   a single time step.
maxRotate :: Degrees
maxRotate = 15.0

-- | This represents the energy cost per degree that the Turtle is rotated.
unitcRotate :: Degrees
unitcRotate = unitCost maxRotate 

-- | Computes the energy cost per unit of movement.
unitCost :: Float -> Energy
unitCost = (maxEnergyPerStep /)   
