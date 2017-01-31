{- | This is the module containing the Turtle type
     to be used across the programs.
-}
module Turtle (
  -- Types
  Turtle,
  TUnits,
  Degrees,
  Time,
  Position,
  
  -- Accessors
  pos,
  orient,
  pen,
  status,
  
  -- Initial turtle (to pass into programs when executing)
  initTurtle,
  
  -- Turtle operations
  turtle,
  move,
  rotate,
  dead,
  terminal,
  kill,
  markTerminal,
  countdown,
  penstate,
  pencolor
  ) where
  
import Pen

import TimeStepObject (TimeStepObject)
import qualified TimeStepObject as TSO
  
-- | This is the Turtle datatype. A turtle consists of its position
--   (standard 2D xy axes), orientation, pen and its status.
data Turtle = Turtle {
  pos :: Position, 
  orient :: Orientation,
  pen :: Pen, 
  status :: Status
  } deriving Show -- for debugging
  
-- | TimeStepObject instance for the Turtle class
instance TimeStepObject Turtle where
  terminal  = (/= Nothing) . terminal
  countdown = countdown
  dead      = dead
  kill      = kill


type Position = (TUnits, TUnits)
type Orientation = Float

type TUnits = Float  -- TUnits is read as TurtleUnits  

-- | Status of the Turtle. Terminal means that the Turtle will die
--   in "x" number of steps.
data Status = Alive
            | Dead
            | Terminal Time
  deriving Show -- for debugging
  
  
initTurtle :: Turtle
initTurtle = turtle (0, 100)


-- | Creates a new turtle starting at the given position
--   Initially, the turtle's pen is down and it is white.
--   It is also facing "north" (up) with respect to a standard
--   xy coordinate system.
turtle :: Position -> Turtle
turtle p = Turtle p (pi/2) (makePen White) Alive

-- | Moves the turtle in its current direction
move :: TUnits -> Turtle -> Turtle
move d turtle = let o = orient turtle
                in translate (d * cos(o), d * sin(o)) turtle
                
type Degrees = Float
                
-- | Rotates the turtle by the specified amount.
--   Note that negative values means rotating the turtle "left" (CCW)
--   while positive values means rotating the turtle "right" (CW).
--   The parameter should be in degrees. Could bind the angle r
--   to be between -2*pi and 2*pi to improve this, but this is simpler.
rotate :: Degrees -> Turtle -> Turtle
rotate r = modifyOrient (bound . ((+) $ toTurtleRot r))
                
-- | Checks if the turtle is dead.
dead :: Turtle -> Bool
dead (Turtle _ _ _ Dead) = True
dead _                 = False

-- | Checks if the turtle is terminal. Returns the time remaining before it dies
--   if so, otherwise nothing.
terminal :: Turtle -> Maybe Time
terminal (Turtle _ _ _ (Terminal t)) = Just t
terminal _                           = Nothing

-- | Kills the turtle.
kill :: Turtle -> Turtle
kill = modifyStatus (const Dead)

type Time = Int

-- | Marks the turtle for death after some time, t has passed.
markTerminal :: Time -> Turtle -> Turtle
markTerminal t = modifyStatus toTerminal
  where
   toTerminal ts@(Terminal t')
     | t < t' = Terminal t
     | otherwise = ts
   toTerminal Dead = Dead
   toTerminal s    = Terminal t     

-- | Counts down the clock of a terminal turtle. Note that the
--   passed in turtle must be in the Terminal state for this
--   to work, otherwise the state is returned unmodified. If the Turtle's 
--   terminal timer is 0, then the turtle is killed instead.
countdown :: Turtle -> Turtle
countdown = modifyStatus update
  where
   update (Terminal t)
     | t <= 0    = Dead
     | otherwise = Terminal (t - 1)
   update s = s

-- | Changes the state of the Turtle's pen.
penstate :: PState -> Turtle -> Turtle
penstate s = modifyPen (newState s)

-- | Changes the color of the turtle's pen.
pencolor :: PColor -> Turtle -> Turtle
pencolor color = modifyPen (newColor color)

-- | Helper functions

type TVector = (TUnits, TUnits)

translate :: TVector -> Turtle -> Turtle
translate (vx, vy) = modifyPos (\(x, y) -> (x + vx, y + vy))

modifyPos :: (Position -> Position) -> Turtle -> Turtle
modifyPos f turtle = turtle { pos = f $ pos turtle }

modifyOrient :: (Orientation -> Orientation) -> Turtle -> Turtle
modifyOrient f turtle = turtle { orient = f $ orient turtle }

modifyStatus :: (Status -> Status) -> Turtle -> Turtle
modifyStatus f turtle = turtle { status = f $ status turtle }

modifyPen :: (Pen -> Pen) -> Turtle -> Turtle
modifyPen f turtle = turtle { pen = f $ pen turtle }

-- | Converts a given rotation amount to a rotation
--   in turtle coordinates.
toTurtleRot :: Degrees -> Orientation
toTurtleRot = (* pi) . (/ 180) . negate 

-- | Binds the given angle to the interval [-2pi, 2pi]
bound :: Orientation -> Orientation
bound f
  | f > 2*pi = bound' (2*pi) f
  | f < ((-2)*pi) = bound' ((-2)*pi) f
  | otherwise = f
  where
    truncateO :: Orientation -> Orientation
    truncateO = fromInteger . truncate
    
    bound' :: Orientation -> Orientation -> Orientation
    bound' m o = let i = truncateO $ o / m
                 in o - i * m
    
