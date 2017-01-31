{- | This module stores the data type that summarizes all of the operations that
     can occur when executing a single step of the Turtle program.
-}
module TSummary(
  TSummary(..),
  ProgStatus
  ) where

import Pen (PState, PColor)
import Turtle (TUnits, Degrees, Time)

import TimeStepSummary

-- | The possible operations that a Turtle program can do in a single step is:
--        1. Primitive operations on the Turtle (e.g. moving it)
--
--        2. Change the turtle's pen's color, or put the pen up or place it down
--
--        3. Marking the Turtle for death at some specified time
--
--        4. Running a program on the Turtle that's only to execute for a limited
--           period of time.
--
--        5. Ending a limited program. Either the program was able to finish,
--           or its alloted time ran out.
--
--        6. It can also do a sequence of operations.
data TSummary = Idle
              | Pause
              | Kill
              | Dying
              | Forward TUnits
              | Rotate Degrees
              | PState PState
              | PColor PColor
              | MarkTerminal Time
              | RunLimited Time
              | EndLimited ProgStatus
              | TSummary :>#> TSummary
              
infixr 9 :>#>

-- | Program status for limited. When a limited program ends, it ended either
--   because the inner program finished, or time ran out. True means the program
--   finished, false means that the time ran out.
type ProgStatus = Bool

-- | Monoid instance for TSummary so it can be used with the TimeStepSummary class.
instance Monoid TSummary where
  mempty = Idle
  mappend = combine
  
instance TimeStepSummary TSummary where
  killed = Kill
  dying  = Dying
  
-- Helper function for mappend  
combine :: TSummary -> TSummary -> TSummary

-- Monoid laws
Idle `combine` p = p
p `combine` Idle = p

-- Eliminate redundancies
(Forward x) `combine` (Forward y) = Forward (x + y)

r1@(Rotate x) `combine` r2@(Rotate y)
  | sameSign x y = Rotate (x + y)
  | otherwise    = r1 :>#> r2
  
(PState _) `combine` ps@(PState _) = ps
(PColor _) `combine` pc@(PColor _) = pc

ts1 `combine` ts2 = ts1 :>#> ts2


-- Checks if two numbers are the same sign
sameSign :: (Num a, Ord a) => a -> a -> Bool
sameSign x y = (x  > 0 && y > 0) || (x < 0 && y < 0)
