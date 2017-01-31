{- | This is the module containing the Pen type that's used
     by the Turtle.
-}
module Pen (
  -- Types
  Pen,
  PState,
  PColor(..),
  
  -- Accessors
  getp,
  
  -- Operations
  makePen,
  isDown,
  pColor,
  newColor,
  newState
  ) where
  
-- | This is a Pen datatype for the Turtle. The Turtle's pen
--   is either in the "Up" (False) or "Down" (True) state, where
--   "Up" means the Turtle cannot draw, while "Down" means the Turtle
--   can draw.
newtype Pen = Pen { getp :: (PState, PColor) }
  deriving Show -- for debugging

-- | Indicates if the pen is "up" (no drawing) or "down" (can draw)
type PState = Bool

-- Data type for the color of a pen. Can add more here.
data PColor = Black
            | Blue
            | Green
            | Cyan
            | Red
            | Magenta
            | Yellow
            | White
  deriving Show  

-- | Constructor to create a Pen
--   By default, the pen will be down.
makePen :: PColor -> Pen
makePen = Pen . (,) True

-- | Indicates if the pen is currently down or not.
isDown :: Pen -> Bool
isDown = fst . getp

-- | Gets the current pen's color
pColor :: Pen -> PColor
pColor = snd . getp

-- | Changes the pen's color
newColor :: PColor -> Pen -> Pen
newColor c p = let (s, _) = getp p
               in  Pen (s, c)
                        
-- | Changes the pen's state
newState :: PState -> Pen -> Pen
newState s p = let (_, c) = getp p
               in  Pen (s, c)   
