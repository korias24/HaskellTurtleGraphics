{- | This module contains the definition of the TimeStepObject type class.
-}
module TimeStepObject (
  TimeStepObject(..)
  ) where

{- | The TimeStepObject type class, in the context of a time step, indicates
     the object that we're stepping through. For example, we could be stepping
     through the motion of an animal, or a particle.
-}
class TimeStepObject o where
  -- | Returns true if the object is soon to die/terminal. If the object
  --   has no such quality, then return False.
  terminal :: o -> Bool

  -- | If the object is terminal, this function will mark its timer down,
  --   returning the modified object. If the object is not terminal, then 
  --   this function should return the unmodified object.
  countdown :: o -> o
  
  -- | Returns True if the object is dead, false otherwise. If the object
  --   cannot ever die, return False.
  dead :: o -> Bool
  
  -- | Kills the object. If the object is not supposed to die, it should just
  --   return the unmodified object.
  kill :: o -> o

            

            


