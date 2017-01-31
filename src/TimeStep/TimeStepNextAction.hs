{- | This module contains the definition of the TimeStepNextAction type class,
     and also a base type that implements it.
-}
module TimeStepNextAction (
  TimeStepNextAction(..),
  NextAction(..)
  ) where
  

{- | The TimeStepNextAction type class, in the context of a time step, indicates
     if the current program finished, or if it is partially complete.
     
     For any instance n to be a valid TimeStepNextAction type, we must have the
     following rule hold for all n:
        reme n ~= contm n
     
     Basically saying that if contm returns a Just object, then reme must return
     Nothing and vice versa.
-}
class TimeStepNextAction n where
  -- | Returns the energy remaining to use with the next program.
  --   If the current program did not finish, then this returns Nothing.
  reme  :: n m e -> Maybe e
  
  -- | Indicates that the program has finished execution with a remaining
  --   amount of energy left over.
  finish :: e -> n m e
  
  -- | Returns the next portion of the program to execute if the current one was 
  --   only partially completed. If the main operation did finish, this returns 
  --   Nothing.
  nextm :: n m e -> Maybe m
  
  -- | Indicates that the program is only partially completed, with the remaining
  --   portion to be executed later.
  continue :: m -> n m e
  
  
-- | Basic NextAction type. We either do the next program, whereby
--   we have some leftover energy to use, or we've
--   partially finished our current program to execute at the next step.
data NextAction m e =  DoNext e
                     | Continue m
               
instance TimeStepNextAction NextAction where
  reme (DoNext e)      = Just e
  reme _               = Nothing

  finish               = DoNext
  
  nextm (Continue m)   = Just m
  nextm _              = Nothing 
  
  continue             = Continue
            

            


