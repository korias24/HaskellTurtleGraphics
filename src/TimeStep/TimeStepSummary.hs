{- | This module contains the definition of the TimeStepSummary
-}
module TimeStepSummary (
  TimeStepSummary(..)
  ) where

{- | The TimeStepSummary type class summarizes all of the operations that
     were carried out on the object in a single time step. Summaries should
     be capable of being "concatenated" together.
-}
class (Monoid s) => TimeStepSummary s where
  -- | This indicates that the object did not do anything during the operation.
  nothing :: s
  nothing = mempty
  
  -- | This indicates that the object was killed during the time step. If the
  --   object cannot die, this will be nothing by default.
  killed :: s
  killed = nothing

  -- | This indicates that the object is dying, its remaining life span decremented
  --   by one time step. If the object cannot die, this will be nothing by default.
  dying :: s
  dying = nothing

            

            


