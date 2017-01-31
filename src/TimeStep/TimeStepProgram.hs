{-# LANGUAGE MultiParamTypeClasses, 
    FlexibleInstances,
    FunctionalDependencies, 
    AllowAmbiguousTypes,
    ScopedTypeVariables    
 #-}
{- | This module contains the definition of the TimeStepProgram type class, and 
     a base type that implements its interface.
-}
module TimeStepProgram (
  TimeStepProgram(..),
  Program(..)
  ) where

import TimeStepNextAction
import TimeStepObject 
import TimeStepSummary

import Data.Monoid ((<>))

{- | The TimeStepProgram type class represent simple programs where, when given an 
     object, they do some operations on it when executed. Execution is carried out
     in  step-by-step basis (like frame rates in games), where each step returns
     the following three-tuple:
                           (NextAction, NewObject, Summary) 
     where NextAction is the next step to take, NewObject is the modified
     object, and Summary represents all of the operations that were performed
     on the object during that step. Note that for a single program, if the
     current program did finish, it means that we are finished executing the
     program. The remaining energy amount after a program is finished only
     matters when sequencing programs together.
     
     For the present project, this class can be used as a base to create turtle
     programs.
     
     Note: I could probably have separated the program part from the stepping,
     animation, and execution parts, but I did not want to create any more extra 
     type classes.
-}
class (TimeStepNextAction n, TimeStepObject o) =>
       TimeStepProgram e n s o p | p -> e n s o where
  -- | This takes in a program, the maximum number of allowable operations,
  --   and an object, and steps through an instance of time, returning
  --  (NextAction, NewObject, Summary).
  step  :: p
        -> e 
        -> o 
        -> (n p e, o, s)
  
  -- | This is a program that does nothing. The individual implementations can
  --   independently figure out how much of a step it should take (e.g. is it
  --   instantaneous, does it take up a single step, etc.).
  idle :: p
        
  -- | Sequencing operator for programs
  (>@>) :: p -> p -> p
  
  -- | Creates a program that executes the passed in program while some condition
  --   about the object is satisfied
  while  :: (o -> Bool) -> p -> p
  
  -- | Creates a program that executes one program or the other based on the condition
  --   of the passed in object.
  ifelse :: (o -> Bool) -> p -> p -> p
  
  -- | Creates a program that repeats the given program n times
  times :: Int -> p -> p
  times t = foldr1 (>@>) . take t . repeat
  
  -- | Creates a program that runs the given program forever.
  forever :: p -> p
  forever p = p >@> (forever p)
        
  -- | This is similar to step, except that it returns an animation
  --   of the operations that occurred during that specific step.
  animateStep :: 
          (o -> s -> IO ())
        -> p
        -> e
        -> o
        -> ((n p e, o), IO ())
  animateStep animate p e o 
    = let (na, o', s) = step p e o
      in  ((na, o'), animate o s)
      
  -- | Executes the given program, producing an animation of the events that
  --   occurred. Execution stops if the object is dead or dies during execution.
  --   Note that the energy parameter passed here is the maximum energy. The passed
  --   in IO action represents the delay between each animation, useful for the
  --   graphical interface, as an example.
  execute :: IO ()
          -> (o -> s -> IO ())
          -> p
          -> e
          -> o
          -> IO ()
  execute delay animate p maxe o
    = do putStrLn "Executing program...\n"
         execute' p o
         putStrLn "Finished executing program."
   where
     execute' :: p -> o -> IO ()
     execute' p o = let ((na :: n p e, o'), act) = animateStep animate p maxe o
                    in do delay
                          act
                          if (dead o') -- Did we kill the object? 
                            then
                              return ()
                            else
                              maybe (return ())
                                    ((flip execute') o')
                                    (nextm na)
                            
infixr 9 >@>
                      
                      
                      
                      
-- Simple, generic program.
newtype Program e n s o 
  = Program { runStep :: e -> o -> (n (Program e n s o) e, o, s) }  

instance (Num e, Ord e, TimeStepNextAction n, TimeStepSummary s, TimeStepObject o) => 
          TimeStepProgram e n s o (Program e n s o) where
          
  -- | Every step, we countdown the object's timer if it is terminal. If this step
  --   is the step that it is supposed to die, then we can't do anything else.
  step p e o 
    | terminal o = let o' = countdown o
                   in if dead o'
                        then
                          (finish 0, o', killed)
                        else
                          let (na, o'', s) = runStep p e o'
                          in (na, o'', dying <> s)
                        
    | otherwise = runStep p e o
                  
  -- Takes up a single time slice/step.
  idle = Program $ \e o -> (finish e, o, nothing)
  
  {- | First run p1. If p1 is partially complete, then we need to run the remaining
       portion of p1 and all of p2 in our next steps.
       
       If p1 is completely finished, then we check if the object died during p1.
       If it did, we're done. Otherwise, we see if we have enough energy to run
       p2. If so, we run p2 and then return p2's result, being sure to append
       p1's summary of operations to p2's summary.
  -}
  p1 >@> p2 = Program $ \e o ->
    let p1Res@(na, o', s1) = runStep p1 e o
    in case (nextm na) of -- Did the first program completely finish in this step?
         (Just p1') -> (continue (p1' >@> p2), o', s1)
         
         Nothing    -> if dead o' -- Did the object die during p1?
                         then
                           (finish 0, o', s1)
                         else
                            let (Just e') = reme na
                            in if e' > 0 -- Is there enough energy left?
                                 then
                                   let (na2, o'', s2) = runStep p2 e' o'
                                   in (na2, o'', s1 <> s2)
                                 else -- We're done.
                                   (continue p2, o', s1)                             
  
  while c p = Program $ \e o -> 
    case (c o) of
      False -> (finish 0, o, mempty)
      True  -> runStep (p >@> (while c p)) e o
      
  ifelse c p1 p2 = Program $ \e o ->
    case (c o) of
      True  -> runStep p1 e o
      False -> runStep p2 e o 
