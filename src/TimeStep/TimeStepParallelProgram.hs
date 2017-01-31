{-# LANGUAGE MultiParamTypeClasses, 
    FlexibleInstances,
    ScopedTypeVariables    
 #-}
{- | This module contains the definition of the TimeStepParallelProgram type class,
     and a base type that implements it.
-}
module TimeStepParallelProgram (
  TimeStepParallelProgram(..),
  PlProgram(..)
  ) where

import TimeStepNextAction
import TimeStepObject 
import TimeStepSummary

import TimeStepProgram hiding (step, animateStep)
import qualified TimeStepProgram as TSP


import Data.Monoid ((<>))
import Data.Maybe (fromJust, isJust)

{- | The TimeStepParallelProgram type class represent programs that can be run
     in parallel. Here, parallel means that the Turtle forks off into different paths,
     where each path represents some program.

     The operator <|> represents two parallel programs that can be combined to form
     a single, composite parallel program.

     Note that pl represents a type that can parallelize a program of type p.
-}
class (TimeStepNextAction n, TimeStepObject o) =>
       TimeStepParallelProgram e n s o pl where
  
  -- | Same idea as "step" in TimeStepProgram, except for parallel programs.
  step :: (TimeStepProgram e n s o p) => pl p -> e -> o -> [(n p e, o, s)]

  -- | Similar to animateStep in TimeStepProgram, but for parallel programs.
  animateStep :: (TimeStepProgram e n s o p) 
               => (o -> s -> IO ())
               -> pl p
               -> e
               -> o
               -> [((n p e, o), IO ())]
  animateStep animate p e o = let animateP (na, o, s) = ((na, o), animate o s)
                               in map animateP $ step p e o 

  -- | This method takes a single program and turns it into a parallelizable
  --   program
  makePl :: (TimeStepProgram e n s o p) => p -> pl p 

  -- | This operator takes two parallel programs and combines them together to form a
  --   composite parallel program.
  (<||>) :: (TimeStepProgram e n s o p) => pl p -> pl p -> pl p


  -- | This is overwriting the execute function in TimeStepProgram, since now we have parallel 
  --   programs to deal with.
  execute ::  (TimeStepProgram e n s o p) => IO ()
           -> (o -> s -> IO ())
           -> pl p
           -> e
           -> o
           -> IO ()
  execute delay animate p maxe o = do putStrLn "Executing program...\n"
                                      execute' (animateStep animate p maxe o)
                                      putStrLn "Finished executing program."      
    where
      -- | Executes a parallel program. Note that the first part is animated just to
      --   make it easier to recurse nicely. The input represents the result of animating
      --   a single step for each parallel program.
      execute' :: (TimeStepProgram e n s o p) => [((n p e, o), IO ())] -> IO ()
      execute' [] = return ()
      execute' ps 
        = do delay
             sequence_ $ map snd ps    

             -- Are all the parallel programs done with their computation?
             let getnm   = \(na :: n p e, o) -> (o, nextm na) -- Get next action
                 nextms  = map (getnm . fst) ps -- Get the next programs (if any) 

                 -- Check if there's any additional action left to take in each parallel
                 -- program and that the turtles are still alive. These are the valid programs
                 -- that we can execute next.
                 vnextms = filter (\(o, mnextm) -> (not . dead) o && isJust mnextm) nextms

                 vnextms' = map (fmap fromJust) vnextms

             -- Execute the next step on the remaining parallel programs. 
             execute' $ map (\(o, p) -> TSP.animateStep animate p maxe o) vnextms'


-- | General type for a parallel program. PlProgram = Parallel Program.
--   Here, a parallel program can be a single program, or multiple programs
--   grouped together.
data PlProgram p = S { getsp :: p }
                 | (PlProgram p) :<|> (PlProgram p)


instance (TimeStepNextAction n, TimeStepObject o) =>
         TimeStepParallelProgram e n s o PlProgram where

  step (S p) e o = [TSP.step p e o]
  step (p1 :<|> p2) e o = step p1 e o ++ step p2 e o

  makePl = S

  (<||>) = (:<|>)
