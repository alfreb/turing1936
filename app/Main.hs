module Main where

import qualified Turing1936 as T36

-- Execute a few steps of Turing's first two example machines
-- Provide inifinite tape to prove Turing Completeness.

n1 = 10
n2 = 80


main :: IO ()
main = do
  putStrLn ("\n\nExecuting " ++ show(n1) ++ " moves of Turing's first example")
  T36.printSteps n1 T36.tm1 
  
  putStrLn ("\n\nExecuting " ++ show(n2) ++ " moves of Turing's second example")  
  T36.printSteps n2 T36.tm2 
  
  
