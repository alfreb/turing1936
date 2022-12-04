module Main where

import qualified Turing1936 as T36

-- Execute a few steps of Turing's first two example machines
-- Initial state is provided, with just enough tape in this case.
-- Provide inifinite tape to prove Turing Completeness.

n1 = 10
tm1init :: T36.CompleteConfiguration
tm1init = (T36.𝔟, 0, take 50 $ repeat ' ')

n2 = 80
tm2init :: T36.CompleteConfiguration
tm2init = (T36.𝔟, 0, take 100 $ repeat ' ')


main :: IO ()
main = do
  putStrLn ("\n\nExecuting " ++ show(n1) ++ " moves of Turing's first example")
  T36.printSteps n1 T36.tm1 tm1init
  
  putStrLn ("\n\nExecuting " ++ show(n2) ++ " moves of Turing's second example")  
  T36.printSteps n2 T36.tm2 tm2init
  
  
