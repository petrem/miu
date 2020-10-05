module Main where

import Miu

main :: IO ()
main = do
  let result = rule1 $ readTheorem "MI"
  putStrLn $ "Result: " ++ (show result)
