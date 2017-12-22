module Main where

import System.Environment(getArgs)
import FractalTree

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "error: usage: fractal-tree nbColumns nbRows"
    else do
      let [cols, rows] = args
      putStrLn $ "# asked for a " ++ cols ++ " columns and " ++ rows ++ " rows fractal tree"
      putStrLn $ unlines $ drawTree (read cols) (read rows)
