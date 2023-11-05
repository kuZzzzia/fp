module Main where

import qualified CPS (main)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  CPS.main
