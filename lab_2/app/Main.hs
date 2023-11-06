module Main where

import qualified CPS (cbn, cbv, scanTerms)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs -- getArgs returns IO monad
    contents <- readFile . getInputFile $ args -- readFile returns IO monad 
    let terms = CPS.scanTerms contents
    putStrLn $ "Terms parsed: " ++ show terms
    putStrLn $ "CBN: " ++ CPS.cbn  terms
    putStrLn $ "CBV: " ++ CPS.cbv  terms
    


-- gets filename with test from command line
-- if len(args) != 1 file'll be set to default = test.hs
getInputFile :: [String] -> String
getInputFile [file] = file
getInputFile _ = "test.hs"