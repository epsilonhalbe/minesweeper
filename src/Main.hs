module Main where

import Data

main :: IO ()
main = let gameBoard1  = unlines ["---" ,"-x-" ,"---"]
       in putStrLn . printArray . dangerBoard $ readGameBoard gameBoard1
