module Main where

import Data
import Control.Applicative ((<$>))
import System.Environment (getArgs)

main :: IO ()
main = do let gb = genBoard 10
          putStrLn . printArray . dangerBoard $ gb
