module Main where

import Data
import Data.GameBoard
import Data.GameBoard.Random
import System.Random (newStdGen)

import Control.Applicative ((<$>))
import System.Environment (getArgs)

main :: IO ()
main = do fname:_ <- getArgs
          g <- newStdGen
          let gb = genBoard g
          writeFile ("in"++fname) (printGameBoard gb)
          writeFile ("out"++fname) (printArray $ dangerBoard gb)
