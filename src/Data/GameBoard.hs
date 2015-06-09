module Data.GameBoard where

import qualified Data.Map as M
import qualified Data.Array as A

data Bomb = Bomb deriving (Eq)

instance Show Bomb where
--  show Bomb = "💣"
  show Bomb = "B"

data Danger = Level Int | Infinity deriving (Eq)

instance Show Danger where 
  show (Level n) = if n == 0 then " " else show n
--  show (Infinity) = "💣"
  show (Infinity) = "B"

data GameBoard = GameBoard { get :: M.Map (Int,Int) Bomb, row :: Int, col :: Int}
               deriving (Eq,Show)


readGameBoard :: String -> GameBoard
readGameBoard str = let board = lines str
                        cols  = length $ head board
                        rows  = length board
                    in  GameBoard (M.map (const Bomb) . M.fromList $
                         filter (('x'==).snd) $ zip (index rows cols) (clean str)) cols rows
                  where clean = filter (`notElem` "\n\r")

index :: Int -> Int -> [(Int,Int)]
index r c = A.range ((1,1),(r,c))


