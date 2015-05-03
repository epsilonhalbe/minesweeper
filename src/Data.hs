module Data where

import qualified Data.Map as M

data Bomb = Bomb deriving (Eq, Show)

newtype GameBoard = GameBoard { get :: M.Map (Int,Int) Bomb } deriving (Eq,Show)

readGameBoard :: String -> GameBoard
readGameBoard str = let board = lines str
                        cols  = length $ head board
                        rows  = length board
                        index = [(i,j)|i <- [1..rows], j <- [1..cols]]
                    in  GameBoard (M.map (const Bomb) . M.fromList $ filter (('x'==).snd) $ zip index (clean str))
                  where clean = filter (`notElem` "\n\r")
