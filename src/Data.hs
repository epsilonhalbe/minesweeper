module Data where

import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.Map as M
import Data.Maybe (catMaybes)

data Bomb = Bomb deriving (Eq)

instance Show Bomb where
  show Bomb = "💣"

data Danger = Level Int | Infinity deriving (Eq)

instance Show Danger where 
  show (Level n) = show n
  show (Infinity) = "B"--"💣"

data GameBoard = GameBoard { get :: M.Map (Int,Int) Bomb, row :: Int, col :: Int}
               deriving (Eq,Show)

readGameBoard :: String -> GameBoard
readGameBoard str = let board = lines str
                        cols  = length $ head board
                        rows  = length board
                    in  GameBoard (M.map (const Bomb) . M.fromList $
                         filter (('x'==).snd) $ zip (index rows cols) (clean str)) cols rows
                  where clean = filter (`notElem` "\n\r")

dangerBoard :: GameBoard -> A.Array (Int,Int) Danger
dangerBoard (GameBoard gb r c) = let ix = index r c
                                 in A.listArray ((1,1),(r,c)) (map aux ix)
  where aux xy = case M.lookup xy gb
                    of Nothing -> Level $ length . catMaybes $ map (`M.lookup` gb) (neighbours xy)
                       Just Bomb -> Infinity

index :: Int -> Int -> [(Int,Int)]
index r c = A.range ((1,1),(r,c))

γ :: (a -> b) -> [a] -> [(a,b)]
γ f = map (\x -> (x, f x))

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) = [(x+i,y+j)|i<-[-1,0,1]
                             ,j<-[-1,0,1]]

printArray :: Show a => A.Array (Int,Int) a -> String
printArray arr = let (_,(i,j)) = A.bounds arr
                 in unlines [unwords [show (arr ! (x, y)) | y <- [1..j]
                                     ]
                                                          | x <- [1..i]
                            ]



