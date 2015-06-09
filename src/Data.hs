{-# LANGUAGE BangPatterns #-}

module Data where

import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe (catMaybes,maybe )
import Data.GameBoard
import Data.GameBoard.Random

dangerBoard :: GameBoard -> A.Array (Int,Int) Danger
dangerBoard (GameBoard gb r c) = let ix = index r c
                                 in A.listArray ((1,1),(r,c)) (map aux ix)
  where aux xy = case M.lookup xy gb
                    of Nothing -> Level $ length . catMaybes $ map (`M.lookup` gb) (neighbours xy)
                       Just Bomb -> Infinity

γ :: (a -> b) -> [a] -> [(a,b)]
γ f = map (\x -> (x, f x))

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) = [(x+i,y+j)|i<-[-1,0,1]
                             ,j<-[-1,0,1]]

printArray :: Show a => A.Array (Int,Int) a -> String
printArray arr = let (_,(i,j)) = A.bounds arr
                 in unlines [unwords [show (arr A.! (x, y)) | y <- [1..j]
                                     ]
                                                          | x <- [1..i]
                            ]

solve :: String -> String
solve = printArray . dangerBoard . readGameBoard

printGameBoard :: GameBoard -> String
printGameBoard gb = let r = row gb
                        c = col gb
                    in unlines $ (show r++" "++show c):[unwords [maybe "-" aux (M.lookup (x, y) (get gb)) | y <- [1..c]
                                        ]
                                                                                   | x <- [1..r]
                               ]
                    where aux Bomb = "x"

replicateM :: (Ord k, Monad m) => Int -> m k -> m v -> m (M.Map k v)
replicateM = replicateM' M.empty
  where --replicateM' :: (Ord k) => M.Map k v -> Int -> m k -> m v -> m (M.Map k v)
        replicateM' !acc 0 _ _ = return acc
        replicateM' !acc !n kGen vGen =
             do k <- kGen
                v <- vGen
                if k `M.member` acc
                  then replicateM' acc n kGen vGen
                  else replicateM' (M.insert k v acc) (n-1) kGen vGen

clickBoa
