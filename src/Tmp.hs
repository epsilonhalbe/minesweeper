module Tmp where

import Data.Function (on)
import Data.List ((\\),subsequences, sortBy, groupBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data

f x = map (\y -> if y `elem` x then '-' else 'x') [1..9]
minimalBoards = subsequences ([1..9]\\[5]) :: [[Int]]
sortBoards = groupBy ((==) `on` length) $ sortBy (comparing length) minimalBoards :: [[[Int]]]
allPossibleInputs = map (map (unlines . chunksOf 3 . f)) sortBoards

writeTestFiles = do let files0 = zip [1..] (map ("3 3\n"++) bomb_0)
                        files1 = zip [1..] (map ("3 3\n"++) bomb_1)
                        files2 = zip [1..] (map ("3 3\n"++) bomb_2)
                        files3 = zip [1..] (map ("3 3\n"++) bomb_3)
                        files4 = zip [1..] (map ("3 3\n"++) bomb_4)
                        files5 = zip [1..] (map ("3 3\n"++) bomb_5)
                        files6 = zip [1..] (map ("3 3\n"++) bomb_6)
                        files7 = zip [1..] (map ("3 3\n"++) bomb_7)
                        files8 = zip [1..] (map ("3 3\n"++) bomb_8)
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_0-"++show n) str) files0
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_1-"++show n) str) files1
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_2-"++show n) str) files2
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_3-"++show n) str) files3
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_4-"++show n) str) files4
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_5-"++show n) str) files5
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_6-"++show n) str) files6
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_7-"++show n) str) files7
                    mapM_ (\(n,str) -> writeFile ("testFiles/bomb_8-"++show n) str) files8

writeDangerFiles = do let files0 = zip [1..] (map solve bomb_0)
                          files1 = zip [1..] (map solve bomb_1)
                          files2 = zip [1..] (map solve bomb_2)
                          files3 = zip [1..] (map solve bomb_3)
                          files4 = zip [1..] (map solve bomb_4)
                          files5 = zip [1..] (map solve bomb_5)
                          files6 = zip [1..] (map solve bomb_6)
                          files7 = zip [1..] (map solve bomb_7)
                          files8 = zip [1..] (map solve bomb_8)
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_0-"++show n) str) files0
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_1-"++show n) str) files1
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_2-"++show n) str) files2
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_3-"++show n) str) files3
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_4-"++show n) str) files4
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_5-"++show n) str) files5
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_6-"++show n) str) files6
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_7-"++show n) str) files7
                      mapM_ (\(n,str) -> writeFile ("solutionFiles/danger_8-"++show n) str) files8


bomb_0 = ["---\n---\n---\n"]
bomb_1 = ["x--\n---\n---\n","-x-\n---\n---\n","--x\n---\n---\n","---\nx--\n---\n"
         ,"---\n--x\n---\n","---\n---\nx--\n","---\n---\n-x-\n","---\n---\n--x\n"]
bomb_2 = ["xx-\n---\n---\n","x-x\n---\n---\n","-xx\n---\n---\n","x--\nx--\n---\n"
         ,"-x-\nx--\n---\n","--x\nx--\n---\n","x--\n--x\n---\n","-x-\n--x\n---\n"
         ,"--x\n--x\n---\n","---\nx-x\n---\n","x--\n---\nx--\n","-x-\n---\nx--\n"
         ,"--x\n---\nx--\n","---\nx--\nx--\n","---\n--x\nx--\n","x--\n---\n-x-\n"
         ,"-x-\n---\n-x-\n","--x\n---\n-x-\n","---\nx--\n-x-\n","---\n--x\n-x-\n"
         ,"---\n---\nxx-\n","x--\n---\n--x\n","-x-\n---\n--x\n","--x\n---\n--x\n"
         ,"---\nx--\n--x\n","---\n--x\n--x\n","---\n---\nx-x\n","---\n---\n-xx\n"]
bomb_3 = ["xxx\n---\n---\n","xx-\nx--\n---\n","x-x\nx--\n---\n","-xx\nx--\n---\n"
         ,"xx-\n--x\n---\n","x-x\n--x\n---\n","-xx\n--x\n---\n","x--\nx-x\n---\n"
         ,"-x-\nx-x\n---\n","--x\nx-x\n---\n","xx-\n---\nx--\n","x-x\n---\nx--\n"
         ,"-xx\n---\nx--\n","x--\nx--\nx--\n","-x-\nx--\nx--\n","--x\nx--\nx--\n"
         ,"x--\n--x\nx--\n","-x-\n--x\nx--\n","--x\n--x\nx--\n","---\nx-x\nx--\n"
         ,"xx-\n---\n-x-\n","x-x\n---\n-x-\n","-xx\n---\n-x-\n","x--\nx--\n-x-\n"
         ,"-x-\nx--\n-x-\n","--x\nx--\n-x-\n","x--\n--x\n-x-\n","-x-\n--x\n-x-\n"
         ,"--x\n--x\n-x-\n","---\nx-x\n-x-\n","x--\n---\nxx-\n","-x-\n---\nxx-\n"
         ,"--x\n---\nxx-\n","---\nx--\nxx-\n","---\n--x\nxx-\n","xx-\n---\n--x\n"
         ,"x-x\n---\n--x\n","-xx\n---\n--x\n","x--\nx--\n--x\n","-x-\nx--\n--x\n"
         ,"--x\nx--\n--x\n","x--\n--x\n--x\n","-x-\n--x\n--x\n","--x\n--x\n--x\n"
         ,"---\nx-x\n--x\n","x--\n---\nx-x\n","-x-\n---\nx-x\n","--x\n---\nx-x\n"
         ,"---\nx--\nx-x\n","---\n--x\nx-x\n","x--\n---\n-xx\n","-x-\n---\n-xx\n"
         ,"--x\n---\n-xx\n","---\nx--\n-xx\n","---\n--x\n-xx\n","---\n---\nxxx\n"]
bomb_4 = ["xxx\nx--\n---\n","xxx\n--x\n---\n","xx-\nx-x\n---\n","x-x\nx-x\n---\n"
         ,"-xx\nx-x\n---\n","xxx\n---\nx--\n","xx-\nx--\nx--\n","x-x\nx--\nx--\n"
         ,"-xx\nx--\nx--\n","xx-\n--x\nx--\n","x-x\n--x\nx--\n","-xx\n--x\nx--\n"
         ,"x--\nx-x\nx--\n","-x-\nx-x\nx--\n","--x\nx-x\nx--\n","xxx\n---\n-x-\n"
         ,"xx-\nx--\n-x-\n","x-x\nx--\n-x-\n","-xx\nx--\n-x-\n","xx-\n--x\n-x-\n"
         ,"x-x\n--x\n-x-\n","-xx\n--x\n-x-\n","x--\nx-x\n-x-\n","-x-\nx-x\n-x-\n"
         ,"--x\nx-x\n-x-\n","xx-\n---\nxx-\n","x-x\n---\nxx-\n","-xx\n---\nxx-\n"
         ,"x--\nx--\nxx-\n","-x-\nx--\nxx-\n","--x\nx--\nxx-\n","x--\n--x\nxx-\n"
         ,"-x-\n--x\nxx-\n","--x\n--x\nxx-\n","---\nx-x\nxx-\n","xxx\n---\n--x\n"
         ,"xx-\nx--\n--x\n","x-x\nx--\n--x\n","-xx\nx--\n--x\n","xx-\n--x\n--x\n"
         ,"x-x\n--x\n--x\n","-xx\n--x\n--x\n","x--\nx-x\n--x\n","-x-\nx-x\n--x\n"
         ,"--x\nx-x\n--x\n","xx-\n---\nx-x\n","x-x\n---\nx-x\n","-xx\n---\nx-x\n"
         ,"x--\nx--\nx-x\n","-x-\nx--\nx-x\n","--x\nx--\nx-x\n","x--\n--x\nx-x\n"
         ,"-x-\n--x\nx-x\n","--x\n--x\nx-x\n","---\nx-x\nx-x\n","xx-\n---\n-xx\n"
         ,"x-x\n---\n-xx\n","-xx\n---\n-xx\n","x--\nx--\n-xx\n","-x-\nx--\n-xx\n"
         ,"--x\nx--\n-xx\n","x--\n--x\n-xx\n","-x-\n--x\n-xx\n","--x\n--x\n-xx\n"
         ,"---\nx-x\n-xx\n","x--\n---\nxxx\n","-x-\n---\nxxx\n","--x\n---\nxxx\n"
         ,"---\nx--\nxxx\n","---\n--x\nxxx\n"]
bomb_5 = ["xxx\nx-x\n---\n","xxx\nx--\nx--\n","xxx\n--x\nx--\n","xx-\nx-x\nx--\n"
         ,"x-x\nx-x\nx--\n","-xx\nx-x\nx--\n","xxx\nx--\n-x-\n","xxx\n--x\n-x-\n"
         ,"xx-\nx-x\n-x-\n","x-x\nx-x\n-x-\n","-xx\nx-x\n-x-\n","xxx\n---\nxx-\n"
         ,"xx-\nx--\nxx-\n","x-x\nx--\nxx-\n","-xx\nx--\nxx-\n","xx-\n--x\nxx-\n"
         ,"x-x\n--x\nxx-\n","-xx\n--x\nxx-\n","x--\nx-x\nxx-\n","-x-\nx-x\nxx-\n"
         ,"--x\nx-x\nxx-\n","xxx\nx--\n--x\n","xxx\n--x\n--x\n","xx-\nx-x\n--x\n"
         ,"x-x\nx-x\n--x\n","-xx\nx-x\n--x\n","xxx\n---\nx-x\n","xx-\nx--\nx-x\n"
         ,"x-x\nx--\nx-x\n","-xx\nx--\nx-x\n","xx-\n--x\nx-x\n","x-x\n--x\nx-x\n"
         ,"-xx\n--x\nx-x\n","x--\nx-x\nx-x\n","-x-\nx-x\nx-x\n","--x\nx-x\nx-x\n"
         ,"xxx\n---\n-xx\n","xx-\nx--\n-xx\n","x-x\nx--\n-xx\n","-xx\nx--\n-xx\n"
         ,"xx-\n--x\n-xx\n","x-x\n--x\n-xx\n","-xx\n--x\n-xx\n","x--\nx-x\n-xx\n"
         ,"-x-\nx-x\n-xx\n","--x\nx-x\n-xx\n","xx-\n---\nxxx\n","x-x\n---\nxxx\n"
         ,"-xx\n---\nxxx\n","x--\nx--\nxxx\n","-x-\nx--\nxxx\n","--x\nx--\nxxx\n"
         ,"x--\n--x\nxxx\n","-x-\n--x\nxxx\n","--x\n--x\nxxx\n","---\nx-x\nxxx\n"]
bomb_6 = ["xxx\nx-x\nx--\n","xxx\nx-x\n-x-\n","xxx\nx--\nxx-\n","xxx\n--x\nxx-\n"
         ,"xx-\nx-x\nxx-\n","x-x\nx-x\nxx-\n","-xx\nx-x\nxx-\n","xxx\nx-x\n--x\n"
         ,"xxx\nx--\nx-x\n","xxx\n--x\nx-x\n","xx-\nx-x\nx-x\n","x-x\nx-x\nx-x\n"
         ,"-xx\nx-x\nx-x\n","xxx\nx--\n-xx\n","xxx\n--x\n-xx\n","xx-\nx-x\n-xx\n"
         ,"x-x\nx-x\n-xx\n","-xx\nx-x\n-xx\n","xxx\n---\nxxx\n","xx-\nx--\nxxx\n"
         ,"x-x\nx--\nxxx\n","-xx\nx--\nxxx\n","xx-\n--x\nxxx\n","x-x\n--x\nxxx\n"
         ,"-xx\n--x\nxxx\n","x--\nx-x\nxxx\n","-x-\nx-x\nxxx\n","--x\nx-x\nxxx\n"]
bomb_7 = ["xxx\nx-x\nxx-\n","xxx\nx-x\nx-x\n","xxx\nx-x\n-xx\n","xxx\nx--\nxxx\n"
         ,"xxx\n--x\nxxx\n","xx-\nx-x\nxxx\n","x-x\nx-x\nxxx\n","-xx\nx-x\nxxx\n"]
bomb_8 = ["xxx\nx-x\nxxx\n"]

