{-# LANGUAGE BangPatterns #-}

module Data.GameBoard.Random where

import Data.GameBoard

import System.Random (randomRIO,Random(..),RandomGen,mkStdGen)
import qualified Data.Map as M
import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first)

instance Random GameBoard where
  randomR (GameBoard _ lr lc , GameBoard _ hr hc) gen =
      let (r,gen1) = randomR (lr,hr) gen
          (c,gen2) = randomR (lc,hc) gen1
          (n,gen3) = randomR (1, hr*hc) gen2
          (lst,gen4)  = replicateRR n ((lr,hr),(lc,hc)) (Bomb,Bomb) gen3
      in (GameBoard {get = lst, row = r, col = c},gen4)

  random gen =
      let (r,gen1) = first abs $ random gen
          (c,gen2) = first abs $ random gen1
          (n,gen3) = randomR (1, r*c) gen2
          (lst,gen4)  = replicateR n gen3
      in (GameBoard {get = lst, row = r, col = c}, gen4)

instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)
  random gen1 =
    let (x, gen2) = random gen1
        (y, gen3) = random gen2
    in ((x, y), gen3)

instance Random Bomb where
  random gen = (Bomb,gen)
  randomR _ gen = (Bomb,gen)

replicateRR :: (Ord k, Random k, Random v, RandomGen g)
      => Int -> (k,k) -> (v,v) -> g -> (M.Map k v,g)
replicateRR = replicateRR' M.empty
  where --replicateR' :: (Ord k) => M.Map k v -> Int -> m k -> m v -> m (M.Map k v)
        replicateRR' !acc 0 kk vv gen = (acc, gen)
        replicateRR' !acc !n kk vv gen =
             let (k,gen1) = randomR kk gen
                 (v,gen2) = randomR vv gen1
             in if k `M.member` acc
                  then replicateRR' acc n kk vv gen2
                  else replicateRR' (M.insert k v acc) (n-1) kk vv gen2

replicateR :: (Ord k, Random k, Random v, RandomGen g) => Int -> g -> (M.Map k v,g)
replicateR = replicateR' M.empty
  where --replicateR' :: (Ord k) => M.Map k v -> Int -> m k -> m v -> m (M.Map k v)
        replicateR' !acc 0 gen = (acc, gen)
        replicateR' !acc !n gen =
             let (k,gen1) = random gen
                 (v,gen2) = random gen1
             in if k `M.member` acc
                  then replicateR' acc n gen2
                  else replicateR' (M.insert k v acc) (n-1) gen2


genBoard :: Int -> GameBoard
genBoard n = let lo = GameBoard {row = 1 , col=  1, get = M.empty}
                 hi = GameBoard {row = 15, col= 20, get = M.empty}
             in fst $ randomR (lo,hi) (mkStdGen n)

