module Main where

import Data.List


main :: IO ()
main = putStrLn $ show test

-- search numbers less than maxNum
maxNum = 10000

-- Find sums this close to an integer
epsilon = 0.000001

nonIntegerSqrt :: Integer -> Bool
nonIntegerSqrt n = fromIntegral (floor s) /= s
  where
    s = sqrt (fromIntegral n)

-- Tests whether sqrt n + sqrt m (where n and m aren't
-- perfect squares) is close to an integer
testPair n m =
  (nonIntegerSqrt n && nonIntegerSqrt m &&
    d  < epsilon, (n, m, d))
  where
    d = abs (v - fromIntegral (round v))
    v = sqrt (fromIntegral n) + sqrt (fromIntegral m)

testList = test' 1 1
  where
    test' n m | n < maxNum = testPair n m : test' (n+1) m
    test' n m | m < maxNum = testPair n m : test' 1 (m + 1)
    test' _ _ | otherwise = []

test = map getPair $ sortBy compareD $ filter fst testList
  where
    compareD a b = compare (getD a) (getD b)
    getD (_, (_, _, d)) = d
    getPair (_, (x, y,_)) = (x,y)

