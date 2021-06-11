module Main where

import Data.List


main :: IO ()
main = do
  let result = test
  mapM_ (\x -> putStrLn $ show x) result
  putStrLn $ "epsilon = " ++ show epsilon
  let numResults = length result
  putStrLn $ "number of pairs = " ++ show numResults
  let manyNines = filter (\(_,_,sum) -> abs (toFractional (round sum)) > abs sum) result
  let numManyNines = length manyNines
  putStrLn $ "number of pairs whose sum is just below = " ++ show (length manyNines)
  putStrLn $ "percentage = " ++ show ((fromIntegral numManyNines)/(fromIntegral numResults) * 100.0)


toFractional :: Integer -> Double
toFractional = fromIntegral

-- search numbers less than maxNum
maxNum = 1000

-- Find sums this close to an integer
epsilon = 0.00001

nonIntegerSqrt :: Integer -> Bool
nonIntegerSqrt n = toFractional (round s) /= s
  where
    s = sqrt (toFractional n)

-- Tests whether sqrt n + sqrt m (where n and m aren't
-- perfect squares) is close to an integer
testPair n m =
  (nonIntegerSqrt n && nonIntegerSqrt m && d < epsilon && abs (m - n) /= 2 && n /= m,
  (n, m, d)  )
  where
    d = abs (v - toFractional (round v))
    v = sqrtSum n m - 0.25

testList = test' 1 1
  where
    test' n m | n < maxNum = testPair n m : test' (n+1) m
    test' n m | m < maxNum = testPair n m : test' 1 (m + 1)
    test' _ _ | otherwise = []

test = map getPair $ sortBy compareD $ filter fst testList
  where
    compareD a b = compare (getD a) (getD b)
    getD (_, (_, _, d)) = d
    getPair (_, (x, y,_)) = (x,y, sqrtSum x y)

sqrtSum x y = sqrt (toFractional x) + sqrt (toFractional y)