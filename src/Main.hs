module Main where

import Data.List
import Data.BigDecimal
import Data.BigFloating
import Debug.Trace


ctx :: MathContext
ctx = (HALF_UP, Just 0)

places = 10

sqrtPrec :: MathContext
sqrtPrec = (HALF_UP, Just places)

main :: IO ()
main = do
  let result = test
  mapM_ (\x -> putStrLn $ show x) $ map (\(x,y,z,_) -> (x,y,z)) $ result
  putStrLn $ "epsilon = " ++ show epsilon
  let numResults = length result
  putStrLn $ "number of pairs = " ++ show numResults
  let manyNines = filter (\(_,_,_,sum) -> abs (roundBD sum ctx) > abs sum) result
  let numManyNines = length manyNines
  putStrLn $ "number of pairs whose sum is just below = " ++ show (length manyNines)
  putStrLn $ "percentage = " ++ show ((fromIntegral numManyNines)/(fromIntegral numResults) * 100.0)

toFractional :: Integer -> BigDecimal
toFractional n = fromInteger n

-- search numbers less than maxNum
maxNumX = 500
maxNumY = 500

-- Find sums this close to an integer
epsilon :: BigDecimal
epsilon = 0.0001

nonIntegerSqrt :: Integer -> Bool
nonIntegerSqrt n = roundBD s ctx  /= s
  where
    s = mySqrt n

-- Tests whether sqrt n + sqrt m (where n and m aren't
-- perfect squares) is close to an integer
testPair n m =
  (abs (m - n) /= 2 && n /= m && d < epsilon && notPerfect ,
  (n, m, v, d)  )
  where
    sqrtN = mySqrt n
    sqrtM = mySqrt m

    d = abs (v - roundBD v ctx)
    v = sqrtN + sqrtM
    notPerfect = d > BigDecimal 1 places

testList = test' 1 1
  where
    test' n m | n < maxNumX = testPair n m : test' (n+1) m
    test' n m | m < maxNumY = trace (show m ++ "/" ++ show maxNumY) $ testPair n m : test' 1 (m + 1)
    test' _ _ | otherwise = []

test = map getPair $ sortBy compareD $ filter fst testList
  where
    compareD a b = compare (getD a) (getD b)
    getD (_, (_, _, _, d)) = d
    getPair (_, (x, y, v, _)) = (x,y, toString v, v)

mySqrt n = sqr (toFractional n) sqrtPrec

