module Main where

import Data.List
import Debug.Trace


--ctx :: MathContext
--ctx = (HALF_UP, Just 0)

places = 10

bias = 0.0

--sqrtPrec :: MathContext
--sqrtPrec = (HALF_UP, Just places)




main :: IO ()
main = do
  let result = sequenceUpToM 10000
  mapM_ (\x -> putStrLn $ show x) $ result


rnd :: Double -> Double
rnd a = fromIntegral (round a)

toFractional :: Integer -> Double
toFractional n = fromInteger n

-- search numbers less than maxNum
maxNumX = 250
maxNumY = 250

-- Find sums this close to an integer or better
epsilon :: Double
epsilon = 0.001

nonIntegerSqrt :: Integer -> Bool
nonIntegerSqrt n = rnd s   /= s
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

    d = distance v
    v = sqrtN + sqrtM
    notPerfect = d > 0.0000000001

-- distance to integer
distance v = abs (v - rnd v + bias)

distance' (a, b) = distance (mySqrt a + mySqrt b)

notPerfect n = mySqrt n /= rnd (mySqrt n)

notBoring m n = notPerfect m && notPerfect n

testForN best n = (a, b)
  where
    (a, b) = go best 1 n
    go best@(a, b) m n
      | m <= n =
        let e = distance' (m, n)
            best' = if e < distance' best && notBoring m n then (m, n) else best
        in  go best' (m+1) n
      | otherwise = best

sequenceUpToM m = map withSum $ go first 2
  where
    withSum (a,b) = ((a,b), mySqrt a + mySqrt b)
    first = (1,2)
    go best n | n <= m =
                  let best' = testForN best n
                  in if best' /= best then best : go best' (n+1) else go best (n+1)
              | otherwise = [best]



testList = test' 1 1
  where
    test' n m | n < maxNumX = testPair n m : test' (n+1) m
    test' n m | m < maxNumY = trace (show m ++ "/" ++ show maxNumY) $ testPair n m : test' 1 (m + 1)
    test' _ _ | otherwise = []

test = map getPair $ sortBy compareD $ filter fst testList
  where
    compareD a b = compare (getD a) (getD b)
    getD (_, (_, _, _, d)) = d
    getPair (_, (x, y, v, _)) = (x,y, show v, v)

mySqrt n = sqrt (toFractional n) 




