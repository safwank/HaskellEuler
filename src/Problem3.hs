module Problem3 where

import Test.HUnit
import Data.List

-- Problem 3: Largest prime factor
problem3 = maximum $ primeFactors 600851475143

primeFactors x = filter (isPrime) $ factors x

factors x = let maxLow = truncate . sqrt $ fromIntegral x
                lows = filter (divisible x) [1..maxLow]
                highs = reverse $ map (div x) lows
            in nub $ lows ++ highs

divisible x y = x `mod` y == 0

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not $ any (divisible x) [2..x-1]

test3 = TestCase (assertEqual "problem #3" problem3 6857)