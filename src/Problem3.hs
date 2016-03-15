module Problem3 where

import Test.HUnit
import Data.List
import Common

-- Problem 3: Largest prime factor
problem3 = maximum $ primeFactors 600851475143

primeFactors x = filter (isPrime) $ factors x

factors x = let lows = filter (divisible x) [1..sqrtInt x]
                highs = reverse $ map (div x) lows
            in nub $ lows ++ highs

test3 = TestCase (assertEqual "problem #3" problem3 6857)
