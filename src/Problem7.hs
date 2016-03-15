module Problem7 where

import Test.HUnit
import Common

-- 10001st prime
problem7 = nthPrime 10001

nthPrime n = last $ take n $ filter (isPrime) [1..]

test7 = TestCase (assertEqual "problem #7" problem7 104743)
