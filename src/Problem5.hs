module Problem5 where

import Test.HUnit
import Common

-- Smallest multiple
problem5 = smallestEvenlyDivisible [1..20]

smallestEvenlyDivisible xs = smallestEvenlyDivisible' (last xs) xs

smallestEvenlyDivisible' x xs = case all (divisible x) xs of
                                True  -> x
                                False -> smallestEvenlyDivisible' (x + (last xs)) xs

test5 = TestCase (assertEqual "problem #5" problem5 232792560)
