module Problem5 where

import Test.HUnit

-- Smallest multiple
problem5 = smallestEvenlyDivisible [1..20]

smallestEvenlyDivisible xs = smallestEvenlyDivisible' (last xs) xs

smallestEvenlyDivisible' x xs = case allDivisible x xs of
                                True  -> x
                                False -> smallestEvenlyDivisible' (x + (last xs)) xs

allDivisible x xs = all (divisible x) xs

divisible x y = x `mod` y == 0

test5 = TestCase (assertEqual "problem #5" problem5 232792560)
