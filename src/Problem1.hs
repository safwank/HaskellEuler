module Problem1 where

import Test.HUnit
import Data.List

-- Problem 1: Multiples of 3 and 5
problem1 = let threes = takeWhile (<1000) [3,6..]
               fives  = takeWhile (<1000) [5,10..]
           in sum $ nub $ threes ++ fives

test1 = TestCase (assertEqual "problem #1" problem1 233168)