module Euler where

import Data.List
import Test.HUnit

--Problem 1
problem1 = let threes = takeWhile (<1000) [3,6..]
               fives  = takeWhile (<1000) [5,10..]
           in sum $ nub $ threes ++ fives

test1 = TestCase (assertEqual "problem #1" problem1 233168)

--Problem 2
problem2 = sum $ filter (even) (takeWhile (<4000000) (fibList [1..]))

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
fibList = map fib

test2 = TestCase (assertEqual "problem #2" problem2 4613732)