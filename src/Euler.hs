module Euler where

import Data.List
import Test.HUnit
import qualified Data.MemoCombinators as Memo

-- Problem 1: Multiples of 3 and 5
problem1 = let threes = takeWhile (<1000) [3,6..]
               fives  = takeWhile (<1000) [5,10..]
           in sum $ nub $ threes ++ fives

test1 = TestCase (assertEqual "problem #1" problem1 233168)

-- Problem 2: Even Fibonacci numbers
problem2 = sum $ filter (even) $ takeWhile (<4000000) $ fibList [1..]

fib = Memo.integral fib'
      where
      fib' 0 = 0
      fib' 1 = 1
      fib' x = fib (x-1) + fib (x-2)

fibList = map fib

test2 = TestCase (assertEqual "problem #2" problem2 4613732)

-- Problem 3: Largest prime factor
problem3 = maximum $ primeFactors 600851475143

primeFactors 0 = []
primeFactors 1 = []
primeFactors x = filter (isPrime) $ factors x

factors x = nub $ lows ++ (reverse $ map (div x) lows)
            where lows = filter (divisible x) [1..truncate . sqrt $ fromIntegral x]

divisible x y = x `mod` y == 0

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not $ any (divisible x) [2..(x-1)]

test3 = TestCase (assertEqual "problem #3" problem3 6857)