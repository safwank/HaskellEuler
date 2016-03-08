module Euler where

import Data.List
import Test.HUnit

-- Problem 1: Multiples of 3 and 5
problem1 = let threes = takeWhile (<1000) [3,6..]
               fives  = takeWhile (<1000) [5,10..]
           in sum $ nub $ threes ++ fives

test1 = TestCase (assertEqual "problem #1" problem1 233168)

-- Problem 2: Even Fibonacci numbers
problem2 = sum $ filter (even) $ takeWhile (<4000000) $ fibList [1..]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
fibList = map fib

test2 = TestCase (assertEqual "problem #2" problem2 4613732)

-- Problem 3: Largest prime factor
problem3 = primeFactors 600851475143

primeFactors 0 = []
primeFactors 1 = []
primeFactors n = filter (isPrime) $ factors n

factors n = filter (divisible n) [1..n]

divisible a b = a `mod` b == 0

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = not $ any (divisible n) [2..(n `quot` 2)]

test3 = TestCase (assertEqual "problem #3" problem3 6857)