module Problem2 where

import Test.HUnit
import qualified Data.MemoCombinators as Memo

-- Problem 2: Even Fibonacci numbers
problem2 = sum $ filter (even) $ takeWhile (<4000000) $ fibList [1..]

fib = Memo.integral fib'
      where
      fib' 0 = 0
      fib' 1 = 1
      fib' x = fib (x-1) + fib (x-2)

fibList = map fib

test2 = TestCase (assertEqual "problem #2" problem2 4613732)