module Problem6 where

import Test.HUnit

-- Sum square difference
problem6 = let list = [1..100]
           in (squareOfSum list) - (sumOfSquares list)

sumOfSquares xs = sum $ map (square) xs

squareOfSum xs = square $ sum xs

square x = x * x

test6 = TestCase (assertEqual "problem #6" problem6 25164150)
