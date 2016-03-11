module Problem4 where

import Test.HUnit
import Data.List

-- Problem 4: Largest palindrome product
problem4 = largestPalindromeProduct 100 999

largestPalindromeProduct min max = maximum
                                   $ filter (isPalindrome)
                                   $ map (product)
                                   $ permutate 2
                                   $ [max,max-1..min]

isPalindrome x = let y = digits x
                 in y == reverse y

digits n = map (\x -> read [x] :: Int) (show n)

permutate _ [] = [[]]
permutate 0 _ = [[]]
permutate len xs = [x:y | x <- xs, y <- permutate (len-1) xs]

test4 = TestCase (assertEqual "problem #4" problem4 906609)
