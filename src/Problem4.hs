module Problem4 where

import Data.List
import Control.Monad
import Test.HUnit
import Common

-- Problem 4: Largest palindrome product
problem4 = largestPalindromeProduct 100 999

largestPalindromeProduct min max = maximum
                                   $ filter (isPalindrome)
                                   $ map (product)
                                   $ replicateM 2
                                   $ [max,max-1..min]

isPalindrome x = let y = digits x
                 in y == reverse y

test4 = TestCase (assertEqual "problem #4" problem4 906609)
