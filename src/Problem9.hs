module Problem9 where

import Test.HUnit
import Control.Monad

-- Special Pythagorean triplet
problem9 = product $ pythagoreanTriplet 1000 1

pythagoreanTriplet total a = case triplets of
                                  [] -> pythagoreanTriplet total (a+1)
                                  _  -> head triplets
                             where triplets = filter (isPythagoreanTriplet total)
                                               $ map (a:)
                                               $ filter (\xs -> (head xs) < (last xs))
                                               $ replicateM 2 [a+1..500]

isPythagoreanTriplet total xs = sum xs == total && pythagorean xs

pythagorean (a:b:c:[]) = (c**2 == a**2 + b**2)
pythagorean _ = False

test9 = TestCase (assertEqual "problem #9" problem9 31875000)
