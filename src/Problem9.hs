module Problem9 where

import Test.HUnit
import Control.Monad

-- Special Pythagorean triplet
problem9 = product $ pythagoreanTriplet 1000 1

pythagoreanTriplet total a = case triplets of
                                  [] -> pythagoreanTriplet total (a+1)
                                  _  -> head triplets
                             where triplets = filter (isPythagoreanTriplet total)
                                              $ map (\b -> a : b : [calculateC a b])
                                              $ [a+1..total]

isPythagoreanTriplet total xs = sum xs == total && pythagorean xs

pythagorean (a:b:c:[]) = (c**2 == a**2 + b**2)
pythagorean _ = False

calculateC a b = sqrt $ a**2 + b**2

test9 = TestCase (assertEqual "problem #9" problem9 31875000)
