module Common where

divisible x y = x `mod` y == 0

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not $ any (divisible x) [2..sqrtInt x]

sqrtInt x = truncate . sqrt $ fromIntegral x
