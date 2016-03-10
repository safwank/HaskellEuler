module Problem4 where

-- Problem 4: Largest palindrome product
problem4 = largestPalindromeProduct 100 999

largestPalindromeProduct min max = head $ filter (isPalindrome) $ map (product) $ nub $ map (take 2) $ permutations $ takeWhile (>=min) [max,max-1..]

isPalindrome x = let y = digits x
                 in y == reverse y

digits n = map (\x -> read [x] :: Int) (show n)