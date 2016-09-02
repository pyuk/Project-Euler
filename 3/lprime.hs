testCase :: Integral a => a -> [a] -> Bool
testCase n [] = True
testCase n (x:xs)
  | n `mod` x == 0 = False
  | otherwise = testCase n xs

prime :: Integral a => [a]
prime = p [2..] []
  where p (x:xs) ys
          | testCase x ys = x : p xs (x:ys)
          | otherwise = p xs ys

--lPrime :: Integral a => a -> [a]
lPrime :: Integer -> [Integer]
lPrime n = [x | x <- (takeWhile (<(n `div` 2)) prime), n `mod` x == 0]