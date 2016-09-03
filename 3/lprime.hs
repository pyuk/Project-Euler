import Data.List ((\\))

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

prime' :: Integral a => [a]
prime' = p [2..]
  where p (x:xs) = x : p [x | x <- (xs `minus` [2*x,3*x..])]

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] _ = []
minus (x:xs) (y:ys)
  | x < y = x : minus xs (y:ys)
  | x > y = minus (x:xs) ys
  | otherwise = minus xs ys

lPrime :: Integer -> [Integer]
lPrime n = [x | x <- (takeWhile (<(n `div` 2)) prime'), n `mod` x == 0]