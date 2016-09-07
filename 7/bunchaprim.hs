primes :: Integral a => [a]
primes = p [2..]
  where p (x:xs) = x : p [n | n <- xs, n `mod` x /= 0]

main = print $ primes !! 10001