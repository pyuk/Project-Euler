primes = p [2..]
  where p (x:xs) = x : [n | n <- xs, n `mod` x == 0]

primeSum = sum $ [x | x <- primes, x < 2000000]
