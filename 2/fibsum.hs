fib :: Num a => [a]
fib = fib' 1 2
  where fib' x y = x : fib' y (x + y)

findSum :: Integral a => a
findSum = sum [x | x <- takeWhile (<4000000) fib, even x]

main :: IO ()
main = print findSum