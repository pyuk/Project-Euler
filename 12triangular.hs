triangular :: [Integer]
triangular = tri 1
  where tri x = (sum . take x) [1..] : tri (x + 1)

divisors :: Integer -> [Integer]
divisors n = n : [x | x <- [1 .. (n `div` 2)], n `mod` x == 0]

findNum :: [[Integer]] -> Integer
findNum [] = 0
findNum (x:xs)
  | length x >= 500 = head x
  | otherwise = findNum xs

main :: IO ()
main = print $ (findNum . map divisors) triangular
