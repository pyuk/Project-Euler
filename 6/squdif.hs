sumOfSquares :: (Num a, Enum a) => a
sumOfSquares = sum [x^2 | x <- [1..100]]

sumOfSquares' :: (Num a, Enum a) => a
sumOfSquares' = foldr (\a b -> a^2 + b) 0 [1..100]

squareOfSum :: (Num a, Enum a) => a
squareOfSum = (^2) $ sum [1..100]

main :: IO ()
main = print $ squareOfSum - sumOfSquares