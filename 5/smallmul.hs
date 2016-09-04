checkDiv :: Integral a => a -> [a] -> Bool
checkDiv n = and . foldr (\a b -> (n `mod` a == 0) : b) []

checkDiv' :: Integral a => a -> [a] -> Bool
checkDiv' _ [] = True
checkDiv' n (x:xs)
  | n `mod` x == 0 = checkDiv' n xs
  | otherwise = False

smallMul :: Integral a => [a] -> a
smallMul xs = head [x | x <- [1..], checkDiv' x xs]

smallMul' :: Integral a => [a] -> [a] -> a
smallMul' [] _ = 0
smallMul' (x:xs) ys
  | checkDiv' x ys = x
  | otherwise = smallMul' xs ys