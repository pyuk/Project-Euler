multi :: Integer -> Integer -> Integer
multi x y = sum [z | z <- [1..999], z `mod` x == 0, z `mod` y == 0]

main :: IO ()
main = print $ multi 3 5