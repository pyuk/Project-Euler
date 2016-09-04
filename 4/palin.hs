isPali :: Integral a => a -> Bool
isPali x
  | pal x == reverse (pal x) = True
  | otherwise = False
  where pal 0 = []
        pal a = a `mod` 10 : pal (a `div` 10)

palis :: Integral a => [a]
palis = [x*y | x <- [100..999], y <- [x..999], isPali (x*y)]

main :: IO ()
main = print $ maximum palis