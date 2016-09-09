theTrip :: Integer
theTrip = head [a*b*c | c <- [100..], a <- [100..c], b <- [a..c], a^2 + b^2 == c^2, a+b+c == 1000]