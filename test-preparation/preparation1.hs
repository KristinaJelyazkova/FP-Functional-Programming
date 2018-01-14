duplicateElements [] = []
duplicateElements (x:xs) = x : x : (duplicateElements xs)

slice _ _ [] = []
slice from to list = (take (to - from + 1) (drop from list))

primeSum n = head [(x,y) | x <- [2..(n-2)], y <- [2..(n-2)], prime x, prime y, y > x, x + y == n]
	where
		prime number = helper (number - 1) number
			where
				helper div n
					| div == 1 = True
					| n `mod` div == 0 = False
					| otherwise = helper (div - 1) n