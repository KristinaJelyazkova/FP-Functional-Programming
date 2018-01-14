import Data.List

hailstone 1 = [1]
hailstone n 
	| even n = n : (hailstone (n `div` 2))
	| otherwise = n : (hailstone (3 * n + 1))
	
prime number = helper (number - 1) number
	where
		helper div n
			| div == 1 = True
			| n `mod` div == 0 = False
			| otherwise = helper (div - 1) n

primeAll = [x | x <- [2..99], prime x]
	
sum' y 
	| length [1 | z <- [1..7], p <- primeAll, p + 2 * z * z == y] == 0 = True
	| otherwise = False
	
amount = [ x | x <- [10..500], not (prime x), odd x, sum' x]

divisors n = [(x,(countDiv x n)) | x <- [2..n], prime x, n `mod` x == 0] where
	countDiv x n 
		| (n `mod` x /= 0) = 0
		| otherwise = 1 + countDiv x (n `div` x)
		
intercalate' str list = foldl1 (++)
							(map (\i -> if i == (length list - 1)
									then list !! i
									else (list !! i) ++ str)
								[0..(length list - 1)])
								
allWords listOfWords = helper [] listOfWords [listOfWords]
	where
		helper result list currentList
			| length list == 0 = result
			| length currentList == 0 = helper result (tail list) [((head (tail list)) : (delete (head (tail list)) listOfWords))]
			| otherwise = helper ((map head currentList) ++ result) list 
					[(((head x) ++ (tail y)) : (delete y (tail x))) | x <- currentList, y <- tail x, last (head x) == (head y)]
					
findLongestWord list = maximum (allWords list)
		
		
		
		
		
		
		
		
		
		
