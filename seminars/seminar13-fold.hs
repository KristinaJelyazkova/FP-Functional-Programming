import Data.List

selsort :: Ord t => [t] -> [t]
selsort [] = []
selsort l = minElement : selsort (delete minElement l) where
  minElement = minimum l

removeAll :: Eq t => t -> [t] -> [t]
removeAll x l = [ y | y <- l, y /= x ]

count :: (Num a, Eq a1) => a1 -> [a1] -> a
count x l = sum [ 1 | y <- l, y == x ]

multiUnion :: Eq a => [a] -> [a] -> [a]
multiUnion [] y = y
multiUnion (x:xs) y =
  replicate occurences x ++ multiUnion restX restY where
    occurences = max (count x xs + 1) (count x y)
    restX = removeAll x xs
    restY = removeAll x y

minimum' :: Ord a => [a] -> a
minimum' = foldr1 min

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max

concat' :: [[a]] -> [a]
concat' = foldr (++) []

sum' :: Num a => [a] -> a
sum' = foldr1 (+)

fact :: Integral a => a -> a
fact n = foldr1 (*) [1..n]

reverse' :: [a] -> [a]
reverse' = foldr (\ x result -> result ++ [x]) []

reverse2' :: [a] -> [a]
reverse2' = foldl (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x result -> if p x then x : result else result) []

filter2' :: (a -> Bool) -> [a] -> [a]
filter2' p = foldl (\result x -> if p x then result ++ [x] else result) []

remDups :: Eq a => [a] -> [a]
remDups = foldl helper [] where
  helper result x
   | x `elem` result = result
   | otherwise = result ++ [x]
   
inits :: [a] -> [[a]]
inits = foldr helper [[]] where
  helper x result = [] : map (x:) result
  
prime :: Integral a => a -> Bool
prime 1 = False
prime n =
  helper (n - 1)
  where
    helper 1       = True
    helper divisor = mod n divisor /= 0 && helper (divisor - 1)

goldbach :: Integral t => t -> [(t, t)]
goldbach n = helper initial initial where
  initial = div n 2
  helper a b
    | a == 1 = []
    | prime a && prime b = (a, b) : helper (a - 1) (b + 1)
    | otherwise = helper (a - 1) (b + 1)
	
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ (xs !! i) : x | i <- [0..length xs-1],
                                      x <- combinations (n-1) (drop (i+1) xs) ]

