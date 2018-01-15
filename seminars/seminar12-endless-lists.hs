fst' :: (t1, t) -> t1
fst' (x,xs) = x

snd' :: (t, t1) -> t1
snd' (x,xs) = xs

cartesian :: [t1] -> [t] -> [(t1, t)]
cartesian l1 l2 = [(i, j) | i <- l1, j <- l2]

matrixElements :: [[t]] -> [t]
matrixElements m =
    [m!!i!!j | i<-[0..length m - 1],
               j<-[0..length(head m) - 1],
               even(i + j)]

removeAll :: Eq t => t -> [t] -> [t]
removeAll x l = [ y | y <- l, y /= x ]

count :: (Num a, Eq a1) => a1 -> [a1] -> a
count x l = sum [ 1 | y <- l, y == x ]

histogram :: (Num t, Eq t1) => [t1] -> [(t1, t)]
histogram []     = []
histogram (x:xs) = (x, count x xs + 1) : histogram (removeAll x xs)

-- Реализация на quicksort чрез list comprehension.
qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort (pivot:rest) = lessThan ++ [pivot] ++ moreThan where
  lessThan = qsort [x | x <- rest, x <= pivot]
  moreThan = qsort [x | x <- rest, x > pivot]

-- Това е еквивалентно решение с filter:
qsort2 :: Ord t => [t] -> [t]
qsort2 [] = []
qsort2 (x:xs) = lessThan ++ [x] ++ moreThan where
  lessThan = qsort2(filter (<=x) xs)
  moreThan = qsort2(filter (>x) xs)
  
naturals :: [Integer]
naturals = helper 1 where
  helper start = start : helper (start + 1)

tribonacci :: [Integer]
tribonacci = [1, 1] ++ helper 1 1 1 where
  helper current prev1 prev2 = current : helper (current + prev1 + prev2) current prev1
  
sieve :: (Integral a) => [a]
sieve = helper [2..] where
  helper (x:xs) = x : helper (filter (\el -> el `mod` x > 0) xs)



