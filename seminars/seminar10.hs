addTwo :: Num a => a -> a
addTwo x = x + 2

add :: Num a => a -> a -> a
add x y = x + y

addFour :: Integer -> Integer
addFour = add 4

-- първи вариант - с if
max' :: Ord t => t -> t -> t
max' a b = if a > b
  then a
  else b

-- втори вариант - с guard
-- забележете индентирането пред | - без него няма да работи
max2' :: Ord t => t -> t -> t
max2' a b
  | a > b = a
  | otherwise = b

  -- първи вариант - с if
fact :: (Eq t, Num t) => t -> t
fact n = if n == 1
  then 1
  else n * fact (n - 1)

-- втори вариант - с guard
-- забележете индентирането пред | - без него няма да работи
fact2 :: (Eq t, Num t) => t -> t
fact2 n
  | n == 1 = 1
  | otherwise = n * fact2 (n - 1)

-- трети вариант - с pattern matching
fact3 :: (Eq t, Num t) => t -> t
fact3 1 = 1
fact3 n = n * fact3 (n - 1)

-- първи вариант - с if
countDigits :: (Num t, Integral t1) => t1 -> t
countDigits n = if n < 10
  then 1
  else 1 + countDigits (div n 10)

-- втори вариант - с guard
countDigits2 :: (Num t, Integral t1) => t1 -> t
countDigits2 n
  | n < 10 = 1
  | otherwise = 1 + countDigits2 (div n 10)
  
 -- първи варинт - с външен helper
helper :: Integral t => t -> t -> t
helper i result
    | i <= 0 = result
    | otherwise = helper (i `div` 10) (10 * result + i `mod` 10)
reverseDigits :: Integral t => t -> t
reverseDigits n = helper n 0

-- втори вариант - вложена дефиниция с where
reverseDigits2 :: Integral t => t -> t
reverseDigits2 n = helper n 0 where
  helper i result
    | i <= 0 = result
    | otherwise = helper (i `div` 10) (10 * result + i `mod` 10)

-- трети вариант, чрез let ... in
reverseDigits3 :: Integral t => t -> t
reverseDigits3 n = let
  helper i result
    | i <= 0 = result
    | otherwise = helper (i `div` 10) (10 * result + i `mod` 10)
  in helper n 0
  
prime :: Integral a => a -> Bool
prime 1 = False
prime n =
  helper (n - 1)
  where
    helper 1       = True
    helper divisor = mod n divisor /= 0 && helper (divisor - 1)

-- първи вариант - чрез if
fibonacci :: (Ord a, Num a, Num t) => a -> t
fibonacci n = if n < 2
  then 1
  else fibonacci (n-1) + fibonacci (n-2)

-- втори вариант - с pattern matching
fibonacci2 :: (Eq a, Num a, Num t) => a -> t
fibonacci2 0 = 1
fibonacci2 1 = 1
fibonacci2 n = fibonacci2(n-1) + fibonacci2(n-2)

-- трети вариант - итеративно
fibonacci3 :: (Num t, Num t1, Eq t1) => t1 -> t
fibonacci3 n = fib 1 1 1 where
  fib previous current i =
    if i == n
      then current
      else fib current (previous + current) (i + 1)

fpow :: (Integral a, Floating a1) => a1 -> a -> a1
fpow x 0 = 1
fpow x n = if even n
  then fpow x (n `div` 2) ** 2
  else x * fpow x (n - 1)



