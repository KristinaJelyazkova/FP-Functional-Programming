-- Примерите тук показват как можем да имплементираме наши версии на head и tail
-- чрез destruct-ване на списъка, подаден като аргумент. Това е форма на pattern
-- matching-а, познат ни от предишните упражнения.
-- Идеята е, че главата на списъка е достъпна през x, а опашката - през xs. По
-- този начин, имплементацията на head и tail всъщност става един от
-- най-тривиалните примери за destruct-ване.

head' :: [t] -> t
head' (x:xs) = x

tail' :: [t] -> [t]
tail' (x:xs) = xs

-- Това е наш вариант на вградената функция elem, която проверява дали даден
-- елемент се съдържа в списък.
elem' :: Eq t => t -> [t] -> Bool
elem' x []     = False
elem' x (y:ys) = x == y || elem' x ys

{- невалидно: elem' x (x:xs) = True -}

-- Наша версия на вградената функция sum.
sum' :: Num t => [t] -> t
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- Брои срещанията на елемент в списък.
countOccurences :: (Num res, Eq t1) => t1 -> [t1] -> res
countOccurences x []     = 0
countOccurences x (y:ys) =
  (if x == y then 1 else 0) + countOccurences x ys

-- по-нататък ще го постигаме с foldr:
countOccurences2 :: (Num res, Eq t1) => t1 -> [t1] -> res
countOccurences2 x l = foldr (\ y -> (+) (if x == y then 1 else 0)) 0 l

-- ... или с map + sum
countOccurences3 :: (Num res, Eq t1) => t1 -> [t1] -> res
countOccurences3 x l = sum $ map (\ y -> if x == y then 1 else 0) l

-- ...или с list comprehension
countOccurences4 :: (Num res, Eq t1) => t1 -> [t1] -> res
countOccurences4 x l     = sum [ 1 | y <- l, y == x ]

-- Наша версия на вградената функция reverse.
reverse' :: [t] -> [t]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Наша версия на вградената функция flip, която „обръща“ аргументите на дадена
-- двуаргументна функция. За целта се създава lambda функция, която замества
-- оригиналната.
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- Втори варинт на горното, с currying. Продължава да се ползва с аргумент, като
-- той (f) се фиксира, а останалите (x и y) се взимат по-късно.
flip2' :: (a -> b -> c) -> b -> a -> c
flip2' f x y = f y x

evens l = filter even l

-- currying:
evens2 = filter even

square l = map (\x -> x ^ 2) l

-- Можем да го решим и с инфиксен currying на ^. Езикът ни позволява да подадем
-- инфиксна функция само с един (вместо с два) аргумента и да се приложи
-- currying като се фиксира вторият аргумент. Това се различава от обичайния
-- currying, където аргументите се фиксират първо по-предните аргументи.
square2 l = map (^ 2) l

-- и с нормален currying:
square3 = map (^ 2)

-- Филтриране на елементи в матрица.
filterMatrix :: (a -> Bool) -> [[a]] -> [[a]]
filterMatrix p m = map (\row -> filter p row) m

-- Втори вариант - с currying в lambda-та.
filterMatrix2 :: (a -> Bool) -> [[a]] -> [[a]]
filterMatrix2 p m = map (filter p) m

-- Трети вариант - с currying и за параметъра m. Ако даден параметър е
-- най-вдясно в сигнатурата на функцията и се използва най-вдясно в извикването
-- в тялото на тази функция, то можем да го пропуснем. Така реално правим
-- currying за функцията map.
filterMatrix3 :: (a -> Bool) -> [[a]] -> [[a]]
filterMatrix3 p = map (filter p)

-- n-та колона в матрица.
getColumn :: [[b]] -> Int -> [b]
getColumn m n = map (\row -> row !! n) m

-- Друг вариант, този път с инфиксен currying
getColumn2 :: [[b]] -> Int -> [b]
getColumn2 m n = map (!! n) m

diagonal :: [[b]] -> [b]
diagonal m = map (\idx -> (m !! idx) !! idx) [0..(length m - 1)]

secondDiagonal :: [[b]] -> [b]
secondDiagonal m = map (\idx -> (m !! idx) !! (length m - idx - 1)) [0..(length m - 1)]

transpose :: [[a]] -> [[a]]
transpose m = map (\n -> getColumn m n) [0..(length(head m) - 1)]

-- lambda-та може да се трансформира с currying:
transpose2 :: [[a]] -> [[a]]
transpose2 m = map (getColumn m) [0..(length(head m) - 1)]

-- „Завърта“ елементите на списък наляво. Отрицателни числа задават „завъртане“
-- надясно.
-- Имаме отделна проверка за случаите с отрицателен или много голям n. Второто
-- е с цел да спестим твърде много изпълнения - по този начин винаги функцията
-- се върти най-много length l - 1 пъти. Всъщност, това условие може дори да се
-- избегне, чрез изчисляване на mod при извикването на rotate по-долу.
rotate :: [a] -> Int -> [a]
rotate l 0 = l
rotate l n
 | n < 0 || n > length l = rotate l (mod n (length l))
 | otherwise = rotate (tail l ++ [head l]) (n - 1)