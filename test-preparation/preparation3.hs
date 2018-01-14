getColumn i m = map (\row -> row !! i) m

transpose m = map (\i -> getColumn i m) [0..((length m) - 1)]

findColumns [] = 0
findColumns m = length (filter (\row -> allElemFromRow row m)(transpose m)) where
  allElemFromRow row m
    | length m == 0 = False
    | helper row (head m) = True
	| otherwise = allElemFromRow row (tail m) where
	  helper row otherRow = (length (filter (\el -> not (el `elem` otherRow)) row) == 0)
	  
combine f g h = (\x -> h (f x) (g x))

allFGH uns bins = [combine f g h | f <- uns, g <- uns, h <- bins]

check a b uns bins = helper (allFGH uns bins) uns where
	helper fHg curUns
		| length fHg == 0 = False
		| length curUns == 0 = helper (tail fHg) uns
		| same a b (head fHg) (head curUns) = True
		| otherwise = helper fHg (tail curUns) where
		  same a b f1 f2
		    | a > b = True
			| (f1 a) /= (f2 a) = False
			| otherwise = same (a + 1) b f1 f2
			
name (x,y,z) = x
fromT (x,y,z) = y
toT (x,y,z) = z

tail' (x,y,z) = y : [z]

allTemperatures listOfPlants = [t | plant <- listOfPlants, t <- (tail' plant)]

sortedTemperatures listOfPlants = quickSort (allTemperatures listOfPlants) where
	quickSort [] = []
	quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

allIntervals listOfPlants = [((sortedTemperatures listOfPlants) !! i,(sortedTemperatures listOfPlants) !! (i + 1)) |
	i <- [0..(length (sortedTemperatures listOfPlants)) - 2]]
	
intervalsPlants listOfPlants = [(interval, plants (fst interval) (snd interval)) |
	interval <- (allIntervals listOfPlants)] where
		plants from to = [(name plant)| plant <- listOfPlants, (fromT plant) <= from, (toT plant) >= to]
		
indexOfMax listOfPlants = helper 0 0 0 where
	helper index max curIndex
		| curIndex == (length (intervalsPlants listOfPlants)) = index
		| (length (snd ((intervalsPlants listOfPlants) !! curIndex))) >= max = helper curIndex (length (snd ((intervalsPlants listOfPlants) !! curIndex))) (curIndex + 1)
		| otherwise = helper index max (curIndex + 1)
		
garden listOfPlants = ((intervalsPlants listOfPlants) !! (indexOfMax listOfPlants))
		
		
		
		
		
		
		
		