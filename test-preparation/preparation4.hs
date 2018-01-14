allDifferent f g a b = [a - 1] ++ [x | x <- [a..b], (f x) /= (g x)] ++ [b + 1]

allIntervals f g a b = [( ((allDifferent f g a b) !! i) + 1,((allDifferent f g a b) !! (i + 1)) - 1 ) |
	i <- [0..((length (allDifferent f g a b)) - 2)], (((allDifferent f g a b) !! (i + 1)) - ((allDifferent f g a b) !! i)) > 1]
	
maxIntervalIndex f g a b = helper 0 0 0 where
	helper maxIndex max curIndex
		| curIndex >= length (allIntervals f g a b) = maxIndex
		| (diff curIndex) > max = 
			helper curIndex (diff curIndex) (curIndex + 1)
		| otherwise = helper maxIndex max (curIndex + 1) where
			diff curIndex = (snd ((allIntervals f g a b) !! curIndex) - fst ((allIntervals f g a b) !! curIndex))

largestInterval f g a b = (allIntervals f g a b) !! (maxIntervalIndex f g a b)

sum' n = [1 | x <- [1..(n - 1)], y <- [1..x], x * x + y * y == n]

sumOfSquares = filter (\n -> length (sum' n) > 0) [2..]

averageLength listOfVideos = (sum [snd video | video <- listOfVideos]) `div` (length listOfVideos)

changeList listOfVideos = filter (\video -> (snd video) <= (averageLength listOfVideos)) listOfVideos

findClosestIndex listOfVideos = helper 0 ((averageLength listOfVideos) - snd (head listOfVideos)) 0 where
	helper closestIndex closestDifference curIndex
		| closestDifference == 0 = closestIndex
		| curIndex >= length listOfVideos = closestIndex
		| (curDiff curIndex) < closestDifference = helper curIndex (curDiff curIndex) (curIndex + 1) 
		| otherwise = helper closestIndex closestDifference (curIndex + 1) where
			curDiff curIndex = ((averageLength listOfVideos) - (snd (listOfVideos !! curIndex)))
			
averageVideo listOfVideos = fst ((changeList listOfVideos) !! (findClosestIndex (changeList listOfVideos)))