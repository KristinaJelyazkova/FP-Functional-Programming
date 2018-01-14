import Data.List

ln x = helper 1 0 0.01 where
	helper index result epsilon
		| (abs current) < epsilon = result + current
		| otherwise = helper (index + 1) (result + current) epsilon where
			current = (fromIntegral ((-1) ^ (index + 1))) * (x ^ index) / (fromIntegral index)
			
isOrdered list = helper 0 where
	helper index 
		| index >= (length list) - 1 = True
		| curX >= nextX = False
		| otherwise = helper (index + 1) where
			curX = fst (list !! index)
			nextX = fst (list !! (index + 1))
			
deletePoints listOfPoints = helper 1 listOfPoints where
	helper index currentList
		| (isOrdered currentList) = currentList
		| index >= (length currentList) = currentList
		| curX <= previousX = helper index (delete (currentList !! index) currentList) 
		| otherwise = helper (index + 1) currentList where
			curX = fst (currentList !! index)
			previousX = fst (currentList !! (index - 1))
			
findFace listOfPoints = helper 0 0 where
	helper index result
		| index >= length listOfPoints - 1 = result
		| otherwise = helper (index + 1) (result + curFace) where
			curFace = (curY + nextY) * (nextX - curX) / 2 where
				curX = fst (listOfPoints !! index)
				curY = snd (listOfPoints !! index)
				nextX = fst (listOfPoints !! (index + 1))
				nextY = snd (listOfPoints !! (index + 1))
				
findFace' listOfPoints = findFace (deletePoints listOfPoints)

mergeOrderedStreams stream1 stream2
	| length stream1 == 0 = stream2
	| length stream2 == 0 = stream1
	| (head stream1) < (head stream2) = (head stream1) : (mergeOrderedStreams (tail stream1) stream2)
	| otherwise = (head stream2) : (mergeOrderedStreams stream1 (tail stream2))