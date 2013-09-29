module FirstStage
where

	import Data.List

	data Colour = Colour {
		red :: Int,
		green :: Int,
		blue :: Int,
		alpha :: Double
		} deriving (Show, Eq)

	data Patch = Patch {
		colour :: Colour,
		size :: Double, 
		coord :: Coordinate
	} deriving (Show, Eq)

	data Coordinate = Coord Int Int deriving (Show, Eq, Ord)

	range = 25
	viewingDistance = 4

	withinRange :: Int -> Int -> Bool
	withinRange x y = x - range < y && x + range > y 

	distanceBetween :: Coordinate -> Coordinate -> Double
	distanceBetween (Coord x y) (Coord i j) = xs + ys
		where 
			xs = sqrt $ fromIntegral $ (x - i)^2
			ys = sqrt $ fromIntegral $ (y - j)^2  

	matesRates :: Colour -> Colour -> Int
	matesRates x y = length ranged
		where
			ranged = [1 | c <- [red, green, blue], withinRange (c x) (c y)]

	isMated :: Patch -> Patch -> Bool
	isMated (Patch x _ _) (Patch y _ _) = 2 <= matesRates x y

	isThreat :: Patch -> Patch -> Bool
	isThreat hunter@(Patch x y _) hunted@(Patch i j _) = y >= (j + simRate) 
		where 
			simRate = fromIntegral $ matesRates x i

	isVisible :: Patch -> Patch -> Bool
	isVisible spotter@(Patch x y z) spottee@(Patch i j k) = finalDistance >= distance
		where
			distance = distanceBetween z k
			spotterViewDistance = viewingDistance * y
			spotteeShowingChance = j * (alpha i)
			finalDistance = if isMated spotter spottee then
					spotterViewDistance + spotteeShowingChance
				else
					spotterViewDistance

	sortPatchesByDistance :: Patch -> Patch -> Patch -> Ordering
	sortPatchesByDistance (Patch _ _ c) y z
		| cToY > cToZ = GT
		| cToY < cToZ = LT 
		| otherwise = EQ
		where 
			cToY = distanceBetween c (coord y)
			cToZ = distanceBetween c (coord z)

	byDistance :: Patch -> [Patch] -> [Patch]
	byDistance p@(Patch _ _ c) xs = sortBy sorter xs
		where
			sorter = sortPatchesByDistance p

	main = do
		let testCouple = ((Patch (Colour 125 34 78 0.5) 3 (Coord 3 2)), 
			(Patch (Colour 120 38 160 0.2) 2 (Coord 2 3) )) 

		let patches = [Patch (Colour 125 125 125 0.5) 4 (Coord x y) | x <- [1..5], y <- [1..5]]
		putStrLn $ show $ isVisible (fst testCouple) (snd testCouple)
		putStrLn $ show $ isMated (fst testCouple) (snd testCouple) 
		putStrLn $ show $ isThreat (fst testCouple) (snd testCouple) 
		putStrLn $ show $ distanceBetween (coord $ fst testCouple) (coord $ snd testCouple)

		putStrLn $ show $ [coord x | x <- patches]
		putStrLn $ show $ [coord x | x <- take 4 (byDistance (fst testCouple) patches)]