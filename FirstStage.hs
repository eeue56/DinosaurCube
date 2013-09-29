module FirstStage
where
	import Colours
	import Coordinates
	import Data.List

	data Patch = Patch {
		colour :: Colour,
		size :: Double, 
		coord :: Coordinate
	} deriving (Show, Eq, Ord)

	range = 25
	viewingDistance = 4
	
	newSize :: Double -> Double -> Double 
	newSize s1 s2 
		| s1 < s2 = s1 + (s2 / 5)
		| s1 > s2 = s2 - (s1 / 5)
		| otherwise = s1

	isMated :: Patch -> Patch -> Bool
	isMated (Patch x _ _) (Patch y _ _) = 2 <= matesRates x y

	threatRate :: Patch -> Patch -> Double
	threatRate hunted@(Patch x y _) hunter@(Patch i j _) = j - (y + simRate) 
		where 
			simRate = fromIntegral $ matesRates x i

	isThreat :: Patch -> Patch -> Bool
	isThreat h1 h2 = threatRate h1 h2 > 0.0

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

	visiblePatches :: Patch -> [Patch] -> [Patch]
	visiblePatches p xs = filter (isVisible p) $ byDistance p xs

	threateningPatches :: Patch -> [Patch] -> [Patch]
	threateningPatches p xs = filter (isThreat p) $ visiblePatches p xs

	matingPatches :: Patch -> [Patch] -> [Patch]
	matingPatches p xs = filter (isMated p) $ visiblePatches p xs

	matePatches :: Patch -> Patch -> Patch
	matePatches p1@(Patch c1 s1 l1) p2@(Patch c2 s2 l2) = Patch (Colour r g b a) s l1 
		where
			s = newSize s1 s2
			r = newRed c1 c2
			g = newGreen c1 c2
			b = newBlue c1 c2
			a = newAlpha c1 c2

	isNextTo :: Patch -> Patch -> Bool
	isNextTo (Patch _ _ c1) (Patch _ _ c2) = distanceBetween c1 c2 <= 2.0


	main = do
		let testCouple = ((Patch (Colour 125 34 78 0.5) 1 (Coord 3 2)), 
			(Patch (Colour 120 38 160 0.2) 2 (Coord 2 3) )) 

		let patches = [Patch (Colour 120 30 2 0.3) 3 (Coord x y) | x <- [1, 3..25], y <- [2, 4..26]]
		
		putStrLn $ show $ isVisible (fst testCouple) (snd testCouple)
		putStrLn $ show $ isMated (fst testCouple) (snd testCouple) 
		putStrLn $ show $ isThreat (fst testCouple) (snd testCouple) 
		putStrLn $ show $ distanceBetween (coord $ fst testCouple) (coord $ snd testCouple)
		putStrLn $ show $ [coord x | x <- take 4 (byDistance (fst testCouple) patches)]
		putStrLn $ show $ length $ visiblePatches (fst testCouple) patches
		putStrLn $ show $ length $ threateningPatches (fst testCouple) patches
		putStrLn $ show $ length $ threateningPatches (snd testCouple) patches
		putStrLn $ show $ length $ matingPatches (fst testCouple) patches
		putStrLn $ show $ length $ matingPatches (snd testCouple) patches

		putStrLn $ show $ threatRate (fst testCouple) (snd testCouple)

		putStrLn $ show $ matePatches (fst testCouple) (snd testCouple)

		putStrLn $ show $ isNextTo (fst testCouple) (snd testCouple)