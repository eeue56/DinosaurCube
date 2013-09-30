{-# LANGUAGE ParallelListComp, TemplateHaskell, TypeOperators #-}

module FirstStage
where
	import Colours
	import Coordinates
	import Data.List
	import Control.Monad
	import Control.Category
	import Data.Label
	import Prelude hiding ((.), id)

	data Patch = Patch {
		_colour :: Colour,
		_size :: Double, 
		_coord :: Coordinate
	} deriving (Show, Eq, Ord)

	mkLabels[''Patch]
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
			spotteeShowingChance = j * (get alpha i)
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
			cToY = distanceBetween c (get coord y)
			cToZ = distanceBetween c (get coord z)

	byDistance :: Patch -> [Patch] -> [Patch]
	byDistance p xs = sortBy sorter xs
		where
			sorter = sortPatchesByDistance p

	visiblePatches :: Patch -> [Patch] -> [Patch]
	visiblePatches p xs = filter (isVisible p) $ byDistance p xs

	threateningPatches :: Patch -> [Patch] -> [Patch]
	threateningPatches p xs = filter (isThreat p) $ visiblePatches p xs

	matingPatches :: Patch -> [Patch] -> [Patch]
	matingPatches p xs = filter (isMated p) $ visiblePatches p xs

	matePatches :: Patch -> Patch -> Patch
	matePatches (Patch c1 s1 l1) (Patch c2 s2 l2) = Patch (Colour r g b a) s l1 
		where
			s = newSize s1 s2
			r = newRed c1 c2
			g = newGreen c1 c2
			b = newBlue c1 c2
			a = newAlpha c1 c2

	isNextTo :: Patch -> Patch -> Bool
	isNextTo (Patch _ _ c1) (Patch _ _ c2) = distanceBetween c1 c2 <= 2.0

	neighbours :: Patch -> [Patch] -> [Patch]
	neighbours p xs = filter (isNextTo p) xs

	dealDamage :: Patch -> Patch -> Patch
	dealDamage p1 p2 = set size (s2 - (s1 * a1)) p1
		where
			s2 = get size p2
			s1 = get size p1
			a1 = get alpha $ get colour p1

	possibleMoves :: Patch -> [Patch] -> [Coordinate]
	possibleMoves p@(Patch _ _ (Coord i j)) xs = 
		[Coord x y | 
			x <- [i - 1..i + 1], 
			y <- [j - 1..j + 1], 
			not $ any (isIn x y) n,
			x /= i || y /= j
		]
		where
			isIn i j (Patch _ _ (Coord x y)) = x == i && y == j
			n = neighbours p xs

	nearestFreeCoord :: Patch -> [Patch] -> Coordinate
	nearestFreeCoord p xs = 
		if moves /= [] then
			head moves
		else
			head $ head [x | x <- myMoves , x /= []]
		where 
			moves = possibleMoves p xs
			myNeighbours = neighbours p xs
			myMoves = head [ys | x <- neighbours p xs, let ys = [possibleMoves i xs | i <- neighbours x xs], ys /= []]



	main = do
		let testCouple = ((Patch (Colour 125 34 78 0.5) 1 (Coord 3 2)), 
			(Patch (Colour 120 38 160 0.2) 2 (Coord 2 3) )) 

		let patches = [Patch (Colour 120 30 2 0.3) 3 (Coord x y) | x <- [-5..5], y <- [-1..5]]
		
		putStrLn $ "Is second of test couple visibile to first?"
		putStrLn $ show $ isVisible (fst testCouple) (snd testCouple)

		putStrLn $ "Are the test couple a breedable pair?"
		putStrLn $ show $ isMated (fst testCouple) (snd testCouple) 

		putStrLn $ "Is the second a threat to the first?"
		putStrLn $ show $ isThreat (fst testCouple) (snd testCouple) 

		putStrLn $ "What's the distance between them?"
		putStrLn $ show $ distanceBetween (get coord $ fst testCouple) (get coord $ snd testCouple)
		
		putStrLn $ "What's the coordinates of the closest patches to the first?"
		putStrLn $ show $ [get coord x | x <- take 4 (byDistance (fst testCouple) patches)]
		
		putStrLn $ "How many patches are visible to first?"
		putStrLn $ show $ length $ visiblePatches (fst testCouple) patches
		
		putStrLn $ "How many threating patches are there to each couple member?"
		putStrLn $ show $ length $ threateningPatches (fst testCouple) patches
		putStrLn $ show $ length $ threateningPatches (snd testCouple) patches

		putStrLn $ "How many patches are mateable?"
		putStrLn $ show $ length $ matingPatches (fst testCouple) patches
		putStrLn $ show $ length $ matingPatches (snd testCouple) patches

		putStrLn $ show $ threatRate (fst testCouple) (snd testCouple)

		putStrLn $ show $ matePatches (fst testCouple) (snd testCouple)

		putStrLn $ show $ isNextTo (fst testCouple) (snd testCouple)
		putStrLn $ show $ length $ neighbours (fst testCouple) patches

		putStrLn $ show $ dealDamage (fst testCouple) (snd testCouple)

		putStrLn $ show $ [(x, y) | x <- [1..3] , y <- [2..4] ]
		putStrLn $ show $ [(x, y) | (Patch _ _ (Coord x y)) <- patches] 
		putStrLn $ show $ [(x, y) | (Coord x y) <- possibleMoves (snd testCouple) patches]

		putStrLn $ show $ nearestFreeCoord (snd testCouple) patches 