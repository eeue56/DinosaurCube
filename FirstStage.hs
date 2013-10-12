{-# LANGUAGE ParallelListComp, TemplateHaskell, TypeOperators, BangPatterns, DataKinds #-}

module FirstStage
where
	import Colours
	import Coordinates
	import Data.List
	import Control.Monad
	import Control.Category
	import Data.Label
	import Prelude hiding ((.), id)

	import Data.Maybe

	data Patch = Patch {
		_colour :: Colour,
		_size :: Double, 
		_coord :: Coordinate
	} deriving (Show, Eq, Ord)

	data Move = StayStill | MakeAMate Patch 
				| Attack Patch | Move Coordinate
				deriving (Show, Eq, Ord)

	mkLabels[''Patch]
	range = 25
	viewingDistance = 4
	
	-- Get the new size from two previous sizes
	newSize :: Double -> Double -> Double 
	newSize s1 s2 
		| s1 < s2 = s1 + (s2 / 5)
		| s1 > s2 = s2 - (s1 / 5)
		| otherwise = s1

	-- Returns true if patches are similiar enough to reproduce
	isMated :: Patch -> Patch -> Bool
	isMated (Patch x _ _) (Patch y _ _) = 2 <= matesRates x y

	-- Returns the threat rate from the second argument to the first argument
	threatRate :: Patch -> Patch -> Double
	threatRate hunted@(Patch x y _) hunter@(Patch i j _) = j - (y + simRate) 
		where 
			simRate = fromIntegral $ matesRates x i

	-- Returns true if there is a positive thread from h2 to h1
	isThreat :: Patch -> Patch -> Bool
	isThreat h1 h2 = threatRate h1 h2 > 0.0

	-- Returns if the second patch is visible to the first patch
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

	-- Returns the ordering of two patches
	sortPatchesByDistance :: Patch -> Patch -> Patch -> Ordering
	sortPatchesByDistance (Patch _ _ c) y z
		| cToY > cToZ = GT
		| cToY < cToZ = LT 
		| otherwise = EQ
		where 
			cToY = distanceBetween c (get coord y)
			cToZ = distanceBetween c (get coord z)

	-- Returns the patch list sorted by distance, nearest first
	byDistance :: Patch -> [Patch] -> [Patch]
	byDistance p xs = sortBy sorter xs
		where
			sorter = sortPatchesByDistance p

	-- Returns a list of patches visible to the patch
	visiblePatches :: Patch -> [Patch] -> [Patch]
	visiblePatches p xs = filter (isVisible p) $ byDistance p xs

	-- Returns a list of patches which are a threat to the patch
	-- filters on visible patches
	threateningPatches :: Patch -> [Patch] -> [Patch]
	threateningPatches p xs = filter (isThreat p) $ visiblePatches p xs

	-- Returns a list of patches suitable for mating
	-- filters on visible
	matingPatches :: Patch -> [Patch] -> [Patch]
	matingPatches p xs = filter (isMated p) $ visiblePatches p xs

	-- Take two patches, mate them, put it on the nearest free coord and return
	matePatches :: Patch -> Patch -> [Patch] -> Patch
	matePatches p@(Patch c1 s1 l1) (Patch c2 s2 l2) xs = Patch (Colour r g b a) s (nearestFreeCoord p xs)
		where
			s = newSize s1 s2
			r = newRed c1 c2
			g = newGreen c1 c2
			b = newBlue c1 c2
			a = newAlpha c1 c2

	-- Returns true if the patches are next to each other
	isNextTo :: Patch -> Patch -> Bool
	isNextTo (Patch _ _ c1) (Patch _ _ c2) = distanceBetween c1 c2 <= 2.0

	-- Returns the neighbours of the patch
	neighbours :: Patch -> [Patch] -> [Patch]
	neighbours p xs = filter (isNextTo p) xs

	-- Deal damage to p1 from p2
	dealDamage :: Patch -> Patch -> Patch
	dealDamage p1 p2 = set size (s2 - (s1 * a1)) p1
		where
			s2 = get size p2
			s1 = get size p1
			a1 = get alpha $ get colour p1

	-- List the possible moves as a list of coordinates
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

	-- Returns the nearest free coord
	nearestFreeCoord :: Patch -> [Patch] -> Coordinate
	nearestFreeCoord p xs = head $ concat $ map (\x -> possibleMoves x xs) $ byDistance p xs

	-- Returns the next move made by the patch
	nextMove :: Patch -> [Patch] -> Move
	nextMove p xs = threatend threats
		where
			moves = possibleMoves p xs
			n = neighbours p xs
			nextMates = filter (isMated p) n 
			nextToAttack = filter (\x -> isThreat x p) n
			threats = filter (isThreat p) n

			threatend [] = mating nextMates 
			threatend xs = escape moves

			escape [] = threatend []
			escape xs = Move $ head moves

			mating [] = attack nextToAttack
			mating xs = MakeAMate $ head nextMates

			attack [] = StayStill
			attack xs = Attack $ head nextToAttack

	-- Generate the move queue for all patches in patch list
	moveQueue :: [Patch] -> [(Patch, Move)]
	moveQueue xs = [(x, nextMove x xs) | x <- xs]

	-- Apply all queued changes to a singular patch
	-- Applied in the order of the list
	unionPatchChanges :: Patch -> [(Patch, Move)] -> Patch
	unionPatchChanges !p ![] = p
	unionPatchChanges !p !((other, move):xs) = case move of 
			Attack _ -> unionPatchChanges (dealDamage p other) xs
			Move l -> unionPatchChanges (set coord l p) xs
			_ -> unionPatchChanges p xs

	-- Returns true if the patch passed in is the one as part of the move
	isMe :: (Patch, Move) -> Patch -> Bool
	isMe (x, y) t = case y of
		Attack other -> other == t 
		Move _ -> x == t
		_ -> False

	-- returns all moves that involve patch
	relatedMoves :: Patch -> [(Patch, Move)] -> [(Patch, Move)]
	relatedMoves p !xs = filter (\x -> isMe x p) xs

	-- apply all the moves and return the new patch list
	doMoves :: [Patch] -> [Patch]
	doMoves xs = filter (\x -> (get size x) > 0) $ concat [helper x y | (x, y) <- queue] 
		where
			helper x y = case y of
				StayStill -> if attacked x then [] else [x]
				Move l -> if attacked x then [] else [set coord l x]
				Attack other -> [x, (unionPatchChanges other $ myMoves other)]
				MakeAMate other -> if attacked x then 
										[matePatches x other xs]
									else
										[x, matePatches x other xs]
			myMoves x = relatedMoves x queue
			attacked y = 1 < (length $ myMoves y)  
			queue = moveQueue xs

	-- Do a number of moves at once
	doXGenerations :: Int -> [Patch] -> [Patch]
	doXGenerations 0 xs = xs
	doXGenerations n xs = doXGenerations (n - 1) $ doMoves xs

	isInRange :: Coordinate -> Coordinate -> Patch -> Bool
	isInRange (Coord x y) (Coord a b) (Patch _ _ (Coord i j)) = (x <= i && i <= a) && (y <= j && j <= b)

	doRestrictedXGenerations :: Int -> Coordinate -> Coordinate -> [Patch] -> [Patch]
	doRestrictedXGenerations 0 c1 c2 xs = filter (isInRange c1 c2) xs
	doRestrictedXGenerations n c1 c2 xs = doRestrictedXGenerations n c1 c2 $ filter (isInRange c1 c2) xs
	


	main = do
		let testCouple = ((Patch (Colour 125 34 78 0.5) 1 (Coord 3 2)), 
			(Patch (Colour 120 38 160 0.2) 2 (Coord 2 3) )) 



		let patches = doMoves $ [Patch (Colour 215 159 198 0.5) 8 (Coord 3 1),
						 Patch (Colour 121 45 85 0.5) 10 (Coord 2 1),
						 Patch (Colour 71 193 67 0.5) 2 (Coord 8 1),
						 Patch (Colour 178 210 182 0.5) 8 (Coord 8 1),
						 Patch (Colour 105 82 219 0.5) 3 (Coord 7 1),
						 Patch (Colour 52 60 208 0.5) 3 (Coord 0 1),
						 Patch (Colour 215 175 102 0.5) 6 (Coord 4 1),
						 Patch (Colour 36 48 4 0.5) 2 (Coord 5 1),
						 Patch (Colour 27 24 156 0.5) 9 (Coord 7 1),
						 Patch (Colour 132 173 237 0.5) 7 (Coord 9 1),
						 Patch (Colour 161 64 19 0.5) 3 (Coord 8 1), 
						 Patch (Colour 60 125 51 0.5) 2 (Coord 8 0)]
	
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

		putStrLn $ show $ matePatches (fst testCouple) (snd testCouple) patches

		putStrLn $ show $ isNextTo (fst testCouple) (snd testCouple)
		putStrLn $ show $ length $ neighbours (fst testCouple) patches

		putStrLn $ show $ dealDamage (fst testCouple) (snd testCouple)

		putStrLn $ show $ [(x, y) | x <- [1..3] , y <- [2..4] ]
		putStrLn $ show $ [(x, y) | (Patch _ _ (Coord x y)) <- patches] 
		putStrLn $ show $ [(x, y) | (Coord x y) <- possibleMoves (snd testCouple) patches]

		putStrLn $ show $ nearestFreeCoord (snd testCouple) patches 


		putStrLn $ show $ nextMove (fst testCouple) patches

		putStrLn $ show $ length $ patches
		putStrLn $ show $ length $ doXGenerations 8 patches
		putStrLn $ show $ length $ doMoves $ doMoves $ doMoves $ doMoves $ doMoves $ patches
		putStrLn $ show $ length $ patches

		putStrLn $ show $ isMe (fst testCouple, Attack $ snd testCouple) $ snd testCouple 
