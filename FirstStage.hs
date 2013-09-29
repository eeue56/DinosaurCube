module FirstStage
where

	data Colour = Colour {
		red :: Int,
		green :: Int,
		blue :: Int,
		alpha :: Double
		} deriving (Show, Eq)

	data Patch = Patch {
		colour :: Colour,
		size :: Double
	} deriving (Show, Eq)

	range = 25
	viewingDistance = 4

	withinRange :: Int -> Int -> Bool
	withinRange x y = x - range < y && x + range > y  


	matesRates :: Colour -> Colour -> Int
	matesRates x y = length ranged
		where
			ranged = [1 | c <- [red, green, blue], withinRange (c x) (c y)]

	isMated :: Patch -> Patch -> Bool
	isMated (Patch x _) (Patch y _) = 2 <= matesRates x y

	isThreat :: Patch -> Patch -> Bool
	isThreat hunter@(Patch x y) hunted@(Patch i j) = y >= (j + simRate) 
		where 
			simRate = fromIntegral $ matesRates x i

	isVisible :: Patch -> Patch -> Double -> Bool
	isVisible spotter@(Patch x y) spottee@(Patch i j) distance = finalDistance >= distance
		where
			spotterViewDistance = viewingDistance * y
			spotteeShowingChance = j * (alpha i)
			finalDistance = if isMated spotter spottee then
					spotterViewDistance + spotteeShowingChance
				else
					spotterViewDistance



	main = do
		let testCouple = ((Patch (Colour 125 34 78 0.5) 3), 
			(Patch (Colour 120 38 160 0.2) 2)) 
		putStrLn $ show $ [(x, isVisible (fst testCouple) (snd testCouple) x) | x <- [1..13]]
		putStrLn $ show $ isMated (fst testCouple) (snd testCouple) 
		putStrLn $ show $ isThreat (fst testCouple) (snd testCouple) 