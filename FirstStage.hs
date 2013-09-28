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
		size :: Int
	} deriving (Show, Eq)

	range = 25

	withinRange :: Int -> Int -> Bool
	withinRange x y = x - range < y && x + range > y  

	isMated :: Patch -> Patch -> Bool
	isMated (Patch x _) (Patch y _) = 2 <= length (filter (== True) ranged)
		where
			ranged = [withinRange (c x) (c y) | c <- [red, green, blue]]



	main = do
		let testCouple = ((Patch (Colour 125 34 78 0.5) 3), 
			(Patch (Colour 120 38 160 0.4) 2)) 
		putStrLn $ show $ isMated (fst testCouple) (snd testCouple) 