module Colours
where

	data Colour = Colour {
		red :: Int,
		green :: Int,
		blue :: Int,
		alpha :: Double
		} deriving (Show, Eq)

	range = 25

	withinRange :: Int -> Int -> Bool
	withinRange x y = x - range < y && x + range > y 

	matesRates :: Colour -> Colour -> Int
	matesRates x y = length ranged
		where
			ranged = [1 | c <- [red, green, blue], withinRange (c x) (c y)]

	newAlpha :: Colour -> Colour -> Double
	newAlpha (Colour _ _ _ a1) (Colour _ _ _ a2)
		| a1 > a2 = (a1 + a2) / 2
		| a1 < a2 = (a1 + (a2 / 2)) / 2
		| otherwise = a1