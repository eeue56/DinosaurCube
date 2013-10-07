{-# LANGUAGE ParallelListComp, TemplateHaskell, TypeOperators #-}

module Colours
where
	import Control.Monad
	import Control.Category
	import Data.Label
	import Prelude hiding ((.), id)

	data Colour = Colour {
		_red :: Int,
		_green :: Int,
		_blue :: Int,
		_alpha :: Double
		} deriving (Show, Eq, Ord)

	mkLabels[''Colour]

	range = 25

	-- find if the number is between the two numbers
	withinRange :: Int -> Int -> Bool
	withinRange x y = x - range < y && x + range > y 

	-- generate the rate at which two colours are friends
	matesRates :: Colour -> Colour -> Int
	matesRates x y = length ranged
		where
			ranged = [1 | c <- [red, green, blue], withinRange (get c x) (get c y)]

        -- generate a new alpha based on the input colours
	newAlpha :: Colour -> Colour -> Double
	newAlpha (Colour _ _ _ a1) (Colour _ _ _ a2)
		| a1 > a2 = (a1 + a2) / 2
		| a1 < a2 = (a1 + (a2 / 2)) / 2
		| otherwise = a1

	-- generate a new red based on the input coloours
	newRed :: Colour -> Colour -> Int
	newRed (Colour r1 _ _ _) (Colour r2 _ _ _)
		| r1 > r2 = div (r1 + r2) 2
		| r1 < r2 = div (r1 + (div r2 2)) 2
		| otherwise = r1

	-- generate a new green based on the input colours
	newGreen :: Colour -> Colour -> Int
	newGreen (Colour _ g1 _ _) (Colour _ g2 _ _)
		| g1 > g2 = div (g1 + g2) 2
		| g1 < g2 = div (g1 + (div g2 2)) 2
		| otherwise = g1

	-- generate a new blue based on the input colours
	newBlue :: Colour -> Colour -> Int
	newBlue (Colour _ _ b1 _) (Colour _ _ b2 _)
		| b1 > b2 = div (b1 + b2) 2
		| b1 < b2 = div (b1 + (div b2 2)) 2
		| otherwise = b1
