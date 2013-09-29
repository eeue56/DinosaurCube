module Coordinates
where

	data Coordinate = Coord Int Int deriving (Show, Eq, Ord)

	distanceBetween :: Coordinate -> Coordinate -> Double
	distanceBetween (Coord x y) (Coord i j) = xs + ys
		where 
			xs = sqrt $ fromIntegral $ (x - i)^2
			ys = sqrt $ fromIntegral $ (y - j)^2  

	averageCoord :: [Coordinate] -> Coordinate
	averageCoord xs = Coord (div x size) (div y size)
		where
			x = sum [xcor | (Coord xcor _) <- xs]
			y = sum [ycor | (Coord _ ycor) <- xs]
			size = length xs