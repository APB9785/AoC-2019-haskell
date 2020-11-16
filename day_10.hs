import Data.Function
import Data.Ord (comparing)
import Data.List (maximumBy)

main = do
    contents <- readFile "day10input.txt"

    let asteroidList = chartAsteroids $ lines contents
    
    putStr formatHeader

    let (coords, seenQty) = map (checkSights asteroidList) asteroidList
                              & maximumBy (comparing snd)
    
    print seenQty

    putStr "\nPart 2:  "

    destroy asteroidList coords

-- Loops over the input with chartSingleRow to create a list containing
-- the coordinates of each asteroid.
chartAsteroids :: [String] -> [(Int, Int)]
chartAsteroids xs = go xs 0 []
    where
      go []    _   done = done
      go (h:t) row done = go t (row+1) (chartSingleRow h row done)

-- Checks each location in the row, appending the coords of all asteroids.
chartSingleRow :: [Char] -> Int -> [(Int, Int)] -> [(Int, Int)]
chartSingleRow xs row done = go xs row 0 done
    where
      go []    _   _   done = done
      go (h:t) row col done
        | h == '.'  = go t row (col+1) done
        | h == '#'  = go t row (col+1) ((col, row):done)

-- Given a coordinate pair for the base location, checks how many other 
-- asteroids can be seen from that location. An asteroid is considered
-- blocked (unseen) if it shares a dx/dy ratio with a closer asteroid.
checkSights :: [(Int, Int)] -> (Int, Int) -> ((Int, Int), Int)
checkSights astList base = go astList base []
    where
      go []         base ratios = (base, (length ratios))
      go (ast:rest) base ratios = 
        let r = (reduceRatio $ compareCoords base ast) in
        if   ((r `elem` ratios) || (r == (0, 0)))
        then go rest base ratios
        else go rest base (r:ratios)

-- Returns a list of each asteroid destroyed from the base location in 
-- clockwise order, with blocked asteroids surviving until a subsequent
-- rotation of the beam.
destroy :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
destroy astList base =
    sortOn (findAngle base) astList

-- Calculates the angle at which the beam must fire from the base to hit
-- the given asteroid.
findAngle base ast = atan $ compareCoords
    
-- Simple dx/dy calculation.  We use the opposite of dy because the map
-- was scanned from the top down.
compareCoords :: (Int, Int) -> (Int, Int) -> (Int, Int)    
compareCoords (x1, y1) (x2, y2) = ((x2 - x1), (y1 - y2))
     
-- Reduces the dx/dy ratio to lowest terms.
reduceRatio :: (Int, Int) -> (Int, Int)
reduceRatio (x, 0)
    | x > 0  = (1, 0)
    | x < 0  = (-1, 0)
    | x == 0 = (0, 0)
reduceRatio (0, y)
    | y > 0 = (0, 1)
    | y < 0 = (0, -1)
reduceRatio (x, y) =
    let i = gcd x y in
    ((x `div` i), (y `div` i))

formatHeader = "\n***********************\nADVENT OF CODE - DAY 10" ++
    "\n***********************\n\nPart 1:  "
