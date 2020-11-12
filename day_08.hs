import Data.Function
import Data.Ord (comparing)
import Data.List (minimumBy)
import qualified Data.List as List
import qualified Data.IntMap.Strict as Map

main = do
    contents <- readFile "day8input.txt"

    -- Each layer is 25 x 6 (given in puzzle instructions).
    -- init is used to strip trailing newline in puzzle input.
    let layers = splitLayersBySize 150 (init contents)

    putStr formatHeader

    print $ checksum $ layers

    putStr "\nPart 2:  (printed below)\n"

    putStr $ renderImage layers

    putStr "\n\n"


-- Flattens the layers into a single IntMap, then calls 'display'
-- to convert into a single String with newlines.
renderImage :: [String] -> String
renderImage xs =
    let parsed = List.foldl' parseLayer Map.empty xs in
    display parsed

-- Inserts visible pixels into accumulator IntMap.
parseLayer :: Map.IntMap Char -> [Char] -> Map.IntMap Char
parseLayer m layer = go m layer 0
    where
      go done [] _ = done
      go done (x:xs) idx
        | x == '0' = if   idx `Map.member` done
                     then go done xs (idx+1)
                     else go (Map.insert idx ' ' done) xs (idx+1)
        | x == '1' = if   idx `Map.member` done
                     then go done xs (idx+1)
                     else go (Map.insert idx '#' done) xs (idx+1)
        | x == '2' = go done xs (idx+1)

-- Converts the final IntMap to a String by looping over every key 
-- in numerical order. Adds newlines every 25 characters.
display :: Map.IntMap Char -> String
display m = go m 0 []
    where
      go m idx done
        | idx == 150        = reverse done
        | idx `mod` 25 == 0 = go m (idx+1) ((m Map.! idx):'\n':done)
        | otherwise         = go m (idx+1) ((m Map.! idx):done)

splitLayersBySize :: Int -> String -> [String]
splitLayersBySize n [] = []
splitLayersBySize n xs =
    let (h, t) = splitAt n xs in
    h:(splitLayersBySize n t)

-- Checksum formula given in puzzle input.
checksum :: [String] -> Int
checksum s = 
    let x = minimumBy (comparing (countDigit '0')) s in
    (countDigit '1' x) * (countDigit '2' x)

countDigit :: Char -> String -> Int
countDigit d s = length $ filter (== d) s

formatHeader = "\n**********************\nADVENT OF CODE - DAY 8" ++
    "\n**********************\n\nPart 1:  "
