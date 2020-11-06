import Data.Function
import Data.Text (Text, pack, unpack, split, strip)
import qualified Data.IntMap.Strict as Map

main = do
    contents <- readFile "input.txt"
    --contents <- readFile "example.txt"

    let defaultMem = parseInput contents & makeIndex
      
    let partOneRes = initialize 12 2 defaultMem & runProgram 0 & runHalt
    
    let partTwoRes = checkAllInputs defaultMem

    formatRes partOneRes partTwoRes & putStr

-- Uses Data.Text for stripping and splitting the input.
parseInput :: String -> [Int]
parseInput i =
    pack i &
      strip &
      split (==',') &
      map unpack &
      map (read :: String -> Int)

-- Folds a list into map values, using list index (position) as keys
makeIndex :: [Int] -> Map.IntMap Int
makeIndex ns = foldl indexHelper Map.empty ns
    where
    indexHelper :: Map.IntMap Int -> Int -> Map.IntMap Int
    indexHelper acc val = Map.insert (Map.size acc) val acc

-- Looping case function checks the opcodes and calls the appropriate functions.
runProgram :: Int -> Map.IntMap Int -> Map.IntMap Int
runProgram idx m = case (m Map.! idx) of
    1  -> runProgram (idx + 4) (runAdd idx m)
    2  -> runProgram (idx + 4) (runMult idx m)
    99 -> m

-- Loop checks each input combination until result 19690720 is found.
checkAllInputs :: Map.IntMap Int -> Int
checkAllInputs m = go 0 0 m
    where
    go x y m
      | x > 99                  = go 0 (y+1) m
      | checkSingle == 19690720 = 100 * x + y
      | otherwise               = go (x+1) y m
      where checkSingle = initialize x y m & runProgram 0 & runHalt

-- Checks the map twice - first to find the right keys, then to find the values
-- for those keys.
runAdd :: Int -> Map.IntMap Int -> Map.IntMap Int
runAdd idx m = Map.insert key (val1 + val2) m
    where
    val1 = m Map.! (m Map.! (idx + 1))
    val2 = m Map.! (m Map.! (idx + 2))
    key  = m Map.! (idx + 3)

runMult :: Int -> Map.IntMap Int -> Map.IntMap Int
runMult idx m = Map.insert key (val1 * val2) m
    where
    val1 = m Map.! (m Map.! (idx + 1))
    val2 = m Map.! (m Map.! (idx + 2))
    key  = m Map.! (idx + 3)

-- Final function to output the result which is stored at index 0.
runHalt :: Map.IntMap Int -> Int
runHalt m = m Map.! 0

-- Sets the starting values in indexes 1 and 2.
initialize :: Int -> Int -> Map.IntMap Int -> Map.IntMap Int
initialize x y m = 
    Map.insert 1 x m &
      Map.insert 2 y

-- Creates the output display
formatRes :: Int -> Int -> String
formatRes n m = 
    "\n*************************\nADVENT OF CODE -- DAY 2\n" ++
    "*************************\n\nPart 1:  " ++ (show n) ++
    "\n\nPart 2:  " ++ (show m) ++ "\n\n"
