import Data.Function
import qualified Data.IntMap.Strict as Map

main = do
    contents <- readFile "day5input.txt"

    putStr formatHeader

    let initialState = makeIndex $ splitCSV contents
        
    runProgram 1 initialState
      & head
      & print

    putStr "\nPart 2:  "

    runProgram 5 initialState
      & head
      & print

    putStr "\n"


-- Splits on comma delimiter, converts all values to Int.
splitCSV :: [Char] -> [Int]
splitCSV [] = []
splitCSV x =
    let (curr, rest) = break (==',') x in
    case rest of
      [] -> (read curr :: Int):(splitCSV rest)
      _  -> (read curr :: Int):(splitCSV $ tail rest)

-- Folds a list into map values, using list index (position) as keys
makeIndex :: [Int] -> Map.IntMap Int
makeIndex ns = foldl indexHelper Map.empty ns
    where
    indexHelper acc val = Map.insert (Map.size acc) val acc

-- Looping case function checks the opcodes and calls the appropriate functions.
runProgram :: Int -> Map.IntMap Int -> [Int]
runProgram inputVal m = go 0 m []
    where
    go idx mem output =
      let (opcode, mode1, mode2, _) = parseInstruction (mem Map.! idx) in 
      case opcode of
        -- Addition: read and write to the memory
        1  -> go (idx + 4)
                 (runAdd idx mem mode1 mode2)
                 output
        -- Multiplication: read and write to the memory
        2  -> go (idx + 4)
                 (runMult idx mem mode1 mode2)
                 output
        -- Input: write to the memory based on input value
        3  -> go (idx + 2)
                 (runInput idx mem inputVal)
                 output
        -- Output: read the memory and hold a value for later output
        4  -> go (idx + 2)
                 mem
                 ((runOut idx mem mode1):output)
        -- Jump-if-true: read the memory and adjust the pointer
        5  -> go (runJumpIf True idx mem mode1 mode2)
                 mem
                 output
        -- Jump-if-false: read the memory and adjust the pointer
        6  -> go (runJumpIf False idx mem mode1 mode2)
                 mem
                 output
        -- Less than: read and write to the memory
        7  -> go (idx + 4)
                 (runLessThan idx mem mode1 mode2)
                 output
        -- Equals: read and write to the memory
        8  -> go (idx + 4)
                 (runEquals idx mem mode1 mode2)
                 output
        -- Halt: ends the program and returns held values for printing
        99 -> output

parseInstruction :: Int -> (Int, Int, Int, Int)
parseInstruction n =
    let (a:b:c:de) = padZeroes $ show n in
    ((read de :: Int), (read [c] :: Int), (read [b] :: Int), (read [a] :: Int))

runAdd :: Int -> Map.IntMap Int -> Int -> Int -> Map.IntMap Int
runAdd idx mem mode1 mode2 = Map.insert key (val1 + val2) mem
    where
    val1 = lookupWithMode mode1 mem (idx + 1)
    val2 = lookupWithMode mode2 mem (idx + 2)
    key  = mem Map.! (idx + 3)

runMult :: Int -> Map.IntMap Int -> Int -> Int -> Map.IntMap Int
runMult idx mem mode1 mode2 = Map.insert key (val1 * val2) mem
    where
    val1 = lookupWithMode mode1 mem (idx + 1)
    val2 = lookupWithMode mode2 mem (idx + 2)
    key  = mem Map.! (idx + 3)

runInput :: Int -> Map.IntMap Int -> Int -> Map.IntMap Int
runInput idx mem inputVal = Map.insert key inputVal mem
    where
    key = mem Map.! (idx + 1)

runOut :: Int -> Map.IntMap Int -> Int -> Int
runOut idx mem mode = lookupWithMode mode mem (idx + 1)

runJumpIf :: Bool -> Int -> Map.IntMap Int -> Int -> Int -> Int
runJumpIf truth idx mem mode1 mode2 =
    if ((val1 /= 0) == truth)
    then val2
    else idx + 3
    where
      val1 = lookupWithMode mode1 mem (idx + 1)
      val2 = lookupWithMode mode2 mem (idx + 2)

runLessThan :: Int -> Map.IntMap Int -> Int -> Int -> Map.IntMap Int
runLessThan idx mem mode1 mode2 =
    Map.insert key (if val1 < val2 then 1 else 0) mem
    where
      val1 = lookupWithMode mode1 mem (idx + 1)
      val2 = lookupWithMode mode2 mem (idx + 2)
      key  = mem Map.! (idx + 3)

runEquals :: Int -> Map.IntMap Int -> Int -> Int -> Map.IntMap Int
runEquals idx mem mode1 mode2 =
    Map.insert key (if val1 == val2 then 1 else 0) mem
    where
      val1 = lookupWithMode mode1 mem (idx + 1)
      val2 = lookupWithMode mode2 mem (idx + 2)
      key  = mem Map.! (idx + 3)

lookupWithMode :: Int -> Map.IntMap Int -> Int -> Int
lookupWithMode mode mem idx =
    if   mode == 0
    then mem Map.! (mem Map.! (idx))
    else mem Map.! idx

formatHeader = "\n**********************\nADVENT OF CODE - DAY 5\n" ++
    "**********************\n\nPart 1:  "

padZeroes :: String -> String
padZeroes n = replicate (5 - length n) '0' ++ n
