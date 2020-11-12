import Data.Function
import qualified Data.IntMap.Strict as Map

main = do
    contents <- readFile "day9input.txt"

    putStr formatHeader

    let initialState = makeIndex $ splitCSV contents
        
    runProgram 1 initialState
      & head
      & print

    putStr "\nPart 2:  "

    runProgram 2 initialState
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

-- Folds a list into map values, using list index (position) as keys.
-- Now includes an additional 500 empty registers beyond the input list.
makeIndex :: [Int] -> Map.IntMap Int
makeIndex ns = foldl indexHelper Map.empty (ns ++ (take 500 (repeat 0)))
    where
    indexHelper acc val = Map.insert (Map.size acc) val acc

-- Looping case function checks the opcodes and calls the appropriate functions.
runProgram :: Int -> Map.IntMap Int -> [Int]
runProgram inputVal m = go 0 m [] 0
    where
    go idx mem output offset =
      let (opcode, mode1, mode2, mode3) = parseInstruction (mem Map.! idx) in 
      case opcode of
        -- Addition: read and write to the memory
        1  -> go (idx + 4)
                 (runAdd idx mem mode1 mode2 mode3 offset)
                 output
                 offset
        -- Multiplication: read and write to the memory
        2  -> go (idx + 4)
                 (runMult idx mem mode1 mode2 mode3 offset)
                 output
                 offset
        -- Input: write to the memory based on input value
        3  -> go (idx + 2)
                 (runInput idx mem mode1 inputVal offset)
                 output
                 offset
        -- Output: read the memory and hold a value for later output
        4  -> go (idx + 2)
                 mem
                 ((runOut idx mem mode1 offset):output)
                 offset
        -- Jump-if-true: read the memory and adjust the pointer
        5  -> go (runJumpIf True idx mem mode1 mode2 offset)
                 mem
                 output
                 offset
        -- Jump-if-false: read the memory and adjust the pointer
        6  -> go (runJumpIf False idx mem mode1 mode2 offset)
                 mem
                 output
                 offset
        -- Less than: read and write to the memory
        7  -> go (idx + 4)
                 (runLessThan idx mem mode1 mode2 mode3 offset)
                 output
                 offset
        -- Equals: read and write to the memory
        8  -> go (idx + 4)
                 (runEquals idx mem mode1 mode2 mode3 offset)
                 output
                 offset
        -- Relative base offset: adjust the offset by a given value
        9  -> go (idx + 2)
                 mem
                 output
                 (runAdjustOffset idx mem mode1 offset)
        -- Halt: ends the program and returns held values for printing
        99 -> output

parseInstruction :: Int -> (Int, Int, Int, Int)
parseInstruction n =
    let (a:b:c:de) = padZeroes $ show n in
    ((read de :: Int), (read [c] :: Int), (read [b] :: Int), (read [a] :: Int))

runAdd :: Int -> Map.IntMap Int -> Int -> Int -> Int -> Int -> Map.IntMap Int
runAdd idx mem mode1 mode2 mode3 offset =
    Map.insert key (val1 + val2) mem
    where
      val1 = lookupWithMode mode1 mem (idx + 1) offset
      val2 = lookupWithMode mode2 mem (idx + 2) offset
      key  = if   mode3 == 2
             then offset + (mem Map.! (idx + 3))
             else mem Map.! (idx + 3)

runMult :: Int -> Map.IntMap Int -> Int -> Int -> Int -> Int -> Map.IntMap Int
runMult idx mem mode1 mode2 mode3 offset =
    Map.insert key (val1 * val2) mem
    where
      val1 = lookupWithMode mode1 mem (idx + 1) offset
      val2 = lookupWithMode mode2 mem (idx + 2) offset
      key  = if   mode3 == 2
             then offset + (mem Map.! (idx + 3))
             else mem Map.! (idx + 3)

runInput :: Int -> Map.IntMap Int -> Int -> Int -> Int -> Map.IntMap Int
runInput idx mem mode inputVal offset =
    Map.insert key inputVal mem
    where
      key = if   mode == 2
            then offset + (mem Map.! (idx + 1))
            else mem Map.! (idx + 1)

runOut :: Int -> Map.IntMap Int -> Int -> Int -> Int
runOut idx mem mode offset = lookupWithMode mode mem (idx + 1) offset

runJumpIf :: Bool -> Int -> Map.IntMap Int -> Int -> Int -> Int -> Int
runJumpIf truth idx mem mode1 mode2 offset =
    if ((val1 /= 0) == truth)
    then val2
    else idx + 3
    where
      val1 = lookupWithMode mode1 mem (idx + 1) offset
      val2 = lookupWithMode mode2 mem (idx + 2) offset

runLessThan :: Int -> Map.IntMap Int -> Int -> Int -> Int -> Int -> Map.IntMap Int
runLessThan idx mem mode1 mode2 mode3 offset =
    Map.insert key (if val1 < val2 then 1 else 0) mem
    where
      val1 = lookupWithMode mode1 mem (idx + 1) offset
      val2 = lookupWithMode mode2 mem (idx + 2) offset
      key  = if   mode3 == 2
             then offset + (mem Map.! (idx + 3))
             else mem Map.! (idx + 3)

runEquals :: Int -> Map.IntMap Int -> Int -> Int -> Int -> Int -> Map.IntMap Int
runEquals idx mem mode1 mode2 mode3 offset =
    Map.insert key (if val1 == val2 then 1 else 0) mem
    where
      val1 = lookupWithMode mode1 mem (idx + 1) offset
      val2 = lookupWithMode mode2 mem (idx + 2) offset
      key  = if   mode3 == 2
             then offset + (mem Map.! (idx + 3))
             else mem Map.! (idx + 3)

runAdjustOffset :: Int -> Map.IntMap Int -> Int -> Int -> Int
runAdjustOffset idx mem mode offset =
    offset + (lookupWithMode mode mem (idx + 1) offset)

lookupWithMode :: Int -> Map.IntMap Int -> Int -> Int -> Int
lookupWithMode mode mem idx offset =
    case mode of
      0 -> mem Map.! (mem Map.! (idx))
      1 -> mem Map.! idx
      2 -> mem Map.! (offset + (mem Map.! idx))

formatHeader = "\n**********************\nADVENT OF CODE - DAY 9\n" ++
    "**********************\n\nPart 1:  "

padZeroes :: String -> String
padZeroes n = replicate (5 - length n) '0' ++ n
