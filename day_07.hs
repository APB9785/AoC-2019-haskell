import Data.Function
import qualified Data.IntMap.Strict as Map
import qualified Data.List as List

main = do
    contents <- readFile "day7input.txt"

    putStr formatHeader

    let ampSoftware = makeIndex $ splitCSV contents
        
    let states = take 5 (repeat (0, ampSoftware))

    -- Run the software on each permutation, only saving the output values (fst).
    map (fst . (runSeries states 0)) (List.permutations [0,1,2,3,4])
      & List.maximum
      & print

    putStr "\nPart 2:  "

    map (runSeriesLoop states) (List.permutations [5,6,7,8,9])
      & List.maximum
      & print

    putStr "\n"


-- Makes one loop from amp A to E.  Returns the output and also the state of each amp.
runSeries :: [(Int, Map.IntMap Int)] -> Int -> [Int] -> (Int, [(Int, Map.IntMap Int)])
runSeries states startVal phases =
    let [(a_idx, a_mem), (b_idx, b_mem), (c_idx, c_mem), (d_idx, d_mem), (e_idx, e_mem)] = states

        a_input = startVal `insertSecond` phases
        ((a_idx', a_mem', input'), a_out) = runProgram a_input a_idx a_mem

        b_input = a_out `insertSecond` input'
        ((b_idx', b_mem', input2), b_out) = runProgram b_input b_idx b_mem

        c_input = b_out `insertSecond` input2
        ((c_idx', c_mem', input3), c_out) = runProgram c_input c_idx c_mem

        d_input = c_out `insertSecond` input3
        ((d_idx', d_mem', input4), d_out) = runProgram d_input d_idx d_mem

        e_input = d_out `insertSecond` input4
        ((e_idx', e_mem', _     ), e_out) = runProgram e_input e_idx e_mem
        in
    (e_out, [(a_idx', a_mem'), (b_idx', b_mem'), (c_idx', c_mem'), (d_idx', d_mem'), (e_idx', e_mem')])

-- Loops over runSeries until -1 is passed, then returns last good output from amp E.
runSeriesLoop :: [(Int, Map.IntMap Int)] -> [Int] -> Int
runSeriesLoop initStates phases =
    let (e_out, stateList) = runSeries initStates 0 phases in
    go stateList e_out [e_out]
    where
      go states (-1)     done = head $ tail done
      go states inputVal done =
        let (inputVal', states') = runSeries states inputVal [] in
        go states' inputVal' (inputVal':done)
        
-- If list is empty, inserts at index 0, otherwise index 1.
insertSecond n [] = [n]
insertSecond n xs =
    let (h:rest) = xs in
    h:n:rest

-- Splits on comma delimiter, converts all values to Int.
splitCSV :: [Char] -> [Int]
splitCSV [] = []
splitCSV x  =
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
-- Stops running once -1 is passed - future calls do nothing.
runProgram :: [Int] -> Int -> Map.IntMap Int -> ((Int, Map.IntMap Int, [Int]), Int)
runProgram [-1]      startIdx m = ((startIdx, m, []), -1)
runProgram inputList startIdx m = go startIdx m inputList
    where
    go idx mem input =
      let (opcode, mode1, mode2, _) = parseInstruction (mem Map.! idx) in 
      case opcode of
        -- Addition: read and write to the memory
        1  -> go (idx + 4)
                 (runAdd idx mem mode1 mode2)
                 input
        -- Multiplication: read and write to the memory
        2  -> go (idx + 4)
                 (runMult idx mem mode1 mode2)
                 input
        -- Input: write to the memory based on next input value then pop value
        3  -> let val:rest = input in
              go (idx + 2)
                 (runInput idx mem val)
                 rest
        -- Output: read the memory, halt, return (state, read value)
        4  -> (((idx + 2), mem, input), (runOut idx mem mode1))
        -- Jump-if-true: read the memory and adjust the pointer
        5  -> go (runJumpIf True idx mem mode1 mode2)
                 mem
                 input
        -- Jump-if-false: read the memory and adjust the pointer
        6  -> go (runJumpIf False idx mem mode1 mode2)
                 mem
                 input
        -- Less than: read and write to the memory
        7  -> go (idx + 4)
                 (runLessThan idx mem mode1 mode2)
                 input
        -- Equals: read and write to the memory
        8  -> go (idx + 4)
                 (runEquals idx mem mode1 mode2)
                 input
        -- Halt: signals the end of the program with -1
        99 -> ((idx, mem, input), -1)

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
