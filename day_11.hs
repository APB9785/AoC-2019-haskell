import Data.Function
import qualified Data.IntMap.Strict as Map
import qualified Data.HashMap.Strict as Hash
import qualified Data.List as List

main = do
    contents <- readFile "day11input.txt"

    putStr formatHeader

    let initialState = makeIndex $ splitCSV contents
        
    runPaint initialState Hash.empty
      & Hash.size
      & print

    putStr "\nPart 2:  (displayed below)\n\n"

    runPaint initialState (Hash.singleton (0, 0) 1)
      & printHull
      & putStr

    putStr "\n"


-- Loop through iterations of the Intcode program by calling singlePaint.
-- The states of the Intcode program and robot are preserved between iterations.
runPaint :: Map.IntMap Int -> Hash.HashMap (Int, Int) Int -> Hash.HashMap (Int, Int) Int
runPaint startMem startHull = go startHull (0, 0, "N") (0, startMem, 0)
    where
      -- End condition: robot direction changed to "X" by singlePaint
      go hullMap (_,      _,      "X"     ) (_,   _,   _     ) = hullMap

      go hullMap (robotX, robotY, robotDir) (idx, mem, offset)

        | (robotX, robotY) `Hash.member` hullMap =
           
            let color = hullMap Hash.! (robotX, robotY)

                (hull', x', y', dir', idx', mem', offset') = 
                  singlePaint color hullMap (robotX, robotY, robotDir) (idx, mem, offset)


            in go hull' (x', y', dir') (idx', mem', offset')

        | otherwise =

            let (hull', x', y', dir', idx', mem', offset') =
                  singlePaint 0 hullMap (robotX, robotY, robotDir) (idx, mem, offset)

            in go hull' (x', y', dir') (idx', mem', offset')


-- Runs the Intcode program until two outputs are received.
-- Uses both output values to return new state info for the next iteration.
singlePaint
    :: Int
    -> Hash.HashMap (Int, Int) Int
    -> (Int, Int, String)
    -> (Int, Map.IntMap Int, Int)
    -> (Hash.HashMap (Int, Int) Int, Int, Int, String, Int, Map.IntMap Int, Int)
singlePaint color hull (robotX, robotY, robotDir) (idx, mem, offset) =

    let ((idx', mem', offset'), [a, b]) = runProgram color idx mem offset
        toPaint = if (a == 0) then 0 else 1
        newHull = Hash.insert (robotX, robotY) toPaint hull
        (x', y') = stepForward robotX robotY dir'
        dir' = if (b == 0) then turnLeft Hash.! robotDir else turnRight Hash.! robotDir

    in if   a == 99
       then (hull, robotX, robotY, "X", idx, mem, offset)
       else (newHull, x', y', dir', idx', mem', offset')


stepForward :: Int -> Int -> String -> (Int, Int)
stepForward x y d =
    case d of
      "N" -> (x,   y+1)
      "E" -> (x+1, y  )
      "S" -> (x,   y-1)
      "W" -> (x-1, y  )

printHull :: Hash.HashMap (Int, Int) Int -> String
printHull h = go 0 1 []
    where
      go x  (-7) done = List.reverse done
      go 41 y     done = go (0) (y-1) ('\n':done)
      go x  y     done = 
        if   (x, y) `Hash.member` h
        then go (x + 1) y (if (h Hash.! (x, y) == 0) then ' ':done else '#':done)
        else go (x + 1) y (' ':done)

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
-- Breaks after every 2 outputs to receive a new input.
runProgram :: Int -> Int -> Map.IntMap Int -> Int -> ((Int, Map.IntMap Int, Int), [Int])
runProgram inputVal startIdx startMem startOffset = go startIdx startMem [] startOffset
    where
    -- Base case: reverse the output list for the proper ordering
    go idx mem [a, b] offset = ((idx, mem, offset), [b, a])
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
        99 -> ((idx, mem, offset), [99, 99])

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

turnLeft = Hash.fromList [("N", "W"), ("E", "N"), ("S", "E"), ("W", "S")]
turnRight = Hash.fromList [("N", "E"), ("E", "S"), ("S", "W"), ("W", "N")]

formatHeader = "\n***********************\nADVENT OF CODE - DAY 11\n" ++
    "***********************\n\nPart 1:  "

padZeroes :: String -> String
padZeroes n = replicate (5 - length n) '0' ++ n
