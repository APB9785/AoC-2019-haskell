import Data.Function
import qualified Data.List as List
import qualified Data.IntMap.Strict as Map
import qualified Data.HashMap.Strict as Hash
import qualified Data.HashSet as Set

data IntComp = IntComp { mem :: Map.IntMap Int
                       , idx :: Int
                       , offset :: Int
                       , input  :: [Int]
                       , output :: [Int]
                       } deriving (Show)

main = do
    contents <- readFile "day17input.txt"

    putStr formatHeader

    let initMem = makeIndex $ map (\x -> read x :: Int) $ split ',' contents

    let initComp1 = IntComp {mem    = initMem,
                             idx    = 0,
                             offset = 0,
                             input  = [],
                             output = []}

    -- Creates the view of the scaffold and robot which will be used in
    -- Part 1 for calibration, then in Part 2 for generating the path.
    let (initScaf, initPos) =
          runProgram initComp1
            & output
            & reverse
            & map (\x -> toEnum x :: Char)
            & mapScaffold

    print $ calibrate initScaf

    putStr "\nPart 2:  "

    -- Traverse the scaffolding and remember the path, compressing all forward
    -- movements into numbers of consecutive steps.
    let path = compressFs $ tracePath initScaf initPos

    -- Find all useful patterns in the path, give each a letter name.
    let dict = List.zip (patterns path) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 & Hash.fromList

    -- Compress the path further by replacing patterns with their letter name.
    let path' = patReplace dict path

    -- After successful compression, discard all unused patterns from the dict.
    let commands = Hash.filter (\v -> v `elem` path') dict

    -- Use the list of patterns and path to create a list of
    -- Integer inputs to feed the IntCode comp (ASCII encoded).
    let initInput = prepInput commands path'


    let initComp2  = IntComp {mem    = Map.insert 0 2 initMem,
                              idx    = 0,
                              offset = 0,
                              input  = initInput,
                              output = []}

    -- Running the IntComp with new settings will move the robot along the
    -- path and eventually output the final "score" for Part 2.
    print $ head $ output $ runProgram initComp2

    putStr "\n"


-- Replace consecutive forward steps with numbers (of steps taken).
compressFs :: [Char] -> [String]
compressFs p =
    go p 0 []
    where
      go []     fCount done = done
      go (x:xs) fCount done
        | x == 'F' =
            go xs (fCount + 1) done
        | otherwise =
            go xs 0 ((x:(show fCount)):done)


-- Scan the list for all subsequences (of 5 or less strings), and store
-- them in a HashMap with their quantities.  Filter out unhelpful patterns
-- and convert into a list.
patterns :: [String] -> [[String]]
patterns x =
    go x Hash.empty
    where
      go [] dict = Hash.keys $ Hash.filterWithKey goodPatterns dict
      go (c:cs) dict =
        let patterns =
              List.scanl' (\acc x -> acc ++ [x]) [c] (take 4 cs)
            f new old =
              old + new
            dict' =
              List.foldl' (\acc x -> Hash.insertWith f x 1 acc) dict patterns
        in  go cs dict'


-- If a pattern appears less than 3 times, or contains less than three strings,
-- it will not be helpful for our compression task.
goodPatterns :: [String] -> Int -> Bool
goodPatterns k v = ((length k) > 2) && (v > 2)


-- Replace the entire list with pattern names by looking up chunks
-- of 5, 4, and 3 until we find a match.  Note the use of "lose" with
-- smaller chunks to avoid similar patterns from conflicting (see below).
patReplace :: Hash.HashMap [String] Char -> [String] -> [Char]
patReplace h [] = []
patReplace h (a:b:c:d:e:rest) =
    if   [a,b,c,d,e] `Hash.member` h
    then (h Hash.! [a,b,c,d,e]):(patReplace h rest)
    else if   [a,b,c,d] `Hash.member` h
         then (h Hash.! [a,b,c,d]):(patReplace (lose [a,b,c,d] h) (e:rest))
         else (h Hash.! [a,b,c]):(patReplace (lose [a,b,c] h) (d:e:rest))


-- Once a pattern is used, we delete other patterns which contain the used
-- pattern as a subsequence.  This will prevent our simple compression
-- algorithm from getting "creative" and using more than three patterns.
lose :: [String] -> Hash.HashMap [String] Char -> Hash.HashMap [String] Char
lose s h
    | length s == 3 =
        Hash.filterWithKey (\k v -> (init k /= s) && (init (init k) /= s)) h
    | length s == 4 =
        Hash.filterWithKey (\k v -> init k /= s) h


-- Converts the three pattern names to A, B, and C. Then joins the path
-- and patterns with commas + newlines, encoded as ASCII for IntCode input.
prepInput :: Hash.HashMap [String] Char -> String -> [Int]
prepInput dict path =
    let conversions = Hash.fromList $ List.zip (List.nub path) "ABC"

        dict' = dict & Hash.map (\v -> conversions Hash.! v)
                     & Hash.toList
                     & map (\(a, b) -> (b, a))
                     & Hash.fromList

        route = toASCII $ parseRoute $ map (\x -> conversions Hash.! x) path
        funcA = toASCII $ parseFunc $ dict' Hash.! 'A'
        funcB = toASCII $ parseFunc $ dict' Hash.! 'B'
        funcC = toASCII $ parseFunc $ dict' Hash.! 'C'

    in  route ++ funcA ++ funcB ++ funcC ++ [(fromEnum 'n')] ++ [10]

    where
      toASCII s = (map fromEnum s) ++ [10]

      parseRoute [r]    = [r]
      parseRoute (r:rs) = r:',':(parseRoute rs)

      parseFunc [c]    =
        let ([i], j) = List.splitAt 1 c
        in  i:',':j
      parseFunc (c:cs) =
        let ([i], j) = List.splitAt 1 c
        in  i:',':j <> "," <> parseFunc(cs)


-- Simple path traversal. End result is the path reversed.
-- Next, compressFs will fix the order.
tracePath :: Set.HashSet (Int, Int) -> (Int, Int) -> [Char]
tracePath s pos =
    go s pos 'N' []
    where
      go scaffold (x, y) direction done =
        case (x, y) of
          p | (move direction p) `Set.member` scaffold ->
            go scaffold (move direction p) direction ('F':done)
          p | (move (turnLeft direction) p) `Set.member` scaffold ->
            go scaffold p (turnLeft direction) ('L':done)
          p | (move (turnRight direction) p) `Set.member` scaffold ->
            go scaffold p (turnRight direction) ('R':done)
          p ->
            done


-- Final task for Part 1.
calibrate :: Set.HashSet (Int, Int) -> Int
calibrate s =
    calculateAlignment $ findIntersections s


-- For each scaffold, check if there is more scaffolding in all four directions.
findIntersections :: Set.HashSet (Int, Int) -> Set.HashSet (Int, Int)
findIntersections s =
    Set.filter (\(x, y) -> (((x + 1), y) `Set.member` s) &&
                           (((x - 1), y) `Set.member` s) &&
                           ((x, (y + 1)) `Set.member` s) &&
                           ((x, (y - 1)) `Set.member` s)
               ) s


-- Score function for Part 1.
calculateAlignment :: Set.HashSet (Int, Int) -> Int
calculateAlignment s =
    Set.foldl' (\acc (x, y) -> acc + (x * y)) 0 s


-- Parse the full "camera" view into a set of coordinates which contain
-- scaffolding. Output as a tuple also containing the "robot" position.
mapScaffold :: [Char] -> (Set.HashSet (Int, Int), (Int, Int))
mapScaffold outList =
    go outList 0 0 Set.empty (-1, -1)
    where
      go []   x y s p = (s, p)
      go (c:cs) x y s p
        | c == '\n' =
            go cs 0 (y + 1) s p
        | c == '.'  =
            go cs (x + 1) y s p
        | c == '^'  =
            go cs (x + 1) y (Set.insert (x, y) s) (x, y)
        | c == '#' =
            go cs (x + 1) y (Set.insert (x, y) s) p


-- Below are simple helpers and IntCode functions.

move :: Char -> (Int, Int) -> (Int, Int)
move direction (x, y)
    | direction == 'N' =
        (x, (y - 1))
    | direction == 'S' =
        (x, (y + 1))
    | direction == 'W' =
        ((x - 1), y)
    | direction == 'E' =
        ((x + 1), y)

turnLeft :: Char -> Char
turnLeft 'N' = 'W'
turnLeft 'E' = 'N'
turnLeft 'S' = 'E'
turnLeft 'W' = 'S'

turnRight :: Char -> Char
turnRight 'N' = 'E'
turnRight 'E' = 'S'
turnRight 'S' = 'W'
turnRight 'W' = 'N'

-- Looping case function checks the opcodes and calls the appropriate functions.
runProgram :: IntComp -> IntComp
runProgram s = go s
    where
      go state =

        let (opcode, mode1, mode2, mode3) =
              parseInstruction ((mem state) Map.! (idx state))

        in case opcode of

          -- Addition: read and write to the memory
          1  -> let newMem =
                      runAdd (idx state) (mem state) mode1 mode2 mode3
                        (offset state)

                in  go IntComp {mem    = newMem,
                                idx    = (idx state) + 4,
                                offset = offset state,
                                input  = input state,
                                output = output state}

          -- Multiplication: read and write to the memory
          2  -> let newMem =
                      runMult (idx state) (mem state) mode1 mode2 mode3
                        (offset state)

                in  go IntComp {mem    = newMem,
                                idx    = (idx state) + 4,
                                offset = offset state,
                                input  = input state,
                                output = output state}

          -- Input: Give the first value in input list and write to memory.
          -- Remove the inputted value afterwards.
          3  -> let newMem =
                      runInput (idx state) (mem state) mode1
                        (head $ input state) (offset state)

                in  go IntComp {mem    = newMem,
                                idx    = (idx state) + 2,
                                offset = offset state,
                                input  = tail $ input state,
                                output = output state}

          -- Output: read the memory and update the state.
          4  -> let outVal =
                      runOut (idx state) (mem state) mode1
                        (offset state)

                    newOutput = outVal:(output state)

                in  go IntComp {mem    = mem state,
                                idx    = (idx state) + 2,
                                offset = offset state,
                                input  = input state,
                                output = newOutput}

          -- Jump-if-true: read the memory and adjust the pointer
          5  -> let newIdx =
                      runJumpIf True (idx state) (mem state) mode1
                        mode2 (offset state)

                in  go IntComp {mem    = mem state,
                                idx    = newIdx,
                                offset = offset state,
                                input  = input state,
                                output = output state}

          -- Jump-if-false: read the memory and adjust the pointer
          6  -> let newIdx =
                      runJumpIf False (idx state) (mem state) mode1
                        mode2 (offset state)

                in  go IntComp {mem    = mem state,
                                idx    = newIdx,
                                offset = offset state,
                                input  = input state,
                                output = output state}

          -- Less than: read and write to the memory
          7  -> let newMem =
                      runLessThan (idx state) (mem state) mode1 mode2
                        mode3 (offset state)

                in  go IntComp {mem    = newMem,
                                idx    = (idx state) + 4,
                                offset = offset state,
                                input  = input state,
                                output = output state}

          -- Equals: read and write to the memory
          8  -> let newMem =
                      runEquals (idx state) (mem state) mode1 mode2
                        mode3 (offset state)

                in  go IntComp {mem    = newMem,
                                idx    = (idx state) + 4,
                                offset = offset state,
                                input  = input state,
                                output = output state}

          -- Relative base offset: adjust the offset by a given value
          9  -> let newOff =
                      runAdjustOffset (idx state) (mem state)
                        mode1 (offset state)

                in  go IntComp {mem    = mem state,
                                idx    = (idx state) + 2,
                                offset = newOff,
                                input  = input state,
                                output = output state}

          -- Halt: ends the program, returning the entire state
          99 -> state

-- Splits on comma delimiter, converts all values to Int.
split :: Char -> [Char] -> [String]
split delimiter [] = []
split delimiter x  =
    let (curr, rest) = break (==delimiter) x in
    case rest of
      [] -> curr:(split delimiter rest)
      _  -> curr:(split delimiter (tail rest))

-- Folds a list into map values, using list index (position) as keys.
-- Now includes an additional 2000 empty registers beyond the input list.
makeIndex :: [Int] -> Map.IntMap Int
makeIndex ns = foldl indexHelper Map.empty (ns ++ (take 2000 (repeat 0)))
    where
      indexHelper acc val = Map.insert (Map.size acc) val acc

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

formatHeader = "\n***********************\nADVENT OF CODE - DAY 17\n" ++
    "***********************\n\nPart 1:  "

padZeroes :: String -> String
padZeroes n = replicate (5 - length n) '0' ++ n
