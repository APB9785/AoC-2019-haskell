import Data.Function
import qualified Data.List as List
import qualified Data.IntMap.Strict as Map
import qualified Data.HashMap.Strict as Hash

data IntComp = IntComp { mem :: Map.IntMap Int
                       , idx :: Int
                       , offset :: Int
                       } deriving (Show)

data MazeState = MazeState { comp :: IntComp
                           , grid :: Hash.HashMap (Int, Int) Int
                           , todo :: [Int]
                           , pos  :: (Int, Int)
                           } deriving (Show)

main = do
    contents <- readFile "day15input.txt"

    putStr formatHeader

    let initMem = makeIndex $ splitCSV contents

    let initComp = IntComp {mem    = initMem,
                            idx    = 0,
                            offset = 0}

    let initState = MazeState {comp = initComp,
                               grid = Hash.singleton (0, 0) 0,
                               todo = [1, 2, 3, 4],
                               pos  = (0, 0)}

    let fullGrid = grid $ runProgram initState

    print $ originToOxygen fullGrid

    putStr "\nPart 2:  "

    print $ oxygenToFurthestSpace fullGrid

    putStr "\n"


-- Looping case function checks the opcodes and calls the appropriate functions.
-- Inputs are given automatically via queue which tries every possible value.
-- Outputs are used to construct a grid map of the explored maze.
runProgram :: MazeState -> MazeState
runProgram s = go s
    where
      go state =

        let compState = comp state

            (opcode, mode1, mode2, mode3) =
              parseInstruction ((mem compState) Map.! (idx compState))

        in case opcode of

          -- Addition: read and write to the memory
          1  -> let newMem =
                      runAdd (idx compState) (mem compState) mode1 mode2 mode3
                        (offset compState)

                    newComp =
                      IntComp {mem    = newMem,
                               idx    = (idx compState) + 4,
                               offset = offset compState}

                in  go MazeState {comp = newComp,
                                  grid = grid state,
                                  todo = todo state,
                                  pos  = pos state}

          -- Multiplication: read and write to the memory
          2  -> let newMem =
                      runMult (idx compState) (mem compState) mode1 mode2 mode3
                        (offset compState)

                    newComp =
                      IntComp {mem    = newMem,
                               idx    = (idx compState) + 4,
                               offset = offset compState}

                in  go MazeState {comp = newComp,
                                  grid = grid state,
                                  todo = todo state,
                                  pos  = pos state}

          -- Input: Give the first value in todo queue and write to memory.
          -- An empty queue will be the halt condition for our program.
          3  -> if   todo state == []
                then state
                else let inputVal = head (todo state)

                         newMem =
                           runInput (idx compState) (mem compState) mode1
                             inputVal (offset compState)

                         newComp =
                           IntComp {mem    = newMem,
                                    idx    = (idx compState) + 2,
                                    offset = offset compState}

                     in  go MazeState {comp = newComp,
                                       grid = grid state,
                                       todo = todo state,
                                       pos  = pos state}

          -- Output: read the memory and update the state.
          4  -> let outVal =
                      runOut (idx compState) (mem compState) mode1
                        (offset compState)

                    newGrid =
                      record outVal (pos state) (head (todo state)) (grid state)

                    newPos =
                      if   outVal == 0
                      then pos state
                      else move (head (todo state)) (pos state)

                    newTodo =
                      if   (outVal == 0) || (newPos `Hash.member` (grid state))
                      then tail (todo state)
                      else (commandsFrom (head (todo state))) ++
                             (tail (todo state))

                    newComp =
                      IntComp {mem    = mem compState,
                               idx    = (idx compState) + 2,
                               offset = offset compState}

                in  go MazeState {comp = newComp,
                                  grid = newGrid,
                                  todo = newTodo,
                                  pos  = newPos}

          -- Jump-if-true: read the memory and adjust the pointer
          5  -> let newIdx =
                      runJumpIf True (idx compState) (mem compState) mode1
                        mode2 (offset compState)

                    newComp =
                      IntComp {mem    = mem compState,
                               idx    = newIdx,
                               offset = offset compState}

                in  go MazeState {comp = newComp,
                                  grid = grid state,
                                  todo = todo state,
                                  pos  = pos state}

          -- Jump-if-false: read the memory and adjust the pointer
          6  -> let newIdx =
                      runJumpIf False (idx compState) (mem compState) mode1
                        mode2 (offset compState)

                    newComp =
                      IntComp {mem    = mem compState,
                               idx    = newIdx,
                               offset = offset compState}

                in  go MazeState {comp = newComp,
                                  grid = grid state,
                                  todo = todo state,
                                  pos  = pos state}

          -- Less than: read and write to the memory
          7  -> let newMem =
                      runLessThan (idx compState) (mem compState) mode1 mode2
                        mode3 (offset compState)

                    newComp =
                      IntComp {mem    = newMem,
                               idx    = (idx compState) + 4,
                               offset = offset compState}

                in  go MazeState {comp = newComp,
                                  grid = grid state,
                                  todo = todo state,
                                  pos  = pos state}

          -- Equals: read and write to the memory
          8  -> let newMem =
                      runEquals (idx compState) (mem compState) mode1 mode2
                        mode3 (offset compState)

                    newComp =
                      IntComp {mem    = newMem,
                               idx    = (idx compState) + 4,
                               offset = offset compState}

                in  go MazeState {comp = newComp,
                                  grid = grid state,
                                  todo = todo state,
                                  pos  = pos state}

          -- Relative base offset: adjust the offset by a given value
          9  -> let newOff =
                      runAdjustOffset (idx compState) (mem compState)
                        mode1 (offset compState)

                    newComp =
                      IntComp {mem    = mem compState,
                               idx    = (idx compState) + 2,
                               offset = newOff}

                in  go MazeState {comp = newComp,
                                  grid = grid state,
                                  todo = todo state,
                                  pos  = pos state}

          -- Halt: ends the program, returning the entire state
          99 -> state


-- Uses Dijkstra's algorith to calculate the minimum distance between the
-- origin and the Oxygen generator.
originToOxygen :: Hash.HashMap (Int, Int) Int -> Int
originToOxygen m =
    let nodes =
          Hash.filter (\v -> (v == 1) || (v == 2)) m

        destination =
          Hash.filter (\v -> v == 2) nodes
            & Hash.keys
            & head

        distances =
          List.foldl'
            (\acc x ->
              if   x == (0, 0)
              then Hash.insert x 0   acc
              else Hash.insert x 999 acc)
            Hash.empty
            (Hash.keys nodes)

    in  go (0, 0) destination distances

    where
      go (x, y) dest todoNodes =
        if   (x, y) == dest
        then todoNodes Hash.! dest
        else let neighbors =
                   List.filter
                     (\n -> n `Hash.member` todoNodes)
                     [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

                 newNodes =
                   Hash.delete
                     (x, y)
                     (List.foldl'
                       (\acc n ->
                         Hash.insert n ((todoNodes Hash.! (x, y)) + 1) acc)
                       todoNodes
                       neighbors)

             in  go (closestNode newNodes) dest newNodes


-- Modified version of our first Dijkstra algorithm, this time we start from
-- the Oxygen generator and travel to every possible location
oxygenToFurthestSpace :: Hash.HashMap (Int, Int) Int -> Int
oxygenToFurthestSpace m =
    let nodes =
          Hash.filter (\v -> (v == 1) || (v == 2)) m

        origin =
          Hash.filter (\v -> v == 2) nodes
            & Hash.keys
            & head

        distances =
          List.foldl'
            (\acc x ->
              if   x == origin
              then Hash.insert x 0   acc
              else Hash.insert x 999 acc)
            Hash.empty
            (Hash.keys nodes)

    in  go origin distances 0

    where
      go (x, y) todoNodes longest =
        if   null todoNodes
        then longest
        else let neighbors =
                   List.filter
                     (\n -> n `Hash.member` todoNodes)
                     [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

                 currentLength = todoNodes Hash.! (x, y)

                 newLongest =
                   if   currentLength > longest
                   then currentLength
                   else longest

                 newNodes =
                   Hash.delete
                     (x, y)
                     (List.foldl'
                       (\acc n -> Hash.insert n (currentLength + 1) acc)
                       todoNodes
                       neighbors)

             in  go (closestNode newNodes) newNodes newLongest


-- Simple "minimumBy value" for our map of nodes - O(n)
closestNode :: Hash.HashMap (Int, Int) Int -> (Int, Int)
closestNode m =
    go (Hash.toList m) (-1, -1) 1000
    where
      go []          best _    = best
      go ((k, v):xs) best dist =
        if   v < dist
        then go xs k    v
        else go xs best dist

-- Add to our grid map based on IntComp output.
record ::
  Int
  -> (Int, Int)
  -> Int
  -> Hash.HashMap (Int, Int) Int
  -> Hash.HashMap (Int, Int) Int
record outVal (x, y) direction gridMap
    | direction == 1  =    -- North
        Hash.insert (x, (y + 1)) outVal gridMap
    | direction == 2  =    -- South
        Hash.insert (x, (y - 1)) outVal gridMap
    | direction == 3  =    -- West
        Hash.insert ((x - 1), y) outVal gridMap
    | direction == 4  =    -- East
        Hash.insert ((x + 1), y) outVal gridMap

move :: Int -> (Int, Int) -> (Int, Int)
move direction (x, y)
    | direction == 1  =    -- North
        (x, (y + 1))
    | direction == 2  =    -- South
        (x, (y - 1))
    | direction == 3  =    -- West
        ((x - 1), y)
    | direction == 4  =    -- East
        ((x + 1), y)

-- Ensures that the path doesn't go backwards until after trying all other
-- possibilities to move forward.
commandsFrom :: Int -> [Int]
commandsFrom direction =
    case direction of
      1 -> [1, 3, 4, 2]
      2 -> [2, 3, 4, 1]
      3 -> [3, 1, 2, 4]
      4 -> [4, 1, 2, 3]

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

formatHeader = "\n***********************\nADVENT OF CODE - DAY 15\n" ++
    "***********************\n\nPart 1:  "

padZeroes :: String -> String
padZeroes n = replicate (5 - length n) '0' ++ n
