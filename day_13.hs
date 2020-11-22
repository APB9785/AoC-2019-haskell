import Data.Function
import qualified Data.List as List
import qualified Data.IntMap.Strict as Map
import qualified Data.HashMap.Strict as Hash

data GameState = GameState { idx :: Int
                           , mem :: Map.IntMap Int
                           , out :: [Int]
                           , offset :: Int
                           , tiles :: Hash.HashMap (Int, Int) Int
                           , score :: Int
                           , ball :: (Int, Int)
                           , paddle :: (Int, Int)
                           } deriving (Show)

main = do
    contents <- readFile "day13input.txt"

    putStr formatHeader

    let initialState = makeIndex $ splitCSV contents

    -- By default the program generates a GameState and immediately halts.
    runProgram initialState
      & countBlocks
      & print

    putStr "\nPart 2:  "

    -- Replacement value given in puzzle input causes the program to begin
    -- accepting our AI-generated input to "play" the game.
    Map.insert 0 2 initialState
      & runProgram
      & score
      & print

    putStr "\n"


-- Looping case function checks the opcodes and calls the appropriate functions.
-- This time we have to include an AI to watch output values and generate
-- appropriate input values to keep the game running.
runProgram :: Map.IntMap Int -> GameState
runProgram m =
    go GameState {idx = 0,
                  mem = m,
                  out = [],
                  offset = 0,
                  score = 0,
                  tiles = Hash.empty,
                  paddle = (-1, -1),
                  ball = (-1, -1)}
    where
      go state =
        let (opcode, mode1, mode2, mode3) =
              parseInstruction ((mem state) Map.! (idx state))
        in case opcode of

          -- Addition: read and write to the memory
          1  -> let newMem = runAdd (idx state) (mem state) mode1 mode2
                                    mode3 (offset state)
                in go GameState {idx    = (idx state) + 4,
                                 mem    = newMem,
                                 out    = out state,
                                 offset = offset state,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Multiplication: read and write to the memory
          2  -> let newMem = runMult (idx state) (mem state) mode1 mode2
                                     mode3 (offset state)
                in go GameState {idx    = (idx state) + 4,
                                 mem    = newMem,
                                 out    = out state,
                                 offset = offset state,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Input: Generate an input based on ball and paddle state
          3  -> let (ballX, ballY)     = ball state
                    (paddleX, paddleY) = paddle state
                    inputVal = if   paddleX < ballX
                               then 1
                               else if   paddleX > ballX
                                    then -1
                                    else 0
                    newMem = runInput (idx state) (mem state) mode1
                                      inputVal (offset state)
                in go GameState {idx    = (idx state) + 2,
                                 mem    = newMem,
                                 out    = out state,
                                 offset = offset state,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Output: read the memory and hold a value for later output.
          -- When three values are held, use them to update the state.
          4  -> if   length (out state) == 2
                then let [x, y, i] = (out state) ++
                           [runOut (idx state) (mem state) mode1 (offset state)]
                         newScore  = if x == -1 then i      else score state
                         newPaddle = if i == 3  then (x, y) else paddle state
                         newBall   = if i == 4  then (x, y) else ball state
                         newTiles  = if   (i /= 3) && (i /= 4) && (x /= -1)
                                     then Hash.insert (x, y) i (tiles state)
                                     else tiles state
                     in go GameState {idx    = (idx state) + 2,
                                      mem    = mem state,
                                      out    = [],
                                      offset = offset state,
                                      tiles  = newTiles,
                                      score  = newScore,
                                      paddle = newPaddle,
                                      ball   = newBall}
                else let newOut = (out state) ++
                           [runOut (idx state) (mem state) mode1 (offset state)]
                     in go GameState {idx    = (idx state) + 2,
                                      mem    = mem state,
                                      out    = newOut,
                                      offset = offset state,
                                      tiles  = tiles state,
                                      score  = score state,
                                      paddle = paddle state,
                                      ball   = ball state}

          -- Jump-if-true: read the memory and adjust the pointer
          5  -> let newIdx = runJumpIf True (idx state) (mem state)
                                       mode1 mode2 (offset state)
                in go GameState {idx    = newIdx,
                                 mem    = mem state,
                                 out    = out state,
                                 offset = offset state,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Jump-if-false: read the memory and adjust the pointer
          6  -> let newIdx = runJumpIf False (idx state) (mem state)
                                       mode1 mode2 (offset state)
                in go GameState {idx    = newIdx,
                                 mem    = mem state,
                                 out    = out state,
                                 offset = offset state,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Less than: read and write to the memory
          7  -> let newMem = runLessThan (idx state) (mem state) mode1
                                         mode2 mode3 (offset state)
                in go GameState {idx    = (idx state) + 4,
                                 mem    = newMem,
                                 out    = out state,
                                 offset = offset state,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Equals: read and write to the memory
          8  -> let newMem = runEquals (idx state) (mem state) mode1
                                       mode2 mode3 (offset state)
                in go GameState {idx    = (idx state) + 4,
                                 mem    = newMem,
                                 out    = out state,
                                 offset = offset state,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Relative base offset: adjust the offset by a given value
          9  -> let newOff = runAdjustOffset (idx state) (mem state)
                                             mode1 (offset state)
                in go GameState {idx    = (idx state) + 2,
                                 mem    = mem state,
                                 out    = out state,
                                 offset = newOff,
                                 tiles  = tiles state,
                                 score  = score state,
                                 paddle = paddle state,
                                 ball   = ball state}

          -- Halt: ends the program, returning the entire state
          99 -> state

-- Scans tiles for id '2' (blocks)
countBlocks :: GameState -> Int
countBlocks state =
    tiles state
      & Hash.elems
      & List.filter (==2)
      & length

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

formatHeader = "\n***********************\nADVENT OF CODE - DAY 13\n" ++
    "***********************\n\nPart 1:  "

padZeroes :: String -> String
padZeroes n = replicate (5 - length n) '0' ++ n
